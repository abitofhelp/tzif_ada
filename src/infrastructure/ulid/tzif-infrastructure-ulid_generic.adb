pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.ULID_Generic
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Generic ULID generator implementation with pluggable RNG.
--
--  ===========================================================================

with Ada.Calendar;
with Ada.Calendar.Arithmetic;
with Functional.Option;
with Functional.Try;

package body TZif.Infrastructure.ULID_Generic
  with SPARK_Mode => Off  --  Uses RNG (non-deterministic, non-SPARK)
is

   use Ada.Calendar;
   use Ada.Calendar.Arithmetic;

   --  ========================================================================
   --  Constants
   --  ========================================================================

   --  Crockford's Base32 alphabet (excludes I, L, O, U to avoid ambiguity)
   Base32_Alphabet : constant String := "0123456789ABCDEFGHJKMNPQRSTVWXYZ";

   --  ========================================================================
   --  Helper Functions
   --  ========================================================================

   --  Get current time as milliseconds since Unix epoch
   --  Uses Calendar.Arithmetic.Difference to avoid Y2038/Duration overflow
   --  Uses Functional.Try for exception-safe fallback to 0

   package Unsigned_64_Option is new Functional.Option (T => Unsigned_64);

   function Raw_Get_Timestamp return Unsigned_64 is
      Unix_Epoch : constant Time :=
        Time_Of (Year => 1_970, Month => 1, Day => 1);
      Now        : constant Time := Clock;
      Days       : Day_Count;
      Seconds    : Duration;
      Leap_Secs  : Leap_Seconds_Count;
      Total_Ms   : Unsigned_64;
   begin
      --  Use Arithmetic.Difference for Y2038-safe calculation
      Difference (Now, Unix_Epoch, Days, Seconds, Leap_Secs);

      --  Convert to milliseconds: (days * 86400 + seconds) * 1000
      Total_Ms := Unsigned_64 (Days) * 86_400_000
                  + Unsigned_64 (Seconds * 1_000.0);

      return Total_Ms;
   end Raw_Get_Timestamp;

   function Try_Get_Timestamp is new Functional.Try.Try_To_Functional_Option
     (T          => Unsigned_64,
      Option_Pkg => Unsigned_64_Option,
      Action     => Raw_Get_Timestamp);

   function Get_Timestamp_Milliseconds return Unsigned_64 is
   begin
      --  Fallback to epoch zero on any time calculation error
      return Unsigned_64_Option.Unwrap_Or (Try_Get_Timestamp, Default => 0);
   end Get_Timestamp_Milliseconds;

   --  Encode a 64-bit value into Base32 string of specified length
   function Encode_Base32 (Value : Unsigned_64; Length : Positive)
     return String
   is
      Result : String (1 .. Length) := [others => '0'];
      Temp   : Unsigned_64          := Value;
      Mask   : constant Unsigned_64 := 16#1F#;  --  5 bits
   begin
      for I in reverse Result'Range loop
         declare
            Index : constant Natural := Natural (Temp and Mask) + 1;
         begin
            --  Invariant: 5-bit mask + 1 always yields 1..32
            pragma Assert (Index in Base32_Alphabet'Range);
            Result (I) := Base32_Alphabet (Index);
         end;
         Temp := Shift_Right (Temp, 5);
      end loop;
      return Result;
   end Encode_Base32;

   --  ========================================================================
   --  Protected Type Implementation
   --  ========================================================================
   --
   --  ARCHITECTURAL EXCEPTION TO RULE 2 (Mandatory Functional.Try)
   --
   --  Protected bodies in Ada cannot use Functional.Try because:
   --  1. Protected operations cannot make potentially blocking calls
   --  2. Functional.Try uses Ada.Finalization which may be blocking
   --  3. This is an Ada language constraint, not a design choice
   --
   --  The manual exception handlers below are the CORRECT pattern for
   --  protected bodies. They provide deterministic fallback behavior:
   --  - RNG reset failure: Mark as initialized, avoid repeated failures
   --  - RNG byte generation failure: Use deterministic counter
   --  - Encoding failure: Use all-zero string (valid ULID)
   --  - Ultimate failure: Return Null_ULID (postcondition violation)
   --
   --  This ensures ULID generation never fails, even under degraded
   --  conditions - the design intent for this fault-tolerant component.
   --  ========================================================================

   protected body ULID_Generator_Type is

      procedure Generate (Result : out ULID_Type) is
         Current_Ms : Unsigned_64;
         ULID_Str   : String (1 .. 26) := [others => '0'];
      begin
         --  Get timestamp (uses Functional.Try internally, returns 0 on error)
         Current_Ms := Get_Timestamp_Milliseconds;

         --  Initialize RNG on first use (see ARCHITECTURAL EXCEPTION above)
         if not Initialized then
            begin
               Reset (RNG);
               Initialized := True;
            exception
               when others =>
                  --  DESIGN DECISION: Protected body cannot use Functional.Try
                  --  (Ada language constraint - potentially blocking calls)
                  Initialized := True;  --  Avoid repeated failures
            end;
         end if;

         --  Handle monotonic increment if same millisecond
         if Current_Ms = Last_Timestamp_Ms then
            --  Increment the 80-bit random component (10 bytes)
            --  Start from least significant byte
            for I in reverse Last_Random_Bytes'Range loop
               if Last_Random_Bytes (I) < 255 then
                  Last_Random_Bytes (I) := Last_Random_Bytes (I) + 1;
                  exit;  --  No carry needed
               else
                  Last_Random_Bytes (I) := 0;  --  Carry to next byte
                  if I = Last_Random_Bytes'First then
                     --  Overflow (extremely rare: 2^80 ULIDs in 1ms)
                     --  Generate fresh random bytes with exception handling
                     begin
                        for J in Last_Random_Bytes'Range loop
                           Last_Random_Bytes (J) := Random_Byte (RNG);
                        end loop;
                     exception
                        when others =>
                           --  DESIGN DECISION: Protected body.
                           --  RNG fail: deterministic fallback.
                           Last_Random_Bytes (Last_Random_Bytes'Last) := 1;
                     end;
                     exit;
                  end if;
               end if;
            end loop;
         else
            --  New millisecond - generate fresh random bytes
            Last_Timestamp_Ms := Current_Ms;
            begin
               for I in Last_Random_Bytes'Range loop
                  Last_Random_Bytes (I) := Random_Byte (RNG);
               end loop;
            exception
               when others =>
                  --  DESIGN DECISION: Protected body - no Functional.Try.
                  --  RNG failure: use deterministic fallback (ensures ULID
                  --  generation succeeds, though not ideal).
                  for I in Last_Random_Bytes'Range loop
                     Last_Random_Bytes (I) := Unsigned_8 (I);
                  end loop;
            end;
         end if;

         --  Build ULID string with exception handling
         begin
            --  First 10 characters: timestamp (48 bits)
            ULID_Str (1 .. 10) := Encode_Base32 (Current_Ms, 10);

            --  Next 16 characters: randomness (80 bits = 10 bytes)
            --  Encode in two chunks for simplicity
            declare
               Random_Value_1 : Unsigned_64 := 0;
               Random_Value_2 : Unsigned_64 := 0;
            begin
               --  First 5 bytes -> 8 base32 characters
               for I in 1 .. 5 loop
                  Random_Value_1 :=
                    Shift_Left (Random_Value_1, 8) or
                    Unsigned_64 (Last_Random_Bytes (I));
               end loop;
               ULID_Str (11 .. 18) := Encode_Base32 (Random_Value_1, 8);

               --  Last 5 bytes -> 8 base32 characters
               for I in 6 .. 10 loop
                  Random_Value_2 :=
                    Shift_Left (Random_Value_2, 8) or
                    Unsigned_64 (Last_Random_Bytes (I));
               end loop;
               ULID_Str (19 .. 26) := Encode_Base32 (Random_Value_2, 8);
            end;
         exception
            when others =>
               --  DESIGN DECISION: Protected body cannot use Functional.Try.
               --  Encoding failed - use all zeros (will become valid ULID)
               ULID_Str := [others => '0'];
         end;

         --  Convert to ULID_Type (Make_ULID has precondition,
         --  so ULID_Str must be valid)
         Result := Make_ULID (ULID_Str);
      exception
         when others =>
            --  DESIGN DECISION: Protected body cannot use Functional.Try.
            --  Ultimate fallback: return null ULID
            --  Note: This violates the postcondition, but protects against
            --  complete failure. In practice, should never reach here.
            Result := Null_ULID;
      end Generate;

      procedure Reset_RNG is
      begin
         Reset (RNG);
         Initialized := True;
         Last_Timestamp_Ms := 0;
         Last_Random_Bytes := [others => 0];
      end Reset_RNG;

   end ULID_Generator_Type;

   --  ========================================================================
   --  Convenience Functions
   --  ========================================================================

   function New_ULID return ULID_Type is
      Result : ULID_Type;
   begin
      Global_Generator.Generate (Result);
      return Result;
   end New_ULID;

   function Generate_From_Seed (Seed : String) return ULID_Type is
      Result : String (1 .. 26) := [others => '0'];
      Hash   : Unsigned_64      := 5_381;  --  DJB2 hash initial value
   begin
      --  Hash the seed string
      for C of Seed loop
         Hash :=
           ((Hash * 33) + Character'Pos (C)) and 16#FFFF_FFFF_FFFF_FFFF#;
      end loop;

      --  Encode hash into ULID format
      Result (1 .. 13)  := Encode_Base32 (Hash, 13);
      Result (14 .. 26) := Encode_Base32 (Shift_Right (Hash, 32), 13);

      return Make_ULID (Result);
   end Generate_From_Seed;

end TZif.Infrastructure.ULID_Generic;
