pragma Ada_2022;
--  ===========================================================================
--  Tzif.Infrastructure.Ulid
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Ulid implementation.
--
--  ===========================================================================

with Ada.Calendar;
with Ada.Numerics.Discrete_Random;
with Interfaces;

package body TZif.Infrastructure.ULID is

   use Ada.Calendar;
   use Interfaces;

   --  ========================================================================
   --  Crockford's Base32 Alphabet (excludes I, L, O, U)
   --  ========================================================================

   Base32_Alphabet : constant String := "0123456789ABCDEFGHJKMNPQRSTVWXYZ";

   --  ========================================================================
   --  Random Number Generator
   --  ========================================================================

   subtype Random_Byte is Unsigned_8;
   package Random_Byte_Gen is new Ada.Numerics.Discrete_Random (Random_Byte);
   Generator : Random_Byte_Gen.Generator;

   --  Initialize random generator
   procedure Initialize_Generator is
      use Random_Byte_Gen;
   begin
      Reset (Generator);
   end Initialize_Generator;

   --  ========================================================================
   --  Encode_Base32
   --  ========================================================================
   --  Encode a 64-bit value into Base32 string of specified length
   --  ========================================================================

   function Encode_Base32
     (Value : Unsigned_64; Length : Positive) return String
   is
      Result : String (1 .. Length) := (others => '0');
      Temp   : Unsigned_64          := Value;
      Mask   : constant Unsigned_64 := 16#1F#;  --  5 bits
   begin
      for I in reverse Result'Range loop
         Result (I) := Base32_Alphabet (Integer (Temp and Mask) + 1);
         Temp       := Shift_Right (Temp, 5);
      end loop;
      return Result;
   end Encode_Base32;

   --  ========================================================================
   --  Get_Timestamp_Milliseconds
   --  ========================================================================
   --  Get current time as milliseconds since Unix epoch
   --  ========================================================================

   function Get_Timestamp_Milliseconds return Unsigned_64 is
      Unix_Epoch           : constant Time        :=
        Time_Of (Year => 1_970, Month => 1, Day => 1);
      Now                  : constant Time        := Clock;
      Duration_Since_Epoch : constant Duration    := Now - Unix_Epoch;
      Milliseconds         : constant Unsigned_64 :=
        Unsigned_64 (Duration_Since_Epoch * 1_000.0);
   begin
      return Milliseconds;
   end Get_Timestamp_Milliseconds;

   --  ========================================================================
   --  Generate
   --  ========================================================================

   function Generate return ULID_Type is
      Timestamp    : constant Unsigned_64 := Get_Timestamp_Milliseconds;
      ULID_Str     : String (1 .. 26);
      Random_Value : Unsigned_64;
   begin
      --  Encode timestamp (48 bits) into 10 characters
      ULID_Str (1 .. 10) := Encode_Base32 (Timestamp, 10);

      --  Generate randomness (80 bits = 16 characters)
      --  We'll do this in two 40-bit chunks for simplicity
      Random_Value := 0;
      for I in 1 .. 5 loop
         Random_Value :=
           Shift_Left (Random_Value, 8) or
           Unsigned_64 (Random_Byte_Gen.Random (Generator));
      end loop;
      ULID_Str (11 .. 18) := Encode_Base32 (Random_Value, 8);

      Random_Value := 0;
      for I in 1 .. 5 loop
         Random_Value :=
           Shift_Left (Random_Value, 8) or
           Unsigned_64 (Random_Byte_Gen.Random (Generator));
      end loop;
      ULID_Str (19 .. 26) := Encode_Base32 (Random_Value, 8);

      return Make_ULID (ULID_Str);
   end Generate;

   --  ========================================================================
   --  Generate_From_Seed
   --  ========================================================================
   --  For testing: generate deterministic ULID from seed string
   --  ========================================================================

   function Generate_From_Seed (Seed : String) return ULID_Type is
      Result : String (1 .. 26) := (others => '0');
      Hash   : Unsigned_64      := 5_381;  --  DJB2 hash initial value
   begin
      --  Simple hash of seed
      for C of Seed loop
         Hash := ((Hash * 33) + Character'Pos (C)) and 16#FFFF_FFFF_FFFF_FFFF#;
      end loop;

      --  Encode hash into ULID format
      Result (1 .. 13)  := Encode_Base32 (Hash, 13);
      Result (14 .. 26) := Encode_Base32 (Shift_Right (Hash, 32), 13);

      return Make_ULID (Result);
   end Generate_From_Seed;

begin
   --  Initialize random generator when package loads
   Initialize_Generator;
end TZif.Infrastructure.ULID;
