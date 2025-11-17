pragma Ada_2022;
--  ===========================================================================
--  Tzif.Domain.Parser
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    TZif binary format parser implementation.
--
--  Supported Versions:
--    - TZif version 1 (legacy)
--    - TZif version 2 (64-bit)
--    - TZif version 3 (with extensions)
--
--  ===========================================================================

with TZif.Domain.Error;
with TZif.Domain.Value_Object;
with TZif.Domain.Value_Object.TZif_Header;

package body TZif.Domain.Parser with
  SPARK_Mode => On
is

   --  ========================================================================
   --  Constants
   --  ========================================================================

   --  TZif magic number: "TZif" (0x54 0x5A 0x69 0x66)
   TZif_Magic : constant array (1 .. 4) of Unsigned_8 :=
     [16#54#, 16#5A#, 16#69#, 16#66#];

   --  Minimum TZif file size (header only)
   Min_TZif_Size : constant := 44;

   --  ========================================================================
   --  Stub Parser Procedures (temporary until full parser is migrated)
   --  ========================================================================

   procedure Parse_Stub_V1
     (Bytes : Byte_Array; Length : Natural; Result : out Parse_Result_Type)
   is
      pragma Unreferenced (Bytes, Length);
      use TZif.Domain.Value_Object.TZif_Header;
      Stub_Data : TZif_Data_Type;
   begin
      --  Create minimal TZif_Data with empty collections
      --  TODO: Migrate actual parser logic
      Stub_Data.Header :=
        (Version => Version_1, UTC_Local_Count => 0, Standard_Wall_Count => 0,
         Leap_Count   => 0, Transition_Count => 0, Type_Count => 1,
         Abbrev_Chars => 4);

      Stub_Data.Transitions    := Transition_Vectors.Empty_Vector;
      Stub_Data.Timezone_Types := Timezone_Type_Vectors.Empty_Vector;
      Stub_Data.Leap_Seconds   := Leap_Second_Vectors.Empty_Vector;
      Stub_Data.POSIX_TZ       := POSIX_TZ_Strings.Null_Bounded_String;

      Result := Parse_Result.Ok (Stub_Data);
   end Parse_Stub_V1;

   procedure Parse_Stub_V2
     (Bytes : Byte_Array; Length : Natural; Result : out Parse_Result_Type)
   is
      pragma Unreferenced (Bytes, Length);
      use TZif.Domain.Value_Object.TZif_Header;
      Stub_Data : TZif_Data_Type;
   begin
      --  Create minimal TZif_Data with empty collections
      --  TODO: Migrate actual parser logic
      Stub_Data.Header :=
        (Version => Version_2, UTC_Local_Count => 0, Standard_Wall_Count => 0,
         Leap_Count   => 0, Transition_Count => 0, Type_Count => 1,
         Abbrev_Chars => 4);

      Stub_Data.Transitions    := Transition_Vectors.Empty_Vector;
      Stub_Data.Timezone_Types := Timezone_Type_Vectors.Empty_Vector;
      Stub_Data.Leap_Seconds   := Leap_Second_Vectors.Empty_Vector;
      Stub_Data.POSIX_TZ       := POSIX_TZ_Strings.Null_Bounded_String;

      Result := Parse_Result.Ok (Stub_Data);
   end Parse_Stub_V2;

   --  ========================================================================
   --  Parse_From_Bytes
   --  ========================================================================

   procedure Parse_From_Bytes
     (Bytes : Byte_Array; Length : Natural; Result : out Parse_Result_Type)
   is
      Version : Unsigned_8;
   begin
      --  Step 1: Validate minimum size
      if Length < Min_TZif_Size then
         Result :=
           Parse_Result.Error
             (TZif.Domain.Error.Validation_Error,
              "TZif file too small (min 44 bytes)");
         return;
      end if;

      --  Step 2: Validate magic number
      for I in TZif_Magic'Range loop
         if Bytes (I) /= TZif_Magic (I) then
            Result :=
              Parse_Result.Error
                (TZif.Domain.Error.Validation_Error, "Invalid TZif magic");
            return;
         end if;
      end loop;

      --  Step 3: Read version byte (offset 4, zero-indexed = position 5)
      Version := Bytes (5);

      --  Step 4: Dispatch to version-specific parser
      --  Version byte: 0x00 (V1), 0x32 ('2' for V2), 0x33 ('3' for V3)

      --  TODO: Implement version-specific parsing
      --  For now, return stub error indicating which version was detected

      case Version is
         when 16#00# =>
            --  TZif version 1 (legacy 32-bit)
            --  TODO: Call V1.Parse_V1 (Bytes, Length, Result);
            --  For now, return a minimal stub TZif_Data
            Parse_Stub_V1 (Bytes, Length, Result);

         when 16#32# =>
            --  TZif version 2 (64-bit, '2')
            --  TODO: Call V2.Parse_V2 (Bytes, Length, Result);
            --  For now, return a minimal stub TZif_Data
            Parse_Stub_V2 (Bytes, Length, Result);

         when 16#33# =>
            --  TZif version 3 (with extensions, '3')
            --  TODO: Call V3.Parse_V3 (Bytes, Length, Result);
            --  For now, use V2 stub (V3 is backward compatible)
            Parse_Stub_V2 (Bytes, Length, Result);

         when others =>
            --  Unknown version
            Result :=
              Parse_Result.Error
                (TZif.Domain.Error.Validation_Error, "Unknown TZif version");
      end case;

   end Parse_From_Bytes;

end TZif.Domain.Parser;
