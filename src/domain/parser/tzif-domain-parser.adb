pragma Ada_2022;
--  ===========================================================================
--  TZif.Domain.Parser
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    TZif binary format parser implementation.
--    Pure parsing logic operating on byte arrays (no I/O).
--
--  Supported Versions:
--    - TZif version 1 (legacy 32-bit)
--    - TZif version 2 (64-bit)
--    - TZif version 3 (with extensions)
--
--  ===========================================================================

with Ada.Unchecked_Conversion;
with TZif.Domain.Error;
with TZif.Domain.Value_Object.TZif_Header;
with TZif.Domain.Value_Object.Transition;
with TZif.Domain.Value_Object.Timezone_Type;
with TZif.Domain.Value_Object.Epoch_Seconds;
with TZif.Domain.Value_Object.UTC_Offset;

package body TZif.Domain.Parser with
  SPARK_Mode => On
is

   use TZif.Domain.Value_Object.TZif_Header;
   use TZif.Domain.Value_Object.Transition;
   use TZif.Domain.Value_Object.Timezone_Type;
   use TZif.Domain.Value_Object.Epoch_Seconds;
   use TZif.Domain.Value_Object.UTC_Offset;

   --  ========================================================================
   --  Unchecked conversions for binary parsing
   --  ========================================================================

   function To_Integer_32 is new Ada.Unchecked_Conversion
     (Source => Unsigned_32, Target => Integer_32);

   function To_Integer_64 is new Ada.Unchecked_Conversion
     (Source => Unsigned_64, Target => Integer_64);

   --  ========================================================================
   --  Constants
   --  ========================================================================

   --  TZif magic number: "TZif" (0x54 0x5A 0x69 0x66)
   TZif_Magic : constant array (1 .. 4) of Unsigned_8 :=
     [16#54#, 16#5A#, 16#69#, 16#66#];

   --  Minimum TZif file size (header only: 44 bytes)
   Min_TZif_Size : constant := 44;

   --  Header size in bytes
   Header_Size : constant := 44;

   --  ========================================================================
   --  Byte-Array Reading Helpers (Pure Functions)
   --  ========================================================================

   --  Read a 32-bit big-endian unsigned integer at position
   function Read_Unsigned_32_At
     (Bytes : Byte_Array; Pos : Positive) return Unsigned_32
   with
     Pre => Pos >= Bytes'First
       and then Pos <= Bytes'Last - 3
       and then Bytes'Last < Positive'Last - 3
   is
   begin
      return Shift_Left (Unsigned_32 (Bytes (Pos)), 24) or
        Shift_Left (Unsigned_32 (Bytes (Pos + 1)), 16) or
        Shift_Left (Unsigned_32 (Bytes (Pos + 2)), 8) or
        Unsigned_32 (Bytes (Pos + 3));
   end Read_Unsigned_32_At;

   --  Read a 32-bit big-endian signed integer at position
   function Read_Int32_At
     (Bytes : Byte_Array; Pos : Positive) return Integer_32
   with
     Pre => Pos >= Bytes'First
       and then Pos <= Bytes'Last - 3
       and then Bytes'Last < Positive'Last - 3
   is
   begin
      return To_Integer_32 (Read_Unsigned_32_At (Bytes, Pos));
   end Read_Int32_At;

   --  Read a 64-bit big-endian unsigned integer at position
   function Read_Unsigned_64_At
     (Bytes : Byte_Array; Pos : Positive) return Unsigned_64
   with
     Pre => Pos >= Bytes'First
       and then Pos <= Bytes'Last - 7
       and then Bytes'Last < Positive'Last - 7
   is
   begin
      return Shift_Left (Unsigned_64 (Bytes (Pos)), 56) or
        Shift_Left (Unsigned_64 (Bytes (Pos + 1)), 48) or
        Shift_Left (Unsigned_64 (Bytes (Pos + 2)), 40) or
        Shift_Left (Unsigned_64 (Bytes (Pos + 3)), 32) or
        Shift_Left (Unsigned_64 (Bytes (Pos + 4)), 24) or
        Shift_Left (Unsigned_64 (Bytes (Pos + 5)), 16) or
        Shift_Left (Unsigned_64 (Bytes (Pos + 6)), 8) or
        Unsigned_64 (Bytes (Pos + 7));
   end Read_Unsigned_64_At;

   --  Read a 64-bit big-endian signed integer at position
   function Read_Int64_At
     (Bytes : Byte_Array; Pos : Positive) return Integer_64
   with
     Pre => Pos >= Bytes'First
       and then Pos <= Bytes'Last - 7
       and then Bytes'Last < Positive'Last - 7
   is
   begin
      return To_Integer_64 (Read_Unsigned_64_At (Bytes, Pos));
   end Read_Int64_At;

   --  ========================================================================
   --  Parse_Header_From_Bytes
   --
   --  Parses TZif header starting at given position.
   --  Header layout (44 bytes):
   --    0-3:   Magic "TZif"
   --    4:     Version (0x00=V1, '2'=V2, '3'=V3)
   --    5-19:  Reserved (15 bytes)
   --    20-23: UTC/Local indicator count
   --    24-27: Standard/Wall indicator count
   --    28-31: Leap second count
   --    32-35: Transition time count
   --    36-39: Local time type count
   --    40-43: Timezone abbreviation chars count
   --  ========================================================================

   procedure Parse_Header_From_Bytes
     (Bytes     :     Byte_Array; Pos : Positive; Length : Natural;
      Header    : out TZif_Header_Type; Data_Start : out Positive;
      Is_Ok     : out Boolean; Error_Msg : out String;
      Error_Len : out Natural)
   is
      Err_Hdr   : constant String := "Insufficient data for header";
      Err_Magic : constant String := "Invalid TZif magic";
      Err_Vers  : constant String := "Unknown TZif version";
      First     : constant Positive := Error_Msg'First;
   begin
      Is_Ok     := False;
      Error_Msg := [others => ' '];
      Error_Len := 0;

      --  Check minimum size for header
      if Length - Pos + 1 < Header_Size then
         Error_Msg (First .. First + Err_Hdr'Length - 1) := Err_Hdr;
         Error_Len := Err_Hdr'Length;
         return;
      end if;

      --  Validate magic number
      for I in TZif_Magic'Range loop
         if Bytes (Pos + I - 1) /= TZif_Magic (I) then
            Error_Msg (First .. First + Err_Magic'Length - 1) := Err_Magic;
            Error_Len := Err_Magic'Length;
            return;
         end if;
      end loop;

      --  Parse version byte (position 4, 0-indexed)
      declare
         Version_Byte : constant Unsigned_8 := Bytes (Pos + 4);
      begin
         case Version_Byte is
            when 16#00# =>
               Header.Version := Version_1;
            when 16#32# =>  -- '2'
               Header.Version := Version_2;
            when 16#33# =>  -- '3'
               Header.Version := Version_3;
            when 16#34# =>  -- '4'
               Header.Version := Version_4;
            when others =>
               Error_Msg (First .. First + Err_Vers'Length - 1) := Err_Vers;
               Error_Len := Err_Vers'Length;
               return;
         end case;
      end;

      --  Skip reserved bytes (5-19), parse counts (20-43)
      declare
         Count_Base : constant Positive := Pos + 20;
      begin
         Header.UTC_Local_Count     :=
           Natural (Read_Int32_At (Bytes, Count_Base));
         Header.Standard_Wall_Count :=
           Natural (Read_Int32_At (Bytes, Count_Base + 4));
         Header.Leap_Count          :=
           Natural (Read_Int32_At (Bytes, Count_Base + 8));
         Header.Transition_Count    :=
           Natural (Read_Int32_At (Bytes, Count_Base + 12));
         Header.Type_Count          :=
           Natural (Read_Int32_At (Bytes, Count_Base + 16));
         Header.Abbrev_Chars        :=
           Natural (Read_Int32_At (Bytes, Count_Base + 20));
      end;

      Data_Start := Pos + Header_Size;
      Is_Ok      := True;
   end Parse_Header_From_Bytes;

   --  ========================================================================
   --  Parse_Data_Section_From_Bytes
   --
   --  Parses TZif data section (transitions, types, abbreviations).
   --  ========================================================================

   procedure Parse_Data_Section_From_Bytes
     (Bytes      :        Byte_Array; Pos : in out Positive; Length : Natural;
      Header     :        TZif_Header_Type; Data : in out TZif_Data_Type;
      Use_64bit  :        Boolean; Is_Ok : out Boolean; Error_Msg : out String;
      Error_Len  : out Natural)
   is
      --  Local type for storing type info before abbreviations are known
      type Type_Info is record
         UTC_Off  : Integer_32;
         Is_DST   : Boolean;
         Abbr_Idx : Natural;
      end record;

      type Type_Info_Array is
        array (Natural range 0 .. 255) of Type_Info;  -- Max 256 types

      Err_Data : constant String := "Insufficient data for data section";
      First    : constant Positive := Error_Msg'First;
   begin
      Is_Ok     := True;
      Error_Msg := [others => ' '];
      Error_Len := 0;

      --  Early exit if no data to parse
      if Header.Type_Count = 0 and then Header.Transition_Count = 0 then
         return;
      end if;

      --  Calculate required data size
      declare
         Time_Size           : constant Natural :=
           (if Use_64bit then 8 else 4);
         Required_Transition : constant Natural :=
           Header.Transition_Count * Time_Size + Header.Transition_Count;
         Required_Types      : constant Natural := Header.Type_Count * 6;
         Required_Abbrev     : constant Natural := Header.Abbrev_Chars;
         Required_Leap       : constant Natural :=
           Header.Leap_Count * (if Use_64bit then 12 else 8);
         Required_Indicators : constant Natural :=
           Header.Standard_Wall_Count + Header.UTC_Local_Count;
         Total_Required : constant Natural :=
           Required_Transition + Required_Types + Required_Abbrev +
           Required_Leap + Required_Indicators;
      begin
         if Length - Pos + 1 < Total_Required then
            Error_Msg (First .. First + Err_Data'Length - 1) := Err_Data;
            Error_Len := Err_Data'Length;
            Is_Ok     := False;
            return;
         end if;
      end;

      --  Parse transition times
      for I in 1 .. Header.Transition_Count loop
         declare
            Time     : Epoch_Seconds_Type;
            Time_Raw : Integer_64;
         begin
            if Use_64bit then
               Time_Raw := Read_Int64_At (Bytes, Pos);
               Pos      := Pos + 8;
            else
               Time_Raw := Integer_64 (Read_Int32_At (Bytes, Pos));
               Pos      := Pos + 4;
            end if;
            Time := Epoch_Seconds_Type (Time_Raw);

            Transition_Vectors.Unchecked_Append
              (Data.Transitions,
               Transition_Type'(Time => Time, Type_Index => 0));
         end;
      end loop;

      --  Parse transition type indices
      for I in 1 .. Header.Transition_Count loop
         declare
            Type_Index : constant Natural := Natural (Bytes (Pos));
         begin
            Pos := Pos + 1;
            Transition_Vectors.Unchecked_Replace
              (Data.Transitions, I,
               Transition_Type'
                 (Time =>
                    Transition_Vectors.Unchecked_Element
                      (Data.Transitions, I).Time,
                  Type_Index => Type_Index));
         end;
      end loop;

      --  Parse timezone types (6 bytes each: 4-byte offset, 1-byte DST,
      --  1-byte abbr idx)
      declare
         Type_Infos : Type_Info_Array := [others => (0, False, 0)];
      begin
         for I in 0 .. Header.Type_Count - 1 loop
            Type_Infos (I).UTC_Off  := Read_Int32_At (Bytes, Pos);
            Pos                     := Pos + 4;
            Type_Infos (I).Is_DST   := Bytes (Pos) /= 0;
            Pos                     := Pos + 1;
            Type_Infos (I).Abbr_Idx := Natural (Bytes (Pos));
            Pos                     := Pos + 1;
         end loop;

         --  Read abbreviation string block
         declare
            Abbrev_Start : constant Positive := Pos;
            Abbrev_End   : constant Positive :=
              Pos + Header.Abbrev_Chars - 1;

            function Extract_Abbreviation
              (Start_Idx : Natural) return String
            is
               Act_Start : constant Positive := Abbrev_Start + Start_Idx;
               End_Idx   : Natural           := Act_Start;
            begin
               if Act_Start > Abbrev_End then
                  return "";
               end if;

               while End_Idx <= Abbrev_End
                 and then Bytes (End_Idx) /= 0
               loop
                  End_Idx := End_Idx + 1;
               end loop;

               declare
                  Len    : constant Natural  := End_Idx - Act_Start;
                  Result : String (1 .. Len) := [others => ' '];
               begin
                  for J in 1 .. Len loop
                     Result (J) := Character'Val (Bytes (Act_Start + J - 1));
                  end loop;
                  return Result;
               end;
            end Extract_Abbreviation;

         begin
            --  Skip past abbreviation block
            Pos := Pos + Header.Abbrev_Chars;

            --  Build timezone types with abbreviations
            for I in 0 .. Header.Type_Count - 1 loop
               declare
                  Abbrev : constant String :=
                    Extract_Abbreviation (Type_Infos (I).Abbr_Idx);
               begin
                  Timezone_Type_Vectors.Unchecked_Append
                    (Data.Timezone_Types,
                     Make_Timezone_Type
                       (UTC_Offset   =>
                          UTC_Offset_Type (Type_Infos (I).UTC_Off),
                        Is_DST       => Type_Infos (I).Is_DST,
                        Abbreviation => Abbrev));
               end;
            end loop;
         end;
      end;

      --  Skip leap seconds
      Pos := Pos + Header.Leap_Count * (if Use_64bit then 12 else 8);

      --  Skip standard/wall and UT/local indicators
      Pos := Pos + Header.Standard_Wall_Count + Header.UTC_Local_Count;

   end Parse_Data_Section_From_Bytes;

   --  ========================================================================
   --  Parse_V1
   --
   --  Parse TZif version 1 (32-bit timestamps).
   --  ========================================================================

   procedure Parse_V1
     (Bytes :     Byte_Array; Length : Natural;
      Result : out Parse_Result_Type)
   is
      Data       : TZif_Data_Type;
      Header     : TZif_Header_Type;
      Pos        : Positive;
      Data_Start : Positive;
      Is_Ok      : Boolean;
      Error_Msg  : String (1 .. 64) := [others => ' '];
      Error_Len  : Natural;
   begin
      --  Parse header
      Parse_Header_From_Bytes
        (Bytes, 1, Length, Header, Data_Start, Is_Ok, Error_Msg, Error_Len);

      if not Is_Ok then
         Result :=
           Parse_Result.Error
             (TZif.Domain.Error.Parse_Error, Error_Msg (1 .. Error_Len));
         return;
      end if;

      Data.Header := Header;
      Pos         := Data_Start;

      --  Initialize vectors
      Data.Transitions    := Transition_Vectors.Empty_Vector;
      Data.Timezone_Types := Timezone_Type_Vectors.Empty_Vector;
      Data.Leap_Seconds   := Leap_Second_Vectors.Empty_Vector;
      Data.POSIX_TZ       := POSIX_TZ_Strings.Null_Bounded_String;

      --  Parse data section (32-bit mode)
      Parse_Data_Section_From_Bytes
        (Bytes, Pos, Length, Header, Data, False, Is_Ok, Error_Msg, Error_Len);

      if not Is_Ok then
         Result :=
           Parse_Result.Error
             (TZif.Domain.Error.Parse_Error, Error_Msg (1 .. Error_Len));
         return;
      end if;

      Result := Parse_Result.Ok (Data);
   end Parse_V1;

   --  ========================================================================
   --  Parse_V2_V3
   --
   --  Parse TZif version 2 or 3 (64-bit timestamps).
   --  Skips V1 section and reads 64-bit V2/V3 section.
   --  ========================================================================

   procedure Parse_V2_V3
     (Bytes :     Byte_Array; Length : Natural;
      Result : out Parse_Result_Type)
   is
      Data       : TZif_Data_Type;
      V1_Header  : TZif_Header_Type;
      V2_Header  : TZif_Header_Type;
      Pos        : Positive;
      Data_Start : Positive;
      Is_Ok      : Boolean;
      Error_Msg  : String (1 .. 64) := [others => ' '];
      Error_Len  : Natural;
   begin
      --  Parse V1 header first
      Parse_Header_From_Bytes
        (Bytes, 1, Length, V1_Header, Data_Start, Is_Ok, Error_Msg, Error_Len);

      if not Is_Ok then
         Result :=
           Parse_Result.Error
             (TZif.Domain.Error.Parse_Error, Error_Msg (1 .. Error_Len));
         return;
      end if;

      --  Calculate size of V1 data section to skip
      declare
         V1_Data_Size : constant Natural :=
           V1_Header.Transition_Count * 5 +  -- 4-byte time + 1-byte index
           V1_Header.Type_Count * 6 +  -- 6 bytes per type
           V1_Header.Abbrev_Chars + V1_Header.Leap_Count * 8 +  -- 8 for V1
           V1_Header.Standard_Wall_Count + V1_Header.UTC_Local_Count;
      begin
         Pos := Data_Start + V1_Data_Size;
      end;

      --  Parse V2/V3 header
      Parse_Header_From_Bytes
        (Bytes, Pos, Length, V2_Header, Data_Start, Is_Ok, Error_Msg,
         Error_Len);

      if not Is_Ok then
         Result :=
           Parse_Result.Error
             (TZif.Domain.Error.Parse_Error, Error_Msg (1 .. Error_Len));
         return;
      end if;

      Data.Header := V2_Header;
      Pos         := Data_Start;

      --  Initialize vectors
      Data.Transitions    := Transition_Vectors.Empty_Vector;
      Data.Timezone_Types := Timezone_Type_Vectors.Empty_Vector;
      Data.Leap_Seconds   := Leap_Second_Vectors.Empty_Vector;
      Data.POSIX_TZ       := POSIX_TZ_Strings.Null_Bounded_String;

      --  Parse data section (64-bit mode)
      Parse_Data_Section_From_Bytes
        (Bytes, Pos, Length, V2_Header, Data, True, Is_Ok, Error_Msg,
         Error_Len);

      if not Is_Ok then
         Result :=
           Parse_Result.Error
             (TZif.Domain.Error.Parse_Error, Error_Msg (1 .. Error_Len));
         return;
      end if;

      --  Read POSIX TZ string (optional, after newline)
      if Pos <= Length and then Bytes (Pos) = 16#0A# then  -- LF
         Pos := Pos + 1;  -- Skip newline
         declare
            TZ_Start : constant Positive := Pos;
            TZ_End   : Natural           := Pos;
         begin
            --  Find ending newline
            while TZ_End <= Length and then Bytes (TZ_End) /= 16#0A# loop
               TZ_End := TZ_End + 1;
            end loop;

            if TZ_End > TZ_Start then
               declare
                  TZ_Len : constant Natural                    :=
                    Natural'Min (TZ_End - TZ_Start, 512);
                  TZ_Str : String (1 .. TZ_Len) := [others => ' '];
               begin
                  for I in 1 .. TZ_Len loop
                     TZ_Str (I) := Character'Val (Bytes (TZ_Start + I - 1));
                  end loop;
                  Data.POSIX_TZ := POSIX_TZ_Strings.To_Bounded_String (TZ_Str);
               end;
            end if;
         end;
      end if;

      Result := Parse_Result.Ok (Data);
   end Parse_V2_V3;

   --  ========================================================================
   --  Parse_From_Bytes (Main Entry Point)
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
                (TZif.Domain.Error.Parse_Error, "Invalid TZif magic");
            return;
         end if;
      end loop;

      --  Step 3: Read version byte (offset 4, zero-indexed = position 5)
      Version := Bytes (5);

      --  Step 4: Dispatch to version-specific parser
      case Version is
         when 16#00# =>
            --  TZif version 1 (legacy 32-bit)
            Parse_V1 (Bytes, Length, Result);

         when 16#32# | 16#33# | 16#34# =>
            --  TZif version 2, 3, or 4 (64-bit)
            Parse_V2_V3 (Bytes, Length, Result);

         when others =>
            Result :=
              Parse_Result.Error
                (TZif.Domain.Error.Parse_Error, "Unknown TZif version");
      end case;

   end Parse_From_Bytes;

end TZif.Domain.Parser;
