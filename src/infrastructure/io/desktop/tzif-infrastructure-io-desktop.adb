pragma Ada_2022;
--  ===========================================================================
--  Tzif.Infrastructure.Io.Desktop
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Desktop implementation.
--
--  ===========================================================================

with Ada.Streams.Stream_IO;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with TZif.Domain.Error;
with TZif.Domain.Value_Object.Zone_Id;
with TZif.Domain.Value_Object.Source_Info;

package body TZif.Infrastructure.IO.Desktop with
  SPARK_Mode => Off
is

   use TZif.Domain.Value_Object.Zone_Id;
   use TZif.Domain.Value_Object.Source_Info;

   --  Rename Stream_IO to avoid namespace conflicts with Ada.Directories
   package SIO renames Ada.Streams.Stream_IO;

   --  Standard zoneinfo base path on POSIX systems
   Zoneinfo_Base : constant String := "/usr/share/zoneinfo/";

   ----------------------------------------------------------------------
   --  Read_File
   --
   --  Read TZif binary file from filesystem.
   --
   --  Implementation:
   --    1. Construct file path from zone ID
   --    2. Open file for reading
   --    3. Read bytes into buffer
   --    4. Return byte count or error
   ----------------------------------------------------------------------
   procedure Read_File
     (Id     :     TZif.Application.Port.Inbound.Find_By_Id.Zone_Id_Input_Type;
      Bytes  : out Byte_Array; Length : out Natural;
      Result : out Read_File_Result.Result)
   is
      use Ada.Exceptions;
      File      : SIO.File_Type;
      Stream    : SIO.Stream_Access;
      File_Path : constant String := Zoneinfo_Base & To_String (Id);
   begin
      Length := 0;

      --  Step 1: Open TZif file
      begin
         SIO.Open (File, SIO.In_File, File_Path);
      exception
         when E : SIO.Name_Error =>
            Result :=
              Read_File_Result.Error
                (TZif.Domain.Error.Not_Found_Error,
                 "Zone file not found: " & File_Path & ": " &
                 Exception_Message (E));
            return;
         when E : SIO.Use_Error  =>
            Result :=
              Read_File_Result.Error
                (TZif.Domain.Error.IO_Error,
                 "Cannot access zone file: " & File_Path & ": " &
                 Exception_Message (E));
            return;
         when E : others         =>
            Result :=
              Read_File_Result.Error
                (TZif.Domain.Error.IO_Error,
                 "File open error for " & File_Path & ": " &
                 Exception_Message (E));
            return;
      end;

      --  Step 2: Read file bytes
      begin
         Stream := SIO.Stream (File);

         --  Read bytes one at a time into buffer
         while not SIO.End_Of_File (File)
           and then Length < Bytes'Length
         loop
            Length := Length + 1;
            Unsigned_8'Read (Stream, Bytes (Length));
         end loop;

         SIO.Close (File);

         --  Return success with read info
         Result := Read_File_Result.Ok ((Bytes_Read => Length));

      exception
         when E : SIO.End_Error  =>
            if SIO.Is_Open (File) then
               SIO.Close (File);
            end if;
            Result :=
              Read_File_Result.Error
                (TZif.Domain.Error.Parse_Error,
                 "Unexpected end of file for " & File_Path & ": " &
                 Exception_Message (E));
         when E : SIO.Data_Error =>
            if SIO.Is_Open (File) then
               SIO.Close (File);
            end if;
            Result :=
              Read_File_Result.Error
                (TZif.Domain.Error.Parse_Error,
                 "File data corrupted for " & File_Path & ": " &
                 Exception_Message (E));
         when E : others         =>
            if SIO.Is_Open (File) then
               SIO.Close (File);
            end if;
            Result :=
              Read_File_Result.Error
                (TZif.Domain.Error.IO_Error,
                 "File read error for " & File_Path & ": " &
                 Exception_Message (E));
      end;

   end Read_File;

   ----------------------------------------------------------------------
   --  List_Directory_Sources
   --
   --  Discover timezone sources in directory paths.
   --
   --  Implementation:
   --    For each path in Search_Paths:
   --      1. Check if directory exists
   --      2. Look for +VERSION or VERSION file
   --      3. Count TZif files (files with TZif magic number)
   --      4. Create Source_Info record with generated ULID
   --    Return Ok(Discovery_Data) or collect errors
   ----------------------------------------------------------------------
   procedure List_Directory_Sources
     (Search_Paths : TZif.Application.Port.Inbound.Discover_Sources.Path_List;
      Result       : out TZif.Application.Port.Inbound.Discover_Sources
        .Discovery_Result_Package
        .Result)
   is
      use Ada.Exceptions;
      use Ada.Strings.Fixed;
      use Ada.Directories;
      package Discover renames TZif.Application.Port.Inbound.Discover_Sources;

      Data : Discover.Discovery_Data_Type :=
        (Sources => Discover.Source_Info_Vectors.Empty_Vector,
         Errors  => Discover.Error_Vectors.Empty_Vector);

      --  Read first few bytes of file to check TZif magic
      function Is_TZif_File (Path : String) return Boolean is
         File   : SIO.File_Type;
         Stream : SIO.Stream_Access;
         Magic  : String (1 .. 4);
      begin
         if Kind (Path) /= Ordinary_File then
            return False;
         end if;

         begin
            SIO.Open (File, SIO.In_File, Path);
            Stream := SIO.Stream (File);
            String'Read (Stream, Magic);
            SIO.Close (File);
            return Magic = "TZif";
         exception
            when others =>
               if SIO.Is_Open (File) then
                  SIO.Close (File);
               end if;
               return False;
         end;
      end Is_TZif_File;

      --  Read version from file
      function Read_Version_File (Path : String) return String is
         File   : SIO.File_Type;
         Stream : SIO.Stream_Access;
         Buffer : String (1 .. 64) := [others => ' '];
         Len    : Natural          := 0;
         Ch     : Character;
      begin
         begin
            SIO.Open (File, SIO.In_File, Path);
            Stream := SIO.Stream (File);

            --  Read until newline or buffer full
            while not SIO.End_Of_File (File) and then Len < Buffer'Last loop
               Character'Read (Stream, Ch);
               exit when Ch = ASCII.LF or else Ch = ASCII.CR;
               Len          := Len + 1;
               Buffer (Len) := Ch;
            end loop;

            SIO.Close (File);
            return Trim (Buffer (1 .. Len), Ada.Strings.Both);
         exception
            when others =>
               if SIO.Is_Open (File) then
                  SIO.Close (File);
               end if;
               return "unknown";
         end;
      end Read_Version_File;

      --  Count TZif files in directory (recursive)
      function Count_TZif_Files (Dir_Path : String) return Natural is
         Count  : Natural := 0;
         Search : Search_Type;
         Item   : Directory_Entry_Type;
      begin
         Start_Search (Search, Dir_Path, "*", (others => True));

         while More_Entries (Search) loop
            Get_Next_Entry (Search, Item);

            declare
               Name      : constant String := Simple_Name (Item);
               Full_Path : constant String := Full_Name (Item);
            begin
               --  Skip . and ..
               if Name = "." or else Name = ".." then
                  null;
               elsif Kind (Item) = Directory then
                  --  Skip posix, right, etc. directories
                  if Name /= "posix"
                    and then Name /= "right"
                    and then Name /= "posixrules"
                  then
                     Count := Count + Count_TZif_Files (Full_Path);
                  end if;
               elsif Kind (Item) = Ordinary_File then
                  --  Check if it's a TZif file
                  if Is_TZif_File (Full_Path) then
                     Count := Count + 1;
                  end if;
               end if;
            end;
         end loop;

         End_Search (Search);
         return Count;

      exception
         when others =>
            return Count;
      end Count_TZif_Files;

      --  Generate simple ULID-like identifier
      function Generate_Simple_ULID return ULID_Type is
         use Ada.Calendar;
         Now        : constant Time    := Clock;
         Y          : Year_Number;
         Mo         : Month_Number;
         D          : Day_Number;
         Secs       : Day_Duration;
         Timestamp  : Natural;
         Result_Str : String (1 .. 26) := [others => '0'];
         Base32     : constant String  := "0123456789ABCDEFGHJKMNPQRSTVWXYZ";
         Idx        : Natural;
      begin
         Split (Now, Y, Mo, D, Secs);
         --  Simple timestamp encoding
         Timestamp := (Y - 2000) * 10_000_000 + Mo * 100_000 + D * 1_000 +
           Natural (Secs / 100.0);

         --  Encode first 10 chars from timestamp
         for I in reverse 1 .. 10 loop
            Idx              := (Timestamp mod 32) + 1;
            Result_Str (I)   := Base32 (Idx);
            Timestamp        := Timestamp / 32;
         end loop;

         --  Fill rest with pseudo-random based on Day_Duration
         declare
            Rand : Natural := Natural (Secs * 1_000.0) mod 1_000_000;
         begin
            for I in 11 .. 26 loop
               Idx            := (Rand mod 32) + 1;
               Result_Str (I) := Base32 (Idx);
               Rand           := (Rand * 7 + 13) mod 1_000_000;
            end loop;
         end;

         return Make_ULID (Result_Str);
      end Generate_Simple_ULID;

      --  Helper to add error to Data.Errors
      procedure Add_Error
        (Kind : TZif.Domain.Error.Error_Kind; Msg : String)
      is
         Err : constant TZif.Domain.Error.Error_Type :=
           (Kind    => Kind,
            Message =>
              TZif.Domain.Error.Error_Strings.To_Bounded_String (Msg));
      begin
         Data.Errors.Append (Err);
      end Add_Error;

   begin
      --  Process each search path
      for Path_Elem of Search_Paths loop
         declare
            Dir_Path : constant String :=
              Discover.Path_Strings.To_String (Path_Elem);
         begin
            --  Check if path exists
            if not Exists (Dir_Path) then
               Add_Error
                 (TZif.Domain.Error.Not_Found_Error,
                  "Path not found: " & Dir_Path);

            elsif Kind (Dir_Path) /= Directory then
               Add_Error
                 (TZif.Domain.Error.Validation_Error,
                  "Not a directory: " & Dir_Path);

            else
               --  Look for VERSION file
               declare
                  Version_Path1 : constant String := Dir_Path & "/+VERSION";
                  Version_Path2 : constant String := Dir_Path & "/VERSION";
                  Version_Str   : Version_String_Type;
               begin
                  if Exists (Version_Path1) then
                     Version_Str :=
                       Make_Version (Read_Version_File (Version_Path1));
                  elsif Exists (Version_Path2) then
                     Version_Str :=
                       Make_Version (Read_Version_File (Version_Path2));
                  else
                     Version_Str := Make_Version ("unknown");
                  end if;

                  --  Count TZif files
                  declare
                     Zone_Count : constant Natural :=
                       Count_TZif_Files (Dir_Path);
                     Source     : Source_Info_Type;
                  begin
                     if Zone_Count > 0 then
                        Source :=
                          Make_Source_Info
                            (ULID       => Generate_Simple_ULID,
                             Path       => Make_Path (Dir_Path),
                             Version    => Version_Str,
                             Zone_Count => Zone_Count);
                        Data.Sources.Append (Source);
                     else
                        Add_Error
                          (TZif.Domain.Error.Validation_Error,
                           "No TZif files found in: " & Dir_Path);
                     end if;
                  end;
               end;
            end if;

         exception
            when E : others =>
               Add_Error
                 (TZif.Domain.Error.IO_Error,
                  "Error scanning: " & Dir_Path & ": " &
                  Exception_Message (E));
         end;
      end loop;

      --  Return result
      Result := Discover.Discovery_Result_Package.Ok (Data);

   end List_Directory_Sources;

   ----------------------------------------------------------------------
   --  Get_Modified_Time
   --
   --  DROPPED FEATURE: No known use case
   --
   --  This operation was originally planned but dropped during v1.0.0
   --  development. Cache invalidation is handled through other
   --  mechanisms defined in the SRS.
   --  If a use case emerges, this can be reconsidered in future releases.
   ----------------------------------------------------------------------
   procedure Get_Modified_Time
     (Id        : TZif.Application.Port.Inbound.Find_By_Id.Zone_Id_Input_Type;
      Timestamp : out Timestamp_Type;
      Result    : out Get_Modified_Time_Result.Result)
   is
      use Ada.Calendar;
   begin
      --  DROPPED: No implementation needed
      --  Returning current time as placeholder to satisfy signature

      pragma Unreferenced (Id);
      Timestamp := Clock;
      Result    := Get_Modified_Time_Result.Ok (Timestamp);

   end Get_Modified_Time;

end TZif.Infrastructure.IO.Desktop;
