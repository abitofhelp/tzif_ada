pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.Io.Desktop
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Desktop implementation.
--
--  Implementation Notes:
--    Uses Functional.Try and Functional.Scoped for exception boundary handling.
--    All I/O operations that may raise exceptions are wrapped with
--    Functional.Try.Try_To_Any_Result_With_Param, and file handles use
--    Functional.Scoped.Conditional_Guard_For for automatic cleanup.
--
--  ===========================================================================

with Ada.Streams.Stream_IO;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Characters.Handling;
with Functional.Option;
with Functional.Scoped;
with Functional.Try;
with GNAT.Regpat;
with TZif.Domain.Error;
with TZif.Domain.Value_Object.Zone_Id;
with TZif.Domain.Value_Object.Unit;
with TZif.Infrastructure.Platform;
with TZif.Infrastructure.Platform.POSIX;
with TZif.Infrastructure.ULID;

package body TZif.Infrastructure.IO.Desktop with
  SPARK_Mode => Off
is

   use TZif.Domain.Value_Object.Zone_Id;
   use TZif.Domain.Value_Object.Source_Info;

   --  Rename Stream_IO to avoid namespace conflicts with Ada.Directories
   package SIO renames Ada.Streams.Stream_IO;

   --  ========================================================================
   --  Exception Mapping (used by Functional.Try)
   --  ========================================================================

   function Map_IO_Exception
     (Occ : Ada.Exceptions.Exception_Occurrence)
      return TZif.Domain.Error.Error_Type
   is
      use Ada.Exceptions;
      use TZif.Domain.Error;
      Exc_Name : constant String := Exception_Name (Occ);
   begin
      if Exc_Name = "ADA.IO_EXCEPTIONS.NAME_ERROR"
        or else Exc_Name = "ADA.STREAMS.STREAM_IO.NAME_ERROR"
      then
         return (Kind    => Not_Found_Error,
                 Message => Error_Strings.To_Bounded_String
                   ("File not found: " & Exception_Message (Occ)));
      elsif Exc_Name = "ADA.IO_EXCEPTIONS.USE_ERROR"
        or else Exc_Name = "ADA.STREAMS.STREAM_IO.USE_ERROR"
      then
         return (Kind    => IO_Error,
                 Message => Error_Strings.To_Bounded_String
                   ("Cannot access file: " & Exception_Message (Occ)));
      elsif Exc_Name = "ADA.IO_EXCEPTIONS.END_ERROR"
        or else Exc_Name = "ADA.STREAMS.STREAM_IO.END_ERROR"
      then
         return (Kind    => Parse_Error,
                 Message => Error_Strings.To_Bounded_String
                   ("Unexpected end of file: " & Exception_Message (Occ)));
      elsif Exc_Name = "ADA.IO_EXCEPTIONS.DATA_ERROR"
        or else Exc_Name = "ADA.STREAMS.STREAM_IO.DATA_ERROR"
      then
         return (Kind    => Parse_Error,
                 Message => Error_Strings.To_Bounded_String
                   ("File data corrupted: " & Exception_Message (Occ)));
      else
         return (Kind    => IO_Error,
                 Message => Error_Strings.To_Bounded_String
                   ("I/O error: " & Exception_Message (Occ)));
      end if;
   end Map_IO_Exception;

   --  ========================================================================
   --  Scoped File Guard for Stream_IO
   --  ========================================================================

   package Stream_File_Guard is new Functional.Scoped.Conditional_Guard_For
     (Resource       => SIO.File_Type,
      Should_Release => SIO.Is_Open,
      Release        => SIO.Close);

   --  ========================================================================
   --  Scoped File Guard for Text_IO
   --  ========================================================================

   package Text_File_Guard is new Functional.Scoped.Conditional_Guard_For
     (Resource       => Ada.Text_IO.File_Type,
      Should_Release => Ada.Text_IO.Is_Open,
      Release        => Ada.Text_IO.Close);
   pragma Unreferenced (Text_File_Guard);  --  Will be used in later refactoring

   --  ========================================================================
   --  Default Zoneinfo Path
   --  ========================================================================
   --
   --  This default path is used for operations that don't receive an explicit
   --  source path. The developer should always provide explicit paths when
   --  creating tzif/zoneinfo instances for cross-platform compatibility.
   --
   --  Platform notes:
   --    - Linux/BSD/macOS: /usr/share/zoneinfo is typically pre-installed
   --    - Windows: No default exists; developer must provide path to
   --               downloaded IANA tzdata (https://www.iana.org/time-zones)
   --  ========================================================================

   Zoneinfo_Base : constant String := "/usr/share/zoneinfo/";

   ----------------------------------------------------------------------
   --  Read_File
   --
   --  Read TZif binary file from filesystem.
   --
   --  Implementation:
   --    Uses Functional.Try for exception-to-Result conversion and
   --    Functional.Scoped for automatic file cleanup.
   ----------------------------------------------------------------------

   --  Context for Read_File raw action
   type Read_File_Context is record
      File_Path   : access constant String;
      Bytes       : access Byte_Array;
      Bytes_Read  : Natural := 0;
   end record;

   --  Raw action that may raise - reads file bytes into buffer
   function Raw_Read_File (Ctx : Read_File_Context) return Read_Info is
      File   : aliased SIO.File_Type;
      Guard  : Stream_File_Guard.Guard (File'Access);
      pragma Unreferenced (Guard);  --  RAII: releases via Finalize
      Stream : SIO.Stream_Access;
      Length : Natural := 0;
   begin
      SIO.Open (File, SIO.In_File, Ctx.File_Path.all);
      Stream := SIO.Stream (File);

      --  Read bytes one at a time into buffer
      while not SIO.End_Of_File (File)
        and then Length < Ctx.Bytes'Length
      loop
         Length := Length + 1;
         Unsigned_8'Read (Stream, Ctx.Bytes (Length));
      end loop;

      --  Guard.Finalize will close file automatically
      return (Bytes_Read => Length);
   end Raw_Read_File;

   --  Instantiate Functional.Try for Read_File
   function Try_Read_File is new Functional.Try.Try_To_Any_Result_With_Param
     (T             => Read_Info,
      E             => TZif.Domain.Error.Error_Type,
      Param         => Read_File_Context,
      Result_Type   => Read_File_Result.Result,
      Ok            => Read_File_Result.Ok,
      New_Error     => Read_File_Result.From_Error,
      Map_Exception => Map_IO_Exception,
      Action        => Raw_Read_File);

   procedure Read_File
     (Id     :     TZif.Application.Port.Inbound.Find_By_Id.Zone_Id_Input_Type;
      Bytes  : out Byte_Array; Length : out Natural;
      Result : out Read_File_Result.Result)
   is
      File_Path : aliased constant String := Zoneinfo_Base & To_String (Id);
      Ctx       : Read_File_Context;
   begin
      Length := 0;
      Ctx := (File_Path  => File_Path'Unchecked_Access,
              Bytes      => Bytes'Unrestricted_Access,
              Bytes_Read => 0);

      Result := Try_Read_File (Ctx);

      --  Extract bytes read from successful result
      if Read_File_Result.Is_Ok (Result) then
         Length := Read_File_Result.Value (Result).Bytes_Read;
      end if;
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
      --  Uses Functional.Try with Option for safe file reading
      package Bool_Option is new Functional.Option (T => Boolean);

      function Raw_Check_TZif_Magic (Path : String) return Boolean is
         File   : aliased SIO.File_Type;
         Guard  : Stream_File_Guard.Guard (File'Access);
         pragma Unreferenced (Guard);  --  RAII: releases via Finalize
         Stream : SIO.Stream_Access;
         Magic  : String (1 .. 4);
      begin
         if Kind (Path) /= Ordinary_File then
            return False;
         end if;
         SIO.Open (File, SIO.In_File, Path);
         Stream := SIO.Stream (File);
         String'Read (Stream, Magic);
         return Magic = "TZif";
      end Raw_Check_TZif_Magic;

      function Try_Check_TZif is new Functional.Try.Try_To_Option_With_Param
        (T          => Boolean,
         Param      => String,
         Option_Pkg => Bool_Option,
         Action     => Raw_Check_TZif_Magic);

      function Is_TZif_File (Path : String) return Boolean is
      begin
         return Bool_Option.Unwrap_Or (Try_Check_TZif (Path), Default => False);
      end Is_TZif_File;

      --  Read version from file using Functional.Try + Scoped
      Max_Version_Len : constant := 64;
      subtype Version_Buffer is String (1 .. Max_Version_Len);
      package Version_Option is new Functional.Option (T => Version_Buffer);

      function Raw_Read_Version (Path : String) return Version_Buffer is
         File   : aliased SIO.File_Type;
         Guard  : Stream_File_Guard.Guard (File'Access);
         pragma Unreferenced (Guard);  --  RAII: releases via Finalize
         Stream : SIO.Stream_Access;
         Buffer : Version_Buffer := [others => ' '];
         Len    : Natural := 0;
         Ch     : Character;
      begin
         SIO.Open (File, SIO.In_File, Path);
         Stream := SIO.Stream (File);

         --  Read until newline or buffer full
         while not SIO.End_Of_File (File) and then Len < Buffer'Last loop
            Character'Read (Stream, Ch);
            exit when Ch = ASCII.LF or else Ch = ASCII.CR;
            Len := Len + 1;
            Buffer (Len) := Ch;
         end loop;

         return Buffer;
      end Raw_Read_Version;

      function Try_Read_Version is new Functional.Try.Try_To_Option_With_Param
        (T          => Version_Buffer,
         Param      => String,
         Option_Pkg => Version_Option,
         Action     => Raw_Read_Version);

      function Read_Version_File (Path : String) return String is
         Default_Version : Version_Buffer := [others => ' '];
         Opt    : constant Version_Option.Option := Try_Read_Version (Path);
         Buffer : Version_Buffer;
         Len    : Natural := Max_Version_Len;
      begin
         Default_Version (1 .. 7) := "unknown";
         Buffer := Version_Option.Unwrap_Or (Opt, Default => Default_Version);
         --  Trim trailing spaces
         while Len > 0 and then Buffer (Len) = ' ' loop
            Len := Len - 1;
         end loop;
         return Trim (Buffer (1 .. Len), Ada.Strings.Both);
      end Read_Version_File;

      --  Count TZif files in directory (recursive)
      --  Uses Functional.Try to handle directory access errors
      package Natural_Option is new Functional.Option (T => Natural);

      --  Forward declaration for recursive calls
      function Count_TZif_Files (Dir_Path : String) return Natural;

      function Raw_Count_TZif_Files (Dir_Path : String) return Natural is
         Count  : Natural := 0;
         Search : Search_Type;
         Item   : Directory_Entry_Type;
      begin
         Start_Search (Search, Dir_Path, "*", [others => True]);

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
      end Raw_Count_TZif_Files;

      function Try_Count_TZif is new Functional.Try.Try_To_Option_With_Param
        (T          => Natural,
         Param      => String,
         Option_Pkg => Natural_Option,
         Action     => Raw_Count_TZif_Files);

      function Count_TZif_Files (Dir_Path : String) return Natural is
      begin
         return Natural_Option.Unwrap_Or (Try_Count_TZif (Dir_Path), Default => 0);
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
         if not Discover.Error_Vectors.Is_Full (Data.Errors) then
            Discover.Error_Vectors.Unchecked_Append (Data.Errors, Err);
         end if;
      end Add_Error;

   begin
      --  Process each search path using index-based iteration
      for I in 1 .. Discover.Path_Vectors.Length (Search_Paths) loop
         declare
            Path_Elem : constant Discover.Path_String :=
              Discover.Path_Vectors.Unchecked_Element (Search_Paths, I);
            Dir_Path  : constant String               :=
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
                        if not Discover.Source_Info_Vectors.Is_Full
                            (Data.Sources)
                        then
                           Discover.Source_Info_Vectors.Unchecked_Append
                             (Data.Sources, Source);
                        end if;
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

   ----------------------------------------------------------------------
   --  Read_Version_File
   --
   --  Reads the +VERSION file from the timezone source directory.
   ----------------------------------------------------------------------
   procedure Read_Version_File
     (Source : TZif.Domain.Value_Object.Source_Info.Source_Info_Type;
      Result : out TZif.Application.Port.Inbound.Get_Version.Version_Result)
   is
      use Ada.Directories;
      use Ada.Exceptions;
      package Get_Version renames TZif.Application.Port.Inbound.Get_Version;

      Path_Str     : constant String :=
        TZif.Domain.Value_Object.Source_Info.To_String
          (TZif.Domain.Value_Object.Source_Info.Get_Path (Source));
      Version_File : constant String := Path_Str & "/+VERSION";
   begin
      if not Exists (Version_File) or else Kind (Version_File) /= Ordinary_File
      then
         Result :=
           Get_Version.Version_Result_Package.Error
             (TZif.Domain.Error.Not_Found_Error,
              "Version file not found: " & Version_File);
         return;
      end if;

      --  Read version from file
      declare
         File   : SIO.File_Type;
         Stream : SIO.Stream_Access;
         Buffer : String (1 .. 32) := [others => ' '];
         Len    : Natural          := 0;
         Ch     : Character;
      begin
         SIO.Open (File, SIO.In_File, Version_File);
         Stream := SIO.Stream (File);

         --  Read until newline or buffer full
         while not SIO.End_Of_File (File) and then Len < Buffer'Last loop
            Character'Read (Stream, Ch);
            exit when Ch = ASCII.LF or else Ch = ASCII.CR;
            Len          := Len + 1;
            Buffer (Len) := Ch;
         end loop;

         SIO.Close (File);

         declare
            Trimmed : constant String :=
              Ada.Strings.Fixed.Trim (Buffer (1 .. Len), Ada.Strings.Both);
         begin
            Result :=
              Get_Version.Version_Result_Package.Ok
                (Get_Version.Version_Strings.To_Bounded_String (Trimmed));
         end;

      exception
         when E : others =>
            if SIO.Is_Open (File) then
               SIO.Close (File);
            end if;
            Result :=
              Get_Version.Version_Result_Package.Error
                (TZif.Domain.Error.IO_Error,
                 "Error reading version: " & Exception_Message (E));
      end;

   exception
      when E : others =>
         Result :=
           Get_Version.Version_Result_Package.Error
             (TZif.Domain.Error.IO_Error,
              "Unexpected error: " & Exception_Message (E));
   end Read_Version_File;

   ----------------------------------------------------------------------
   --  Read_System_Timezone_Id
   --
   --  Reads the system's timezone ID from /etc/localtime symlink.
   ----------------------------------------------------------------------
   procedure Read_System_Timezone_Id
     (Result : out TZif.Application.Port.Inbound.Find_My_Id.Result)
   is
      use Ada.Directories;
      use Ada.Exceptions;
      package Find_My_Id renames TZif.Application.Port.Inbound.Find_My_Id;

      Localtime_Path : constant String := "/etc/localtime";
   begin
      if not Exists (Localtime_Path) then
         Result :=
           Find_My_Id.Result_Zone_Id.Error
             (TZif.Domain.Error.Not_Found_Error, "/etc/localtime not found");
         return;
      end if;

      --  Try to resolve symlink using readlink
      declare
         use TZif.Infrastructure.Platform.POSIX;
         Link_Result :
           constant TZif.Infrastructure.Platform.Platform_String_Result :=
           Operations.Read_Link (Localtime_Path);
      begin
         if not TZif.Infrastructure.Platform.String_Result.Is_Ok (Link_Result)
         then
            Result :=
              Find_My_Id.Result_Zone_Id.Error
                (TZif.Domain.Error.IO_Error,
                 "/etc/localtime is not a symlink");
            return;
         end if;

         declare
            Link_Target_Bounded :
              constant TZif.Infrastructure.Platform.Platform_String :=
              TZif.Infrastructure.Platform.String_Result.Value (Link_Result);
            Link_Target         : constant String                   :=
              TZif.Infrastructure.Platform.Platform_Strings.To_String
                (Link_Target_Bounded);
            Marker              : constant String                   :=
              "zoneinfo/";
            Marker_Pos          : Natural                           := 0;
         begin
            --  Find "zoneinfo/" marker
            for I in Link_Target'First .. Link_Target'Last - Marker'Length + 1
            loop
               if Link_Target (I .. I + Marker'Length - 1) = Marker then
                  Marker_Pos := I;
                  exit;
               end if;
            end loop;

            if Marker_Pos = 0 then
               Result :=
                 Find_My_Id.Result_Zone_Id.Error
                   (TZif.Domain.Error.IO_Error,
                    "Cannot extract zone ID from: " & Link_Target);
               return;
            end if;

            declare
               Zone_Id_Start : constant Positive := Marker_Pos + Marker'Length;
               Zone_Id_Str   : constant String   :=
                 Link_Target (Zone_Id_Start .. Link_Target'Last);
            begin
               Result :=
                 Find_My_Id.Result_Zone_Id.Ok (Make_Zone_Id (Zone_Id_Str));
            end;
         end;
      end;
   exception
      when E : others =>
         Result :=
           Find_My_Id.Result_Zone_Id.Error
             (TZif.Domain.Error.IO_Error,
              "Error detecting timezone: " & Exception_Message (E));
   end Read_System_Timezone_Id;

   ----------------------------------------------------------------------
   --  List_Zones_In_Source
   --
   --  Lists all timezone IDs in a source directory, sorted.
   ----------------------------------------------------------------------
   procedure List_Zones_In_Source
     (Source     : TZif.Domain.Value_Object.Source_Info.Source_Info_Type;
      Descending : Boolean;
      Result     : out TZif.Application.Port.Inbound.List_All_Order_By_Id
        .List_All_Zones_Result)
   is
      use Ada.Directories;
      use Ada.Exceptions;
      package List_All renames
        TZif.Application.Port.Inbound.List_All_Order_By_Id;

      Path_Str : constant String :=
        TZif.Domain.Value_Object.Source_Info.To_String
          (TZif.Domain.Value_Object.Source_Info.Get_Path (Source));
      Zones    : List_All.Zone_Id_List;

      procedure Scan_Directory (Dir_Path : String; Prefix : String := "") is
         Search : Search_Type;
         pragma Warnings (Off, Search);
         Item : Directory_Entry_Type;
      begin
         Start_Search (Search, Dir_Path, "*");

         while More_Entries (Search) loop
            Get_Next_Entry (Search, Item);

            declare
               Name : constant String := Simple_Name (Item);
            begin
               if Name'Length > 0 and then Name (Name'First) /= '.' then
                  declare
                     Full_Path : constant String := Full_Name (Item);
                     Zone_Name : constant String :=
                       (if Prefix = "" then Name else Prefix & "/" & Name);
                  begin
                     case Kind (Item) is
                        when Directory =>
                           Scan_Directory (Full_Path, Zone_Name);

                        when Ordinary_File =>
                           if Name /= "zone.tab"
                             and then Name /= "zone1970.tab"
                             and then Name /= "iso3166.tab"
                             and then Name /= "leapseconds"
                             and then Name /= "tzdata.zi"
                             and then Name /= "+VERSION"
                           then
                              begin
                                 if not List_All.Zone_Id_Vectors.Is_Full
                                   (Zones)
                                 then
                                    List_All.Zone_Id_Vectors.Unchecked_Append
                                      (Zones, Make_Zone_Id (Zone_Name));
                                 end if;
                              exception
                                 when Constraint_Error =>
                                    --  Skip invalid zone names
                                    null;
                              end;
                           end if;

                        when others =>
                           --  Skip non-directory/non-file entries
                           null;
                     end case;
                  end;
               end if;
            end;
         end loop;

         End_Search (Search);
      exception
         when Ada.Directories.Name_Error | Ada.Directories.Use_Error =>
            --  Skip inaccessible directories silently
            null;
      end Scan_Directory;

      function Less_Than (Left, Right : Zone_Id_Type) return Boolean is
        (To_String (Left) < To_String (Right));

      procedure Sort_Zones is new List_All.Zone_Id_Vectors.Generic_Sort
        ("<" => Less_Than);

   begin
      if not Exists (Path_Str) or else Kind (Path_Str) /= Directory then
         Result :=
           List_All.List_All_Zones_Result_Package.Error
             (TZif.Domain.Error.Not_Found_Error,
              "Source path not found: " & Path_Str);
         return;
      end if;

      Scan_Directory (Path_Str);

      Sort_Zones (Zones);
      if Descending then
         List_All.Zone_Id_Vectors.Reverse_Elements (Zones);
      end if;

      Result := List_All.List_All_Zones_Result_Package.Ok (Zones);

   exception
      when E : others =>
         Result :=
           List_All.List_All_Zones_Result_Package.Error
             (TZif.Domain.Error.IO_Error,
              "Error listing zones: " & Exception_Message (E));
   end List_Zones_In_Source;

   ----------------------------------------------------------------------
   --  Load_Source_From_Path
   --
   --  Loads timezone source metadata from filesystem path.
   ----------------------------------------------------------------------
   procedure Load_Source_From_Path
     (Path   : TZif.Application.Port.Inbound.Load_Source.Path_String;
      Result : out TZif.Application.Port.Inbound.Load_Source
        .Load_Source_Result)
   is
      use Ada.Directories;
      use Ada.Exceptions;
      use Ada.Text_IO;
      package Load_Source renames TZif.Application.Port.Inbound.Load_Source;

      Path_Str : constant String :=
        Load_Source.Path_Strings.To_String (Path);
   begin
      if not Exists (Path_Str) then
         Result :=
           Load_Source.Load_Source_Result_Package.Error
             (TZif.Domain.Error.Not_Found_Error,
              "Path not found: " & Path_Str);
         return;
      end if;

      if Kind (Path_Str) /= Directory then
         Result :=
           Load_Source.Load_Source_Result_Package.Error
             (TZif.Domain.Error.Validation_Error,
              "Path is not a directory: " & Path_Str);
         return;
      end if;

      declare
         ULID         : constant ULID_Type        :=
           TZif.Infrastructure.ULID.Generate;
         Path_Val     : constant Path_String_Type := Make_Path (Path_Str);
         Version_File : constant String           := Path_Str & "/+VERSION";
         Version_Str  : String (1 .. 32);
         Last         : Natural;
         File         : File_Type;
      begin
         if Exists (Version_File) and then Kind (Version_File) = Ordinary_File
         then
            Open (File, In_File, Version_File);
            Get_Line (File, Version_Str, Last);
            Close (File);
         else
            Version_Str (1 .. 7) := "unknown";
            Last                 := 7;
         end if;

         declare
            Version    : constant Version_String_Type :=
              Make_Version (Version_Str (1 .. Last));
            Zone_Count : Natural                      := 0;

            procedure Count_Zones (Dir_Path : String) is

               procedure Count_Recursive (P : String) is
                  S : Search_Type;
                  pragma Warnings (Off, S);
                  I : Directory_Entry_Type;
               begin
                  if Zone_Count > 1_000 then
                     return;
                  end if;

                  Start_Search (S, P, "*");
                  while More_Entries (S) loop
                     Get_Next_Entry (S, I);
                     declare
                        N : constant String := Simple_Name (I);
                     begin
                        if N'Length > 0 and then N (N'First) /= '.' then
                           case Kind (I) is
                              when Directory =>
                                 Count_Recursive (Full_Name (I));

                              when Ordinary_File =>
                                 if N /= "zone.tab"
                                   and then N /= "zone1970.tab"
                                   and then N /= "iso3166.tab"
                                   and then N /= "leapseconds"
                                   and then N /= "tzdata.zi"
                                   and then N /= "+VERSION"
                                 then
                                    Zone_Count := Zone_Count + 1;
                                 end if;

                              when others =>
                                 null;
                           end case;
                        end if;
                     end;
                  end loop;
                  End_Search (S);
               exception
                  when Ada.Directories.Name_Error
                     | Ada.Directories.Use_Error =>
                     null;
               end Count_Recursive;

            begin
               Count_Recursive (Dir_Path);
            end Count_Zones;

            Source : Source_Info_Type;
         begin
            Count_Zones (Path_Str);
            Source := Make_Source_Info (ULID, Path_Val, Version, Zone_Count);
            Result := Load_Source.Load_Source_Result_Package.Ok (Source);
         end;
      exception
         when E : others =>
            if Is_Open (File) then
               Close (File);
            end if;
            Result :=
              Load_Source.Load_Source_Result_Package.Error
                (TZif.Domain.Error.IO_Error,
                 "Error loading source: " & Exception_Message (E));
      end;

   exception
      when E : others =>
         Result :=
           Load_Source.Load_Source_Result_Package.Error
             (TZif.Domain.Error.IO_Error,
              "Unexpected error: " & Exception_Message (E));
   end Load_Source_From_Path;

   ----------------------------------------------------------------------
   --  Validate_Source_Path
   --
   --  Validates that a path is a valid timezone source.
   ----------------------------------------------------------------------
   procedure Validate_Source_Path
     (Path   : TZif.Application.Port.Inbound.Validate_Source.Path_String;
      Result : out TZif.Application.Port.Inbound.Validate_Source
        .Validation_Result)
   is
      use Ada.Directories;
      use Ada.Exceptions;
      package Validate_Source renames
        TZif.Application.Port.Inbound.Validate_Source;

      Path_Str : constant String :=
        Validate_Source.Path_Strings.To_String (Path);
   begin
      if not Exists (Path_Str) then
         Result := Validate_Source.Validation_Result_Package.Ok (False);
         return;
      end if;

      if Kind (Path_Str) /= Directory then
         Result := Validate_Source.Validation_Result_Package.Ok (False);
         return;
      end if;

      --  Check for at least one TZif file
      declare
         Search     : Search_Type;
         pragma Warnings (Off, Search);
         Item       : Directory_Entry_Type;
         Found_TZif : Boolean := False;
      begin
         Start_Search (Search, Path_Str, "*");
         while More_Entries (Search) and then not Found_TZif loop
            Get_Next_Entry (Search, Item);
            if Kind (Item) = Ordinary_File then
               Found_TZif := True;
            end if;
         end loop;
         End_Search (Search);

         Result := Validate_Source.Validation_Result_Package.Ok (Found_TZif);
      exception
         when Name_Error | Use_Error =>
            Result := Validate_Source.Validation_Result_Package.Ok (False);
      end;

   exception
      when E : others =>
         Result :=
           Validate_Source.Validation_Result_Package.Error
             (TZif.Domain.Error.IO_Error,
              "Error validating source: " & Exception_Message (E));
   end Validate_Source_Path;

   ----------------------------------------------------------------------
   --  Find_Zones_By_Pattern
   --
   --  Finds timezone IDs matching a substring pattern.
   ----------------------------------------------------------------------
   procedure Find_Zones_By_Pattern
     (Pattern : TZif.Application.Port.Inbound.Find_By_Pattern.Pattern_String;
      Yield   : TZif.Application.Port.Inbound.Find_By_Pattern
        .Yield_Callback_Access;
      Result  : out TZif.Application.Port.Inbound.Find_By_Pattern
        .Find_By_Pattern_Result)
   is
      use Ada.Directories;
      use Ada.Exceptions;
      package Find_By_Pattern renames
        TZif.Application.Port.Inbound.Find_By_Pattern;

      Pattern_Str : constant String :=
        Find_By_Pattern.Pattern_Strings.To_String (Pattern);

      procedure Scan_Directory (Dir_Path : String; Prefix : String := "") is
         Search : Search_Type;
         pragma Warnings (Off, Search);
         Item : Directory_Entry_Type;
      begin
         Start_Search (Search, Dir_Path, "*");

         while More_Entries (Search) loop
            Get_Next_Entry (Search, Item);

            declare
               Name : constant String := Simple_Name (Item);
            begin
               if Name'Length > 0 and then Name (Name'First) /= '.' then
                  declare
                     Full_Path : constant String := Full_Name (Item);
                     Zone_Name : constant String :=
                       (if Prefix = "" then Name else Prefix & "/" & Name);
                  begin
                     case Kind (Item) is
                        when Directory =>
                           Scan_Directory (Full_Path, Zone_Name);

                        when Ordinary_File =>
                           declare
                              Lower_Zone    : constant String :=
                                Ada.Characters.Handling.To_Lower (Zone_Name);
                              Lower_Pattern : constant String :=
                                Ada.Characters.Handling.To_Lower (Pattern_Str);
                           begin
                              if Ada.Strings.Fixed.Index
                                  (Lower_Zone, Lower_Pattern) >
                                0
                              then
                                 Yield
                                   (Find_By_Pattern.Zone_Name_Strings
                                      .To_Bounded_String
                                      (Zone_Name));
                              end if;
                           end;

                        when others =>
                           null;
                     end case;
                  end;
               end if;
            end;
         end loop;

         End_Search (Search);
      exception
         when Ada.Directories.Name_Error | Ada.Directories.Use_Error =>
            null;
      end Scan_Directory;

   begin
      if Exists (Zoneinfo_Base) and then Kind (Zoneinfo_Base) = Directory then
         Scan_Directory (Zoneinfo_Base);
      end if;

      Result :=
        Find_By_Pattern.Find_By_Pattern_Result_Package.Ok
          (TZif.Domain.Value_Object.Unit.Unit);

   exception
      when E : others =>
         Result :=
           Find_By_Pattern.Find_By_Pattern_Result_Package.Error
             (TZif.Domain.Error.IO_Error,
              "Error searching pattern: " & Exception_Message (E));
   end Find_Zones_By_Pattern;

   ----------------------------------------------------------------------
   --  Find_Zones_By_Region
   --
   --  Finds timezone IDs by region prefix.
   ----------------------------------------------------------------------
   procedure Find_Zones_By_Region
     (Region : TZif.Application.Port.Inbound.Find_By_Region.Region_String;
      Yield  : TZif.Application.Port.Inbound.Find_By_Region
        .Yield_Callback_Access;
      Result : out TZif.Application.Port.Inbound.Find_By_Region
        .Find_By_Region_Result)
   is
      use Ada.Directories;
      use Ada.Exceptions;
      package Find_By_Region renames
        TZif.Application.Port.Inbound.Find_By_Region;

      Region_Str : constant String :=
        Find_By_Region.Region_Strings.To_String (Region);

      procedure Scan_Directory (Dir_Path : String; Prefix : String := "") is
         Search : Search_Type;
         pragma Warnings (Off, Search);
         Item : Directory_Entry_Type;
      begin
         Start_Search (Search, Dir_Path, "*");

         while More_Entries (Search) loop
            Get_Next_Entry (Search, Item);

            declare
               Name : constant String := Simple_Name (Item);
            begin
               if Name'Length > 0 and then Name (Name'First) /= '.' then
                  declare
                     Full_Path : constant String := Full_Name (Item);
                     Zone_Name : constant String :=
                       (if Prefix = "" then Name else Prefix & "/" & Name);
                  begin
                     case Kind (Item) is
                        when Directory =>
                           Scan_Directory (Full_Path, Zone_Name);

                        when Ordinary_File =>
                           if Zone_Name'Length >= Region_Str'Length
                             and then
                               Zone_Name
                                 (Zone_Name'First ..
                                      Zone_Name'First + Region_Str'Length -
                                      1) =
                               Region_Str
                           then
                              Yield
                                (Find_By_Region.Zone_Name_Strings
                                   .To_Bounded_String
                                   (Zone_Name));
                           end if;

                        when others =>
                           null;
                     end case;
                  end;
               end if;
            end;
         end loop;

         End_Search (Search);
      exception
         when Ada.Directories.Name_Error | Ada.Directories.Use_Error =>
            null;
      end Scan_Directory;

   begin
      if Exists (Zoneinfo_Base) and then Kind (Zoneinfo_Base) = Directory then
         Scan_Directory (Zoneinfo_Base);
      end if;

      Result :=
        Find_By_Region.Find_By_Region_Result_Package.Ok
          (TZif.Domain.Value_Object.Unit.Unit);

   exception
      when E : others =>
         Result :=
           Find_By_Region.Find_By_Region_Result_Package.Error
             (TZif.Domain.Error.IO_Error,
              "Error searching region: " & Exception_Message (E));
   end Find_Zones_By_Region;

   ----------------------------------------------------------------------
   --  Find_Zones_By_Regex
   --
   --  Finds timezone IDs matching a regular expression.
   ----------------------------------------------------------------------
   procedure Find_Zones_By_Regex
     (Regex  : TZif.Application.Port.Inbound.Find_By_Regex.Regex_String;
      Yield  : TZif.Application.Port.Inbound.Find_By_Regex
        .Yield_Callback_Access;
      Result : out TZif.Application.Port.Inbound.Find_By_Regex
        .Find_By_Regex_Result)
   is
      use Ada.Directories;
      use Ada.Exceptions;
      use GNAT.Regpat;
      package Find_By_Regex renames
        TZif.Application.Port.Inbound.Find_By_Regex;

      Regex_Str : constant String :=
        Find_By_Regex.Regex_Strings.To_String (Regex);

      procedure Scan_Directory
        (Dir_Path : String; Prefix : String := ""; Pattern : Pattern_Matcher)
      is
         Search : Search_Type;
         pragma Warnings (Off, Search);
         Item : Directory_Entry_Type;
      begin
         Start_Search (Search, Dir_Path, "*");

         while More_Entries (Search) loop
            Get_Next_Entry (Search, Item);

            declare
               Name : constant String := Simple_Name (Item);
            begin
               if Name'Length > 0 and then Name (Name'First) /= '.' then
                  declare
                     Full_Path : constant String := Full_Name (Item);
                     Zone_Name : constant String :=
                       (if Prefix = "" then Name else Prefix & "/" & Name);
                  begin
                     case Kind (Item) is
                        when Directory =>
                           Scan_Directory (Full_Path, Zone_Name, Pattern);

                        when Ordinary_File =>
                           if Match (Pattern, Zone_Name) then
                              Yield
                                (Find_By_Regex.Zone_Name_Strings
                                   .To_Bounded_String
                                   (Zone_Name));
                           end if;

                        when others =>
                           null;
                     end case;
                  end;
               end if;
            end;
         end loop;

         End_Search (Search);
      exception
         when Ada.Directories.Name_Error | Ada.Directories.Use_Error =>
            null;
      end Scan_Directory;

   begin
      declare
         Pattern : constant Pattern_Matcher := Compile (Regex_Str);
      begin
         if Exists (Zoneinfo_Base) and then Kind (Zoneinfo_Base) = Directory
         then
            Scan_Directory (Zoneinfo_Base, "", Pattern);
         end if;

         Result :=
           Find_By_Regex.Find_By_Regex_Result_Package.Ok
             (TZif.Domain.Value_Object.Unit.Unit);
      end;

   exception
      when Expression_Error =>
         Result :=
           Find_By_Regex.Find_By_Regex_Result_Package.Error
             (TZif.Domain.Error.Validation_Error,
              "Invalid regex pattern: " & Regex_Str);
      when E : others       =>
         Result :=
           Find_By_Regex.Find_By_Regex_Result_Package.Error
             (TZif.Domain.Error.IO_Error,
              "Error searching regex: " & Exception_Message (E));
   end Find_Zones_By_Regex;

end TZif.Infrastructure.IO.Desktop;
