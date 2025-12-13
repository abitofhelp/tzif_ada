pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.IO.Embedded
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Embedded platform I/O adapter implementation.
--    Uses environment variables for configuration (no OS-level detection).
--
--  ===========================================================================

with Ada.Streams.Stream_IO;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Characters.Handling;
with Ada.Environment_Variables;
with Functional.Scoped;
with Functional.Try;
with Functional.Try.Map_To_Result;
with Functional.Try.Map_To_Result_With_Param;
with GNAT.Regpat;
with TZif.Domain.Error;
with TZif.Domain.Value_Object.Zone_Id;
with TZif.Domain.Value_Object.Unit;
with TZif.Infrastructure.ULID;

package body TZif.Infrastructure.IO.Embedded with
  SPARK_Mode => Off
is

   use TZif.Domain.Value_Object.Zone_Id;
   use TZif.Domain.Value_Object.Source_Info;

   package SIO renames Ada.Streams.Stream_IO;

   --  ========================================================================
   --  Configuration Constants
   --  ========================================================================

   --  Default timezone if TZIF_SYSTEM_ZONE is not set
   Default_System_Zone : constant String := "UTC";

   --  ========================================================================
   --  Get Zoneinfo Base Path
   --  ========================================================================

   function Get_Zoneinfo_Base return String is
   begin
      if Ada.Environment_Variables.Exists ("TZIF_DATA_PATH") then
         declare
            Path : constant String :=
              Ada.Environment_Variables.Value ("TZIF_DATA_PATH");
         begin
            if Path'Length > 0
              and then Path (Path'Last) /= '/'
              and then Path (Path'Last) /= '\'
            then
               return Path & "/";
            else
               return Path;
            end if;
         end;
      else
         return "";
      end if;
   end Get_Zoneinfo_Base;

   ----------------------------------------------------------------------
   --  Read_File
   --
   --  Implementation:
   --    Uses Functional.Try.Map_To_Result for declarative exception mapping
   --    and Functional.Scoped for automatic file cleanup.
   ----------------------------------------------------------------------
   procedure Read_File
     (Id     :     TZif.Application.Port.Inbound.Find_By_Id.Zone_Id_Input_Type;
      Bytes  : out Byte_Array; Length : out Natural;
      Result : out Read_File_Result.Result)
   is
      Zoneinfo_Base : constant String := Get_Zoneinfo_Base;
      File_Path     : constant String := Zoneinfo_Base & To_String (Id);

      --  Scoped guard for file cleanup
      procedure Close_File (F : in out SIO.File_Type) renames SIO.Close;
      function File_Is_Open (F : SIO.File_Type) return Boolean
        renames SIO.Is_Open;
      package Stream_File_Guard is new Functional.Scoped.Conditional_Guard_For
        (Resource       => SIO.File_Type,
         Should_Release => File_Is_Open,
         Release        => Close_File);

      --  Make_Error for open operation
      function Make_Open_Error
        (Kind : TZif.Domain.Error.Error_Kind; Message : String)
         return Read_File_Result.Result
      is
         use TZif.Domain.Error;
      begin
         case Kind is
            when Not_Found_Error =>
               return Read_File_Result.Error
                 (Not_Found_Error, "Zone file not found: " & Message);
            when others =>
               return Read_File_Result.Error
                 (IO_Error, "File open error for " & File_Path & ": " & Message);
         end case;
      end Make_Open_Error;

      --  Make_Error for read operation
      function Make_Read_Error
        (Kind : TZif.Domain.Error.Error_Kind; Message : String)
         return Read_File_Result.Result
      is
         use TZif.Domain.Error;
      begin
         case Kind is
            when Parse_Error =>
               return Read_File_Result.Error
                 (Parse_Error, "File read error: " & Message);
            when others =>
               return Read_File_Result.Error
                 (IO_Error, "File read error for " & File_Path & ": " & Message);
         end case;
      end Make_Read_Error;

   begin
      Length := 0;

      if Zoneinfo_Base = "" then
         Result :=
           Read_File_Result.Error
             (TZif.Domain.Error.Not_Found_Error,
              "TZIF_DATA_PATH environment variable not set");
         return;
      end if;

      --  Main logic with scoped file guard
      declare
         File   : aliased SIO.File_Type;
         Guard  : Stream_File_Guard.Guard (File'Access);
         pragma Unreferenced (Guard);
         Stream : SIO.Stream_Access;

         --  Raw open action - may raise, returns Result for Map_To_Result
         function Raw_Open return Read_File_Result.Result is
         begin
            SIO.Open (File, SIO.In_File, File_Path);
            return Read_File_Result.Ok ((Bytes_Read => 0));
         end Raw_Open;

         --  Instantiate Map_To_Result for open operation
         package Try_Open is new Functional.Try.Map_To_Result
           (Error_Kind_Type    => TZif.Domain.Error.Error_Kind,
            Result_Type        => Read_File_Result.Result,
            Make_Error         => Make_Open_Error,
            Default_Error_Kind => TZif.Domain.Error.IO_Error,
            Action             => Raw_Open);

         Open_Mappings : constant Try_Open.Mapping_Array :=
           [(SIO.Name_Error'Identity, TZif.Domain.Error.Not_Found_Error),
            (SIO.Use_Error'Identity, TZif.Domain.Error.IO_Error)];

         --  Raw read action - may raise, returns Result for Map_To_Result
         function Raw_Read return Read_File_Result.Result is
         begin
            Stream := SIO.Stream (File);

            while not SIO.End_Of_File (File)
              and then Length < Bytes'Length
            loop
               Length := Length + 1;
               Unsigned_8'Read (Stream, Bytes (Length));
            end loop;

            return Read_File_Result.Ok ((Bytes_Read => Length));
         end Raw_Read;

         --  Instantiate Map_To_Result for read operation
         package Try_Read is new Functional.Try.Map_To_Result
           (Error_Kind_Type    => TZif.Domain.Error.Error_Kind,
            Result_Type        => Read_File_Result.Result,
            Make_Error         => Make_Read_Error,
            Default_Error_Kind => TZif.Domain.Error.IO_Error,
            Action             => Raw_Read);

         Read_Mappings : constant Try_Read.Mapping_Array :=
           [(SIO.End_Error'Identity, TZif.Domain.Error.Parse_Error),
            (SIO.Data_Error'Identity, TZif.Domain.Error.Parse_Error)];

         Open_Result : Read_File_Result.Result;
      begin
         --  Step 1: Open file
         Open_Result := Try_Open.Run (Open_Mappings);
         if not Read_File_Result.Is_Ok (Open_Result) then
            Result := Open_Result;
            return;
         end if;

         --  Step 2: Read file (guard handles close on exit)
         Result := Try_Read.Run (Read_Mappings);
      end;
   end Read_File;

   ----------------------------------------------------------------------
   --  List_Directory_Sources
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

      function Read_Version_From_File (Path : String) return String is
         File   : SIO.File_Type;
         Stream : SIO.Stream_Access;
         Buffer : String (1 .. 64) := [others => ' '];
         Len    : Natural          := 0;
         Ch     : Character;
      begin
         begin
            SIO.Open (File, SIO.In_File, Path);
            Stream := SIO.Stream (File);

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
      end Read_Version_From_File;

      function Count_TZif_Files (Dir_Path : String) return Natural is
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
               if Name = "." or else Name = ".." then
                  null;
               elsif Kind (Item) = Directory then
                  if Name /= "posix"
                    and then Name /= "right"
                    and then Name /= "posixrules"
                  then
                     Count := Count + Count_TZif_Files (Full_Path);
                  end if;
               elsif Kind (Item) = Ordinary_File then
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
         Timestamp := (Y - 2000) * 10_000_000 + Mo * 100_000 + D * 1_000 +
           Natural (Secs / 100.0);

         for I in reverse 1 .. 10 loop
            Idx              := (Timestamp mod 32) + 1;
            Result_Str (I)   := Base32 (Idx);
            Timestamp        := Timestamp / 32;
         end loop;

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
            if not Exists (Dir_Path) then
               Add_Error
                 (TZif.Domain.Error.Not_Found_Error,
                  "Path not found: " & Dir_Path);

            elsif Kind (Dir_Path) /= Directory then
               Add_Error
                 (TZif.Domain.Error.Validation_Error,
                  "Not a directory: " & Dir_Path);

            else
               declare
                  Version_Path1 : constant String := Dir_Path & "/+VERSION";
                  Version_Path2 : constant String := Dir_Path & "/VERSION";
                  Version_Str   : Version_String_Type;
               begin
                  if Exists (Version_Path1) then
                     Version_Str :=
                       Make_Version (Read_Version_From_File (Version_Path1));
                  elsif Exists (Version_Path2) then
                     Version_Str :=
                       Make_Version (Read_Version_From_File (Version_Path2));
                  else
                     Version_Str := Make_Version ("unknown");
                  end if;

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

      Result := Discover.Discovery_Result_Package.Ok (Data);

   end List_Directory_Sources;

   ----------------------------------------------------------------------
   --  Get_Modified_Time
   ----------------------------------------------------------------------
   procedure Get_Modified_Time
     (Id        : TZif.Application.Port.Inbound.Find_By_Id.Zone_Id_Input_Type;
      Timestamp : out Timestamp_Type;
      Result    : out Get_Modified_Time_Result.Result)
   is
      use Ada.Calendar;
   begin
      pragma Unreferenced (Id);
      Timestamp := Clock;
      Result    := Get_Modified_Time_Result.Ok (Timestamp);
   end Get_Modified_Time;

   ----------------------------------------------------------------------
   --  Read_Version_File
   --
   --  Implementation:
   --    Uses Functional.Try.Map_To_Result for declarative exception mapping
   --    and Functional.Scoped for automatic file cleanup.
   ----------------------------------------------------------------------
   procedure Read_Version_File
     (Source : TZif.Domain.Value_Object.Source_Info.Source_Info_Type;
      Result : out TZif.Application.Port.Inbound.Get_Version.Version_Result)
   is
      use Ada.Directories;
      package Get_Version renames TZif.Application.Port.Inbound.Get_Version;

      Path_Str     : constant String :=
        TZif.Domain.Value_Object.Source_Info.To_String
          (TZif.Domain.Value_Object.Source_Info.Get_Path (Source));
      Version_File : constant String := Path_Str & "/+VERSION";

      --  Scoped guard for file cleanup
      procedure Close_File (F : in out SIO.File_Type) renames SIO.Close;
      function File_Is_Open (F : SIO.File_Type) return Boolean
        renames SIO.Is_Open;
      package Version_File_Guard is new Functional.Scoped.Conditional_Guard_For
        (Resource       => SIO.File_Type,
         Should_Release => File_Is_Open,
         Release        => Close_File);

      --  Raw action that may raise - returns Result type for Map_To_Result
      function Raw_Read_Version_File return Get_Version.Version_Result is
         File   : aliased SIO.File_Type;
         Guard  : Version_File_Guard.Guard (File'Access);
         pragma Unreferenced (Guard);  --  RAII: releases via Finalize
         Stream : SIO.Stream_Access;
         Buffer : String (1 .. 32) := [others => ' '];
         Len    : Natural          := 0;
         Ch     : Character;
      begin
         SIO.Open (File, SIO.In_File, Version_File);
         Stream := SIO.Stream (File);

         while not SIO.End_Of_File (File) and then Len < Buffer'Last loop
            Character'Read (Stream, Ch);
            exit when Ch = ASCII.LF or else Ch = ASCII.CR;
            Len          := Len + 1;
            Buffer (Len) := Ch;
         end loop;

         --  Guard.Finalize will close file automatically
         return Get_Version.Version_Result_Package.Ok
           (Get_Version.Version_Strings.To_Bounded_String
              (Ada.Strings.Fixed.Trim (Buffer (1 .. Len), Ada.Strings.Both)));
      end Raw_Read_Version_File;

      --  Make_Error for version file reading
      function Make_Version_Error
        (Kind : TZif.Domain.Error.Error_Kind; Message : String)
         return Get_Version.Version_Result
      is
         use TZif.Domain.Error;
      begin
         case Kind is
            when Not_Found_Error =>
               return Get_Version.Version_Result_Package.Error
                 (Not_Found_Error, "Version file not found: " & Message);
            when others =>
               return Get_Version.Version_Result_Package.Error
                 (IO_Error, "Error reading version: " & Message);
         end case;
      end Make_Version_Error;

      --  Instantiate Functional.Try.Map_To_Result for version file reading
      package Try_Read_Version is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => TZif.Domain.Error.Error_Kind,
         Result_Type        => Get_Version.Version_Result,
         Make_Error         => Make_Version_Error,
         Default_Error_Kind => TZif.Domain.Error.IO_Error,
         Action             => Raw_Read_Version_File);

      Version_Mappings : constant Try_Read_Version.Mapping_Array :=
        [(SIO.Name_Error'Identity, TZif.Domain.Error.Not_Found_Error),
         (SIO.Use_Error'Identity,  TZif.Domain.Error.IO_Error)];

   begin
      --  Check if file exists first (avoids exception for common case)
      if not Exists (Version_File) or else Kind (Version_File) /= Ordinary_File
      then
         Result :=
           Get_Version.Version_Result_Package.Error
             (TZif.Domain.Error.Not_Found_Error,
              "Version file not found: " & Version_File);
         return;
      end if;

      Result := Try_Read_Version.Run (Version_Mappings);
   end Read_Version_File;

   ----------------------------------------------------------------------
   --  Read_System_Timezone_Id
   --
   --  Embedded implementation: Uses TZIF_SYSTEM_ZONE environment variable
   --  or returns UTC as default.
   --
   --  Implementation:
   --    Uses Functional.Try.Map_To_Result for declarative exception mapping.
   --    On any error, returns UTC as a sensible default for embedded systems.
   ----------------------------------------------------------------------
   procedure Read_System_Timezone_Id
     (Result : out TZif.Application.Port.Inbound.Find_My_Id.Result)
   is
      package Find_My_Id renames TZif.Application.Port.Inbound.Find_My_Id;

      --  Raw action that may raise
      function Raw_Read_System_Zone return Find_My_Id.Result is
      begin
         --  Check for TZIF_SYSTEM_ZONE environment variable
         if Ada.Environment_Variables.Exists ("TZIF_SYSTEM_ZONE") then
            declare
               Zone_Id_Str : constant String :=
                 Ada.Environment_Variables.Value ("TZIF_SYSTEM_ZONE");
            begin
               return Find_My_Id.Result_Zone_Id.Ok (Make_Zone_Id (Zone_Id_Str));
            end;
         else
            --  Return default UTC if not configured
            return
              Find_My_Id.Result_Zone_Id.Ok (Make_Zone_Id (Default_System_Zone));
         end if;
      end Raw_Read_System_Zone;

      --  Make_Error: On any error, return UTC as sensible default
      function Make_Syszone_Error
        (Kind : TZif.Domain.Error.Error_Kind; Message : String)
         return Find_My_Id.Result
      is
         pragma Unreferenced (Kind, Message);
      begin
         --  Embedded semantics: always succeed with UTC fallback
         return
           Find_My_Id.Result_Zone_Id.Ok (Make_Zone_Id (Default_System_Zone));
      end Make_Syszone_Error;

      --  Instantiate Functional.Try.Map_To_Result
      package Try_Read_Syszone is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => TZif.Domain.Error.Error_Kind,
         Result_Type        => Find_My_Id.Result,
         Make_Error         => Make_Syszone_Error,
         Default_Error_Kind => TZif.Domain.Error.IO_Error,
         Action             => Raw_Read_System_Zone);

      --  Empty mappings - all errors return UTC via Make_Syszone_Error
      Syszone_Mappings : constant Try_Read_Syszone.Mapping_Array :=
        Try_Read_Syszone.Empty_Mappings;

   begin
      Result := Try_Read_Syszone.Run (Syszone_Mappings);
   end Read_System_Timezone_Id;

   ----------------------------------------------------------------------
   --  List_Zones_In_Source
   --
   --  Implementation:
   --    Uses Functional.Try.Map_To_Result_With_Param for declarative mapping.
   --    Inner exception handlers for intentional error recovery are preserved.
   ----------------------------------------------------------------------
   procedure List_Zones_In_Source
     (Source     : TZif.Domain.Value_Object.Source_Info.Source_Info_Type;
      Descending : Boolean;
      Result     : out TZif.Application.Port.Inbound.List_All_Order_By_Id
        .List_All_Zones_Result)
   is
      use Ada.Directories;
      package List_All renames
        TZif.Application.Port.Inbound.List_All_Order_By_Id;

      Path_Str : constant String :=
        TZif.Domain.Value_Object.Source_Info.To_String
          (TZif.Domain.Value_Object.Source_Info.Get_Path (Source));

      --  Context for raw action
      type Scan_Context is record
         Descending : Boolean;
      end record;

      --  Raw action that may raise - returns Result type for Map_To_Result
      function Raw_Scan_And_Sort
        (Ctx : Scan_Context) return List_All.List_All_Zones_Result
      is
         Zones : List_All.Zone_Id_List;

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
                                       --  DESIGN DECISION: Skip zone names exceeding
                                       --  bounded string limit during batch scan.
                                       --  Functional.Try not applicable here as
                                       --  we need continue-on-error semantics.
                                       null;
                                 end;
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
               --  Skip inaccessible directories silently (intentional)
               null;
         end Scan_Directory;

         function Less_Than (Left, Right : Zone_Id_Type) return Boolean is
           (To_String (Left) < To_String (Right));

         procedure Sort_Zones is new List_All.Zone_Id_Vectors.Generic_Sort
           ("<" => Less_Than);

      begin
         Scan_Directory (Path_Str);
         Sort_Zones (Zones);
         if Ctx.Descending then
            List_All.Zone_Id_Vectors.Reverse_Elements (Zones);
         end if;
         return List_All.List_All_Zones_Result_Package.Ok (Zones);
      end Raw_Scan_And_Sort;

      --  Make_Error for list zones operation
      function Make_Listzones_Error
        (Kind : TZif.Domain.Error.Error_Kind; Message : String)
         return List_All.List_All_Zones_Result
      is
         use TZif.Domain.Error;
      begin
         case Kind is
            when Not_Found_Error =>
               return List_All.List_All_Zones_Result_Package.Error
                 (Not_Found_Error, "Source not found: " & Message);
            when others =>
               return List_All.List_All_Zones_Result_Package.Error
                 (IO_Error, "Error listing zones: " & Message);
         end case;
      end Make_Listzones_Error;

      --  Instantiate Functional.Try.Map_To_Result_With_Param
      package Try_Scan_And_Sort is new
        Functional.Try.Map_To_Result_With_Param
          (Error_Kind_Type    => TZif.Domain.Error.Error_Kind,
           Param_Type         => Scan_Context,
           Result_Type        => List_All.List_All_Zones_Result,
           Make_Error         => Make_Listzones_Error,
           Default_Error_Kind => TZif.Domain.Error.IO_Error,
           Action             => Raw_Scan_And_Sort);

      Listzones_Mappings : constant Try_Scan_And_Sort.Mapping_Array :=
        [(Ada.Directories.Name_Error'Identity,
          TZif.Domain.Error.Not_Found_Error),
         (Ada.Directories.Use_Error'Identity, TZif.Domain.Error.IO_Error)];

   begin
      if not Exists (Path_Str) or else Kind (Path_Str) /= Directory then
         Result :=
           List_All.List_All_Zones_Result_Package.Error
             (TZif.Domain.Error.Not_Found_Error,
              "Source path not found: " & Path_Str);
         return;
      end if;

      Result := Try_Scan_And_Sort.Run ((Descending => Descending),
                                       Listzones_Mappings);
   end List_Zones_In_Source;

   ----------------------------------------------------------------------
   --  Load_Source_From_Path
   --
   --  Implementation:
   --    Uses Functional.Try.Map_To_Result for declarative exception mapping
   --    and Functional.Scoped for automatic file cleanup.
   ----------------------------------------------------------------------
   procedure Load_Source_From_Path
     (Path   : TZif.Application.Port.Inbound.Load_Source.Path_String;
      Result : out TZif.Application.Port.Inbound.Load_Source
        .Load_Source_Result)
   is
      use Ada.Directories;
      use Ada.Text_IO;
      package Load_Source renames TZif.Application.Port.Inbound.Load_Source;

      --  Scoped guard for Text_IO file cleanup
      package Text_File_Guard is new Functional.Scoped.Conditional_Guard_For
        (Resource       => File_Type,
         Should_Release => Is_Open,
         Release        => Close);

      Path_Str : constant String :=
        Load_Source.Path_Strings.To_String (Path);

      --  Raw action that may raise - returns Result type for Map_To_Result
      function Raw_Load_Source return Load_Source.Load_Source_Result is
         ULID         : constant ULID_Type        :=
           TZif.Infrastructure.ULID.Generate;
         Path_Val     : constant Path_String_Type := Make_Path (Path_Str);
         Version_File : constant String           := Path_Str & "/+VERSION";
         Version_Str  : String (1 .. 32);
         Last         : Natural;
         File         : aliased File_Type;
         Guard        : Text_File_Guard.Guard (File'Access);
         pragma Unreferenced (Guard);  --  RAII: releases via Finalize

         Zone_Count   : Natural := 0;

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
                  --  Skip inaccessible directories (intentional)
                  null;
            end Count_Recursive;

         begin
            Count_Recursive (Dir_Path);
         end Count_Zones;

         Version : Version_String_Type;
      begin
         if Exists (Version_File) and then Kind (Version_File) = Ordinary_File
         then
            Open (File, In_File, Version_File);
            Get_Line (File, Version_Str, Last);
            --  Guard.Finalize will close file automatically
         else
            Version_Str (1 .. 7) := "unknown";
            Last                 := 7;
         end if;

         Version := Make_Version (Version_Str (1 .. Last));
         Count_Zones (Path_Str);
         return Load_Source.Load_Source_Result_Package.Ok
           (Make_Source_Info (ULID, Path_Val, Version, Zone_Count));
      end Raw_Load_Source;

      --  Make_Error for load source operation
      function Make_Loadsource_Error
        (Kind : TZif.Domain.Error.Error_Kind; Message : String)
         return Load_Source.Load_Source_Result
      is
         use TZif.Domain.Error;
      begin
         case Kind is
            when Not_Found_Error =>
               return Load_Source.Load_Source_Result_Package.Error
                 (Not_Found_Error, "Source path not found: " & Message);
            when others =>
               return Load_Source.Load_Source_Result_Package.Error
                 (IO_Error, "Error loading source: " & Message);
         end case;
      end Make_Loadsource_Error;

      --  Instantiate Functional.Try.Map_To_Result
      package Try_Load_Source is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => TZif.Domain.Error.Error_Kind,
         Result_Type        => Load_Source.Load_Source_Result,
         Make_Error         => Make_Loadsource_Error,
         Default_Error_Kind => TZif.Domain.Error.IO_Error,
         Action             => Raw_Load_Source);

      Loadsource_Mappings : constant Try_Load_Source.Mapping_Array :=
        [(Ada.Directories.Name_Error'Identity,
          TZif.Domain.Error.Not_Found_Error),
         (Ada.Directories.Use_Error'Identity, TZif.Domain.Error.IO_Error)];

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

      Result := Try_Load_Source.Run (Loadsource_Mappings);
   end Load_Source_From_Path;

   ----------------------------------------------------------------------
   --  Validate_Source_Path
   --
   --  Implementation:
   --    Uses Functional.Try.Map_To_Result for declarative exception mapping.
   ----------------------------------------------------------------------
   procedure Validate_Source_Path
     (Path   : TZif.Application.Port.Inbound.Validate_Source.Path_String;
      Result : out TZif.Application.Port.Inbound.Validate_Source
        .Validation_Result)
   is
      use Ada.Directories;
      package Validate_Source renames
        TZif.Application.Port.Inbound.Validate_Source;

      Path_Str : constant String :=
        Validate_Source.Path_Strings.To_String (Path);

      --  Raw action that may raise - returns Result type for Map_To_Result
      function Raw_Validate return Validate_Source.Validation_Result is
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
         return Validate_Source.Validation_Result_Package.Ok (Found_TZif);
      exception
         when Name_Error | Use_Error =>
            --  Inaccessible directory = not valid (intentional)
            return Validate_Source.Validation_Result_Package.Ok (False);
      end Raw_Validate;

      --  Make_Error for validation operation
      function Make_Validate_Error
        (Kind : TZif.Domain.Error.Error_Kind; Message : String)
         return Validate_Source.Validation_Result
      is
         pragma Unreferenced (Kind, Message);
      begin
         --  Validation errors should return Ok(False), not Err
         return Validate_Source.Validation_Result_Package.Ok (False);
      end Make_Validate_Error;

      --  Instantiate Functional.Try.Map_To_Result
      package Try_Validate is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => TZif.Domain.Error.Error_Kind,
         Result_Type        => Validate_Source.Validation_Result,
         Make_Error         => Make_Validate_Error,
         Default_Error_Kind => TZif.Domain.Error.IO_Error,
         Action             => Raw_Validate);

      --  Empty mappings since we handle errors internally by returning False
      Validate_Mappings : constant Try_Validate.Mapping_Array :=
        Try_Validate.Empty_Mappings;

   begin
      if not Exists (Path_Str) then
         Result := Validate_Source.Validation_Result_Package.Ok (False);
         return;
      end if;

      if Kind (Path_Str) /= Directory then
         Result := Validate_Source.Validation_Result_Package.Ok (False);
         return;
      end if;

      Result := Try_Validate.Run (Validate_Mappings);
   end Validate_Source_Path;

   ----------------------------------------------------------------------
   --  Find_Zones_By_Pattern
   --
   --  Implementation:
   --    Uses Functional.Try.Map_To_Result for declarative exception mapping.
   --    Inner exception handler for inaccessible directories preserved.
   ----------------------------------------------------------------------
   procedure Find_Zones_By_Pattern
     (Pattern : TZif.Application.Port.Inbound.Find_By_Pattern.Pattern_String;
      Yield   : TZif.Application.Port.Inbound.Find_By_Pattern
        .Yield_Callback_Access;
      Result  : out TZif.Application.Port.Inbound.Find_By_Pattern
        .Find_By_Pattern_Result)
   is
      use Ada.Directories;
      package Find_By_Pattern renames
        TZif.Application.Port.Inbound.Find_By_Pattern;

      Pattern_Str   : constant String :=
        Find_By_Pattern.Pattern_Strings.To_String (Pattern);
      Zoneinfo_Base : constant String := Get_Zoneinfo_Base;

      --  Raw action that may raise - returns Result type for Map_To_Result
      function Raw_Scan return Find_By_Pattern.Find_By_Pattern_Result is

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
                                   Ada.Characters.Handling.To_Lower
                                     (Pattern_Str);
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
               --  Skip inaccessible directories (intentional)
               null;
         end Scan_Directory;

      begin
         if Zoneinfo_Base /= ""
           and then Exists (Zoneinfo_Base)
           and then Kind (Zoneinfo_Base) = Directory
         then
            Scan_Directory (Zoneinfo_Base);
         end if;
         return Find_By_Pattern.Find_By_Pattern_Result_Package.Ok
           (TZif.Domain.Value_Object.Unit.Unit);
      end Raw_Scan;

      --  Make_Error for pattern search operation
      function Make_Pattern_Error
        (Kind : TZif.Domain.Error.Error_Kind; Message : String)
         return Find_By_Pattern.Find_By_Pattern_Result
      is
         use TZif.Domain.Error;
      begin
         case Kind is
            when Not_Found_Error =>
               return Find_By_Pattern.Find_By_Pattern_Result_Package.Error
                 (Not_Found_Error, "Source not found: " & Message);
            when others =>
               return Find_By_Pattern.Find_By_Pattern_Result_Package.Error
                 (IO_Error, "Error searching pattern: " & Message);
         end case;
      end Make_Pattern_Error;

      --  Instantiate Functional.Try.Map_To_Result
      package Try_Scan is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => TZif.Domain.Error.Error_Kind,
         Result_Type        => Find_By_Pattern.Find_By_Pattern_Result,
         Make_Error         => Make_Pattern_Error,
         Default_Error_Kind => TZif.Domain.Error.IO_Error,
         Action             => Raw_Scan);

      Pattern_Mappings : constant Try_Scan.Mapping_Array :=
        [(Ada.Directories.Name_Error'Identity,
          TZif.Domain.Error.Not_Found_Error),
         (Ada.Directories.Use_Error'Identity, TZif.Domain.Error.IO_Error)];

   begin
      Result := Try_Scan.Run (Pattern_Mappings);
   end Find_Zones_By_Pattern;

   ----------------------------------------------------------------------
   --  Find_Zones_By_Region
   --
   --  Implementation:
   --    Uses Functional.Try.Map_To_Result for declarative exception mapping.
   --    Inner exception handler for inaccessible directories preserved.
   ----------------------------------------------------------------------
   procedure Find_Zones_By_Region
     (Region : TZif.Application.Port.Inbound.Find_By_Region.Region_String;
      Yield  : TZif.Application.Port.Inbound.Find_By_Region
        .Yield_Callback_Access;
      Result : out TZif.Application.Port.Inbound.Find_By_Region
        .Find_By_Region_Result)
   is
      use Ada.Directories;
      package Find_By_Region renames
        TZif.Application.Port.Inbound.Find_By_Region;

      Region_Str    : constant String :=
        Find_By_Region.Region_Strings.To_String (Region);
      Zoneinfo_Base : constant String := Get_Zoneinfo_Base;

      --  Raw action that may raise - returns Result type for Map_To_Result
      function Raw_Scan return Find_By_Region.Find_By_Region_Result is

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
               --  Skip inaccessible directories (intentional)
               null;
         end Scan_Directory;

      begin
         if Zoneinfo_Base /= ""
           and then Exists (Zoneinfo_Base)
           and then Kind (Zoneinfo_Base) = Directory
         then
            Scan_Directory (Zoneinfo_Base);
         end if;
         return Find_By_Region.Find_By_Region_Result_Package.Ok
           (TZif.Domain.Value_Object.Unit.Unit);
      end Raw_Scan;

      --  Make_Error for region search operation
      function Make_Region_Error
        (Kind : TZif.Domain.Error.Error_Kind; Message : String)
         return Find_By_Region.Find_By_Region_Result
      is
         use TZif.Domain.Error;
      begin
         case Kind is
            when Not_Found_Error =>
               return Find_By_Region.Find_By_Region_Result_Package.Error
                 (Not_Found_Error, "Source not found: " & Message);
            when others =>
               return Find_By_Region.Find_By_Region_Result_Package.Error
                 (IO_Error, "Error searching region: " & Message);
         end case;
      end Make_Region_Error;

      --  Instantiate Functional.Try.Map_To_Result
      package Try_Scan is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => TZif.Domain.Error.Error_Kind,
         Result_Type        => Find_By_Region.Find_By_Region_Result,
         Make_Error         => Make_Region_Error,
         Default_Error_Kind => TZif.Domain.Error.IO_Error,
         Action             => Raw_Scan);

      Region_Mappings : constant Try_Scan.Mapping_Array :=
        [(Ada.Directories.Name_Error'Identity,
          TZif.Domain.Error.Not_Found_Error),
         (Ada.Directories.Use_Error'Identity, TZif.Domain.Error.IO_Error)];

   begin
      Result := Try_Scan.Run (Region_Mappings);
   end Find_Zones_By_Region;

   ----------------------------------------------------------------------
   --  Find_Zones_By_Regex
   --
   --  Implementation:
   --    Uses Functional.Try.Map_To_Result for declarative exception mapping.
   --    Maps GNAT.Regpat.Expression_Error to Validation_Error for invalid regex.
   ----------------------------------------------------------------------
   procedure Find_Zones_By_Regex
     (Regex  : TZif.Application.Port.Inbound.Find_By_Regex.Regex_String;
      Yield  : TZif.Application.Port.Inbound.Find_By_Regex
        .Yield_Callback_Access;
      Result : out TZif.Application.Port.Inbound.Find_By_Regex
        .Find_By_Regex_Result)
   is
      use Ada.Directories;
      use GNAT.Regpat;
      package Find_By_Regex renames
        TZif.Application.Port.Inbound.Find_By_Regex;

      Regex_Str     : constant String :=
        Find_By_Regex.Regex_Strings.To_String (Regex);
      Zoneinfo_Base : constant String := Get_Zoneinfo_Base;

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
            --  Skip inaccessible directories (intentional)
            null;
      end Scan_Directory;

      --  Raw function that may raise - returns Result type for Map_To_Result
      function Raw_Scan return Find_By_Regex.Find_By_Regex_Result is
         Pattern : constant Pattern_Matcher := Compile (Regex_Str);
      begin
         if Zoneinfo_Base /= ""
           and then Exists (Zoneinfo_Base)
           and then Kind (Zoneinfo_Base) = Directory
         then
            Scan_Directory (Zoneinfo_Base, "", Pattern);
         end if;
         return Find_By_Regex.Find_By_Regex_Result_Package.Ok
           (TZif.Domain.Value_Object.Unit.Unit);
      end Raw_Scan;

      --  Make_Error for regex search operation
      function Make_Regex_Error
        (Kind : TZif.Domain.Error.Error_Kind; Message : String)
         return Find_By_Regex.Find_By_Regex_Result
      is
         use TZif.Domain.Error;
      begin
         case Kind is
            when Validation_Error =>
               return Find_By_Regex.Find_By_Regex_Result_Package.Error
                 (Validation_Error, "Invalid regex pattern: " & Regex_Str);
            when Not_Found_Error =>
               return Find_By_Regex.Find_By_Regex_Result_Package.Error
                 (Not_Found_Error, "Directory not found: " & Message);
            when others =>
               return Find_By_Regex.Find_By_Regex_Result_Package.Error
                 (IO_Error, "Error searching regex: " & Message);
         end case;
      end Make_Regex_Error;

      --  Instantiate Functional.Try.Map_To_Result with regex-aware mapping
      package Try_Scan is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => TZif.Domain.Error.Error_Kind,
         Result_Type        => Find_By_Regex.Find_By_Regex_Result,
         Make_Error         => Make_Regex_Error,
         Default_Error_Kind => TZif.Domain.Error.IO_Error,
         Action             => Raw_Scan);

      Regex_Mappings : constant Try_Scan.Mapping_Array :=
        [(Expression_Error'Identity, TZif.Domain.Error.Validation_Error),
         (Ada.Directories.Name_Error'Identity,
          TZif.Domain.Error.Not_Found_Error),
         (Ada.Directories.Use_Error'Identity, TZif.Domain.Error.IO_Error)];

   begin
      Result := Try_Scan.Run (Regex_Mappings);
   end Find_Zones_By_Regex;

end TZif.Infrastructure.IO.Embedded;
