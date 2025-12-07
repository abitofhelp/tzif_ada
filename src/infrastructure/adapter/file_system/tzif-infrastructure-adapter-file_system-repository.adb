pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.Adapter.File_System.Repository
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Repository infrastructure adapter.
--
--  Architecture:
--    Infrastructure layer adapter (hexagonal architecture).
--    Implements outbound ports for external systems.
--
--  ===========================================================================

with Ada.Directories;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with GNAT.Regpat;
with TZif.Infrastructure.TZif_Parser;
with TZif.Infrastructure.ULID;
with TZif.Infrastructure.CPU;
with TZif.Infrastructure.Cache.Source_Cache;
with TZif.Infrastructure.Cache.Zone_Cache;
with TZif.Domain.Error;
with TZif.Domain.TZif_Data;

--   Import all inbound ports for canonical Result types (GPT-5 pattern)
with TZif.Domain.Entity.Zone;
with TZif.Domain.Value_Object.Zone_Id;
with TZif.Domain.Value_Object.Timezone_Type;
with TZif.Domain.Value_Object.Transition_Info;
with TZif.Domain.Value_Object.Unit;

package body TZif.Infrastructure.Adapter.File_System.Repository is

   use TZif.Domain.Error;
   use TZif.Domain.TZif_Data;
   use TZif.Domain.Entity.Zone;
   use TZif.Domain.Value_Object.Zone_Id;

   --  ========================================================================
   --  Standard Search Paths
   --  ========================================================================

   type Path_Array is array (Positive range <>) of access constant String;

   Z1 : aliased constant String := "/usr/share/zoneinfo";
   Z2 : aliased constant String := "/var/db/timezone/zoneinfo";
   Z3 : aliased constant String := "/usr/lib/zoneinfo";
   Z4 : aliased constant String := "/etc/zoneinfo";

   Search_Paths : constant Path_Array :=
     [Z1'Access, Z2'Access, Z3'Access, Z4'Access];

   --  ========================================================================
   --  Package-Level Caches (Thread-Safe)
   --  ========================================================================

   Sources : TZif.Infrastructure.Cache.Source_Cache.Source_Cache_Type;
   pragma Unreferenced (Sources);
   --  Thread-safe cache for timezone source metadata
   --  FUTURE: Will be used by discover_sources, load_source operations

   Zones : TZif.Infrastructure.Cache.Zone_Cache.Zone_Cache_Type;
   --  Thread-safe cache for parsed timezone data (TZif_Data)
   --  Populated when zones are loaded via Find_By_Id

   --  ========================================================================
   --  Helper: Find TZif File
   --  ========================================================================

   function Find_TZif_File (Zone_Id : String) return Path_String_Option is
      use Ada.Directories;
   begin
      for Path of Search_Paths loop
         declare
            Full_Path : constant String := Path.all & "/" & Zone_Id;
         begin
            if Exists (Full_Path) and then Kind (Full_Path) = Ordinary_File
            then
               return Path_String_Options.New_Some
                 (Path_Strings.To_Bounded_String (Full_Path));
            end if;
         end;
      end loop;
      return Path_String_Options.None;
   end Find_TZif_File;

   --  ========================================================================
   --  Helper: Bounded String Conversions
   --  ========================================================================

   function To_String (S : Zone_Id_String) return String is
   begin
      return Zone_Id_Strings.To_String (S);
   end To_String;

   --  ========================================================================
   --  1. Find_By_Id (GPT-5 Pattern: Uses port's canonical Result type)
   --  ========================================================================

   function Find_By_Id
     (Id : Application.Port.Inbound.Find_By_Id.Zone_Id_Input_Type)
      return Application.Port.Inbound.Find_By_Id.Find_By_Id_Result_Type
   is
      use TZif.Application.Port.Inbound.Find_By_Id;
      Zone_Id_Str    : constant String := To_String (Id);
      File_Path_Opt  : constant Path_String_Option :=
        Find_TZif_File (Zone_Id_Str);
   begin
      if Path_String_Options.Is_None (File_Path_Opt) then
         return
           Find_By_Id_Result.Error
             (Not_Found_Error, "Zone not found: " & Zone_Id_Str);
      end if;

      declare
         File_Path    : constant String :=
           Path_Strings.To_String (Path_String_Options.Value (File_Path_Opt));
         Parse_Result :
           constant Infrastructure.TZif_Parser.Parse_Result_Type :=
           Infrastructure.TZif_Parser.Parse_From_File (File_Path);
      begin
         if not Infrastructure.TZif_Parser.Parse_Result.Is_Ok (Parse_Result)
         then
            declare
               Err : constant Error_Type :=
                 Infrastructure.TZif_Parser.Parse_Result.Error_Info
                   (Parse_Result);
            begin
               return
                 Find_By_Id_Result.Error
                   (Err.Kind,
                    "Parse failed for " & Zone_Id_Str & ": " &
                    Error_Strings.To_String (Err.Message));
            end;
         end if;

         declare
            TZif_Data : constant TZif_Data_Type :=
              Infrastructure.TZif_Parser.Parse_Result.Value (Parse_Result);
            --  Id is already Zone_Id_Type (from port), use it directly
            Zone      : constant Zone_Type      :=
              Make_Zone (Id => Id, Data => TZif_Data);
         begin
            --  Cache the parsed zone data for export/import
            Zones.Insert (Id, TZif_Data);
            return Find_By_Id_Result.Ok (Zone);
         end;
      end;
   exception
      when E : others =>
         return
           Find_By_Id_Result.Error
             (IO_Error,
              "Unexpected error: " & Ada.Exceptions.Exception_Message (E));
   end Find_By_Id;

   --  ========================================================================
   --  2. Exists_By_Id
   --  ========================================================================

   function Exists_By_Id (Id : Zone_Id_String) return Exists_Result is
      Zone_Id_Str   : constant String := To_String (Id);
      File_Path_Opt : constant Path_String_Option :=
        Find_TZif_File (Zone_Id_Str);
   begin
      return Boolean_Result.Ok (Path_String_Options.Is_Some (File_Path_Opt));
   exception
      when E : others =>
         return
           Boolean_Result.Error
             (IO_Error,
              "Error checking existence: " &
              Ada.Exceptions.Exception_Message (E));
   end Exists_By_Id;

   --  ===================================================================
   --  3. Get_Transition_At_Epoch (GPT-5 Pattern: Uses port's canonical
   --     Result type)
   --  ===================================================================

   function Get_Transition_At_Epoch
     (Id    : TZif.Application.Port.Inbound.Get_Transition_At_Epoch
        .Zone_Id_String;
      Epoch : Epoch_Seconds_Type)
      return Application.Port.Inbound.Get_Transition_At_Epoch
     .Get_Transition_Result
   is
      use TZif.Application.Port.Inbound.Get_Transition_At_Epoch;
      Zone_Id_Str   : constant String :=
        Application.Port.Inbound.Get_Transition_At_Epoch.Zone_Id_Strings
          .To_String
          (Id);
      File_Path_Opt : constant Path_String_Option :=
        Find_TZif_File (Zone_Id_Str);
   begin
      if Path_String_Options.Is_None (File_Path_Opt) then
         return
           Get_Transition_Result_Package.Error
             (Not_Found_Error, "Zone not found: " & Zone_Id_Str);
      end if;

      declare
         File_Path    : constant String :=
           Path_Strings.To_String (Path_String_Options.Value (File_Path_Opt));
         Parse_Result :
           constant Infrastructure.TZif_Parser.Parse_Result_Type :=
           Infrastructure.TZif_Parser.Parse_From_File (File_Path);
      begin
         if not Infrastructure.TZif_Parser.Parse_Result.Is_Ok (Parse_Result)
         then
            declare
               Err : constant Error_Type :=
                 Infrastructure.TZif_Parser.Parse_Result.Error_Info
                   (Parse_Result);
            begin
               return
                 Get_Transition_Result_Package.Error
                   (Err.Kind,
                    "Parse failed: " & Error_Strings.To_String (Err.Message));
            end;
         end if;

         declare
            use TZif.Domain.Value_Object.Timezone_Type;
            use TZif.Domain.TZif_Data.Type_Index_Options;
            TZif_Data    : constant TZif_Data_Type :=
              Infrastructure.TZif_Parser.Parse_Result.Value (Parse_Result);
            --  Find_Type_At_Time returns Option; check if found
            Type_Index_Opt : constant Type_Index_Option :=
              Find_Type_At_Time (TZif_Data, Epoch);
            Tz_Length    : constant Natural       :=
              Timezone_Type_Vectors.Length (TZif_Data.Timezone_Types);
         begin
            --  Validate before accessing
            if Tz_Length = 0 then
               return
                 Get_Transition_Result_Package.Error
                   (Parse_Error, "No timezone types in zone file");
            elsif Is_None (Type_Index_Opt) then
               return
                 Get_Transition_Result_Package.Error
                   (Parse_Error, "No timezone type found for given time");
            end if;

            declare
               --  Note: TZif uses 0-based indices; Get_Type expects 0-based
               --  and adds 1 internally for vector access
               Type_Index : constant Natural := Value (Type_Index_Opt);
            begin
               if Type_Index >= Tz_Length then
                  return
                    Get_Transition_Result_Package.Error
                      (Parse_Error, "Invalid type index in zone file");
               end if;

               declare
                  TZ_Type : constant Timezone_Type_Record :=
                    Get_Type (TZif_Data, Type_Index);
                  Info    :
                    constant Domain.Value_Object.Transition_Info
                      .Transition_Info_Type :=
                    Domain.Value_Object.Transition_Info.Make_Transition_Info
                      (Epoch_Time   => Epoch, UTC_Offset => TZ_Type.UTC_Offset,
                       Is_DST       => TZ_Type.Is_DST,
                       Abbreviation => Get_Abbreviation (TZ_Type));
               begin
                  return Get_Transition_Result_Package.Ok (Info);
               end;
            end;
         end;
      end;
   exception
      when E : others =>
         return
           Get_Transition_Result_Package.Error
             (IO_Error,
              "Unexpected error: " & Ada.Exceptions.Exception_Message (E));
   end Get_Transition_At_Epoch;

   --  ========================================================================
   --  4. Get_Version
   --  ========================================================================

   function Get_Version
     (Source : Source_Info_Type)
      return Application.Port.Inbound.Get_Version.Version_Result
   is
      use Ada.Directories;
      use Ada.Text_IO;
      use TZif.Application.Port.Inbound.Get_Version;

      Path_Str     : constant String := To_String (Get_Path (Source));
      Version_File : constant String := Path_Str & "/+VERSION";
   begin
      if not Exists (Version_File) or else Kind (Version_File) /= Ordinary_File
      then
         return
           Version_Result_Package.Error
             (Not_Found_Error, "Version file not found: " & Version_File);
      end if;

      declare
         File : File_Type;
         Line : String (1 .. 32);
         Last : Natural;
      begin
         Open (File, In_File, Version_File);
         Get_Line (File, Line, Last);
         Close (File);
         return
           Version_Result_Package.Ok
             (Application.Port.Inbound.Get_Version.Version_Strings
                .To_Bounded_String
                (Line (1 .. Last)));
      exception
         when E : others =>
            if Is_Open (File) then
               Close (File);
            end if;
            return
              Version_Result_Package.Error
                (IO_Error,
                 "Error reading version: " &
                 Ada.Exceptions.Exception_Message (E));
      end;
   exception
      when E : others =>
         return
           Version_Result_Package.Error
             (IO_Error,
              "Unexpected error: " & Ada.Exceptions.Exception_Message (E));
   end Get_Version;

   --  ========================================================================
   --  5. Find_My_Id (GPT-5 Pattern: Uses port's canonical Result type)
   --  ========================================================================

   function Find_My_Id return Application.Port.Inbound.Find_My_Id.Result is
      use Ada.Directories;
      use TZif.Application.Port.Inbound.Find_My_Id;
      Localtime_Path : constant String := "/etc/localtime";
   begin
      if not Exists (Localtime_Path) then
         return
           Result_Zone_Id.Error
             (Not_Found_Error, "/etc/localtime not found");
      end if;

      --  Try to resolve symlink using readlink
      declare
         Link_Result :
           constant Infrastructure.Platform.Platform_String_Result :=
           Platform_Ops.Read_Link (Localtime_Path);
      begin
         if not Infrastructure.Platform.String_Result.Is_Ok (Link_Result) then
            return
              Result_Zone_Id.Error
                (IO_Error, "/etc/localtime is not a symlink");
         end if;

         declare
            Link_Target_Bounded :
              constant Infrastructure.Platform.Platform_String :=
              Infrastructure.Platform.String_Result.Value (Link_Result);
            Link_Target         : constant String :=
              Infrastructure.Platform.Platform_Strings.To_String
                (Link_Target_Bounded);
            Marker              : constant String := "zoneinfo/";
            Marker_Pos          : Natural := 0;
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
               return
                 Result_Zone_Id.Error
                   (IO_Error,
                    "Cannot extract zone ID from: " & Link_Target);
            end if;

            declare
               Zone_Id_Start : constant Positive := Marker_Pos + Marker'Length;
               Zone_Id_Str   : constant String   :=
                 Link_Target (Zone_Id_Start .. Link_Target'Last);
            begin
               return
                 Result_Zone_Id.Ok
                   (Domain.Value_Object.Zone_Id.Make_Zone_Id (Zone_Id_Str));
            end;
         end;
      end;
   exception
      when E : others =>
         return
           Result_Zone_Id.Error
             (IO_Error,
              "Error detecting timezone: " &
              Ada.Exceptions.Exception_Message (E));
   end Find_My_Id;

   --  ========================================================================
   --  6. List_All_Zones (GPT-5 Pattern: Uses port's canonical Result type)
   --  ========================================================================

   function List_All_Zones
     (Source : Source_Info_Type; Descending : Boolean)
      return Application.Port.Inbound.List_All_Order_By_Id
     .List_All_Zones_Result
   is
      use Ada.Directories;
      use TZif.Application.Port.Inbound.List_All_Order_By_Id;

      Path_Str : constant String := To_String (Get_Path (Source));
      Zones    : Zone_Id_List;

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
                                 Zones.Append (Make_Zone_Id (Zone_Name));
                              exception
                                 when Constraint_Error =>
                                    --  DELIBERATE: Skip invalid zone names
                                    --  Malformed entries silently skipped
                                    null;
                              end;
                           end if;

                        when others =>
                           --  DELIBERATE: Skip non-directory/non-file entries
                           --  Only regular files and directories are relevant
                           null;
                     end case;
                  end;
               end if;
            end;
         end loop;

         End_Search (Search);
      exception
         when Ada.Directories.Name_Error | Ada.Directories.Use_Error =>
            --  DELIBERATE: Skip inaccessible directories silently
            --  Some system directories may be permission-protected;
            --  continuing scan of other accessible directories
            null;
      end Scan_Directory;

      function Less_Than (Left, Right : Zone_Id_Type) return Boolean is
      begin
         return To_String (Left) < To_String (Right);
      end Less_Than;

      package Zone_Sorting is new Zone_Id_Vectors.Generic_Sorting
        ("<" => Less_Than);

   begin
      if not Exists (Path_Str) or else Kind (Path_Str) /= Directory then
         return
           List_All_Zones_Result_Package.Error
             (Not_Found_Error, "Source path not found: " & Path_Str);
      end if;

      Scan_Directory (Path_Str);

      if Descending then
         Zone_Sorting.Sort (Zones);
         Zone_Id_Vectors.Reverse_Elements (Zones);
      else
         Zone_Sorting.Sort (Zones);
      end if;

      return List_All_Zones_Result_Package.Ok (Zones);

   exception
      when E : others =>
         return
           List_All_Zones_Result_Package.Error
             (IO_Error,
              "Error listing zones: " & Ada.Exceptions.Exception_Message (E));
   end List_All_Zones;

   --  ========================================================================
   --  7. Find_By_Pattern
   --  ========================================================================

   function Find_By_Pattern
     (Pattern : TZif.Application.Port.Inbound.Find_By_Pattern.Pattern_String;
      Yield   : Application.Port.Inbound.Find_By_Pattern.Yield_Callback_Access)
      return Application.Port.Inbound.Find_By_Pattern.Find_By_Pattern_Result
   is
      use Ada.Directories;
      use TZif.Application.Port.Inbound.Find_By_Pattern;
      Pattern_Str : constant String :=
        Application.Port.Inbound.Find_By_Pattern.Pattern_Strings.To_String
          (Pattern);

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
                           --  Check if zone name contains pattern (substring
                           --  match)
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
                                   (Application.Port.Inbound.Find_By_Pattern
                                      .Zone_Name_Strings
                                      .To_Bounded_String
                                      (Zone_Name));
                              end if;
                           end;

                        when others =>
                           --  DELIBERATE: Skip non-directory/non-file entries
                           --  Only regular files and directories are relevant
                           null;
                     end case;
                  end;
               end if;
            end;
         end loop;

         End_Search (Search);
      exception
         when Ada.Directories.Name_Error | Ada.Directories.Use_Error =>
            --  DELIBERATE: Skip inaccessible directories silently
            --  Some system directories may be permission-protected;
            --  continuing scan of other accessible directories
            null;
      end Scan_Directory;

   begin
      --  Scan all search paths
      for Path of Search_Paths loop
         if Exists (Path.all) and then Kind (Path.all) = Directory then
            Scan_Directory (Path.all);
         end if;
      end loop;

      return Find_By_Pattern_Result_Package.Ok (Domain.Value_Object.Unit.Unit);

   exception
      when E : others =>
         return
           Find_By_Pattern_Result_Package.Error
             (IO_Error,
              "Error searching pattern: " &
              Ada.Exceptions.Exception_Message (E));
   end Find_By_Pattern;

   --  ========================================================================
   --  8. Find_By_Region
   --  ========================================================================

   function Find_By_Region
     (Region : TZif.Application.Port.Inbound.Find_By_Region.Region_String;
      Yield  : Application.Port.Inbound.Find_By_Region.Yield_Callback_Access)
      return Application.Port.Inbound.Find_By_Region.Find_By_Region_Result
   is
      use Ada.Directories;
      use TZif.Application.Port.Inbound.Find_By_Region;
      Region_Str : constant String :=
        Application.Port.Inbound.Find_By_Region.Region_Strings.To_String
          (Region);

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
                           --  Check if zone starts with region prefix
                           if Zone_Name'Length >= Region_Str'Length
                             and then
                               Zone_Name
                                 (Zone_Name'First ..
                                      Zone_Name'First + Region_Str'Length -
                                      1) =
                               Region_Str
                           then
                              Yield
                                (Application.Port.Inbound.Find_By_Region
                                   .Zone_Name_Strings
                                   .To_Bounded_String
                                   (Zone_Name));
                           end if;

                        when others =>
                           --  DELIBERATE: Skip non-directory/non-file entries
                           --  Only regular files and directories are relevant
                           null;
                     end case;
                  end;
               end if;
            end;
         end loop;

         End_Search (Search);
      exception
         when Ada.Directories.Name_Error | Ada.Directories.Use_Error =>
            --  DELIBERATE: Skip inaccessible directories silently
            --  Some system directories may be permission-protected;
            --  continuing scan of other accessible directories
            null;
      end Scan_Directory;

   begin
      for Path of Search_Paths loop
         if Exists (Path.all) and then Kind (Path.all) = Directory then
            Scan_Directory (Path.all);
         end if;
      end loop;

      return Find_By_Region_Result_Package.Ok (Domain.Value_Object.Unit.Unit);

   exception
      when E : others =>
         return
           Find_By_Region_Result_Package.Error
             (IO_Error,
              "Error searching region: " &
              Ada.Exceptions.Exception_Message (E));
   end Find_By_Region;

   --  ========================================================================
   --  9. Find_By_Regex
   --  ========================================================================

   function Find_By_Regex
     (Regex : TZif.Application.Port.Inbound.Find_By_Regex.Regex_String;
      Yield : Application.Port.Inbound.Find_By_Regex.Yield_Callback_Access)
      return Application.Port.Inbound.Find_By_Regex.Find_By_Regex_Result
   is
      use Ada.Directories;
      use GNAT.Regpat;
      use TZif.Application.Port.Inbound.Find_By_Regex;

      Regex_Str : constant String :=
        Application.Port.Inbound.Find_By_Regex.Regex_Strings.To_String (Regex);

      --  Pattern_Matcher will be declared in the begin block after validation

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
                                (Application.Port.Inbound.Find_By_Regex
                                   .Zone_Name_Strings
                                   .To_Bounded_String
                                   (Zone_Name));
                           end if;

                        when others =>
                           --  DELIBERATE: Skip non-directory/non-file entries
                           --  Only regular files and directories are relevant
                           null;
                     end case;
                  end;
               end if;
            end;
         end loop;

         End_Search (Search);
      exception
         when Ada.Directories.Name_Error | Ada.Directories.Use_Error =>
            --  DELIBERATE: Skip inaccessible directories silently
            --  Some system directories may be permission-protected;
            --  continuing scan of other accessible directories
            null;
      end Scan_Directory;

   begin
      --  Validate regex - exceptions in declarative part propagate to here
      declare
         Pattern : constant Pattern_Matcher := Compile (Regex_Str);
      begin
         for Path of Search_Paths loop
            if Exists (Path.all) and then Kind (Path.all) = Directory then
               Scan_Directory (Path.all, "", Pattern);
            end if;
         end loop;

         return
           Find_By_Regex_Result_Package.Ok (Domain.Value_Object.Unit.Unit);
      end;

   exception
      when Expression_Error =>
         return
           Find_By_Regex_Result_Package.Error
             (Validation_Error, "Invalid regex pattern: " & Regex_Str);
      when E : others       =>
         return
           Find_By_Regex_Result_Package.Error
             (IO_Error,
              "Error searching regex: " &
              Ada.Exceptions.Exception_Message (E));
   end Find_By_Regex;

   --  ========================================================================
   --  10. Discover_Sources
   --  ========================================================================

   function Discover_Sources
     (Search_Paths : Application.Port.Inbound.Discover_Sources.Path_List)
      return Application.Port.Inbound.Discover_Sources.Discovery_Result
   is
      use Ada.Directories;
      use TZif.Application.Port.Inbound.Discover_Sources;
      use type TZif.Domain.Value_Object.Source_Info.Path_String_Type;

      Data : Discovery_Data_Type;

      --  Helper: Read VERSION file
      function Read_Version (Dir_Path : String) return String is
         use Ada.Text_IO;
         Version_File : constant String := Dir_Path & "/+VERSION";
         File         : File_Type;
         Line         : String (1 .. 32);
         Last         : Natural;
      begin
         if Exists (Version_File) and then Kind (Version_File) = Ordinary_File
         then
            Open (File, In_File, Version_File);
            Get_Line (File, Line, Last);
            Close (File);
            return Line (1 .. Last);
         end if;
         return "unknown";
      exception
         when others =>
            return "unknown";
      end Read_Version;

      --  Helper: Count zone files recursively (with limits)
      function Count_Zones (Dir_Path : String) return Natural is
         Count     : Natural  := 0;
         Max_Count : constant := 10_000;  -- DoS protection

         procedure Count_Recursive (Path : String; Depth : Natural) is
            Search : Search_Type;
            pragma Warnings (Off, Search);
            Item : Directory_Entry_Type;
         begin
            if Count >= Max_Count or else Depth > 10 then
               return;  -- Limits reached

            end if;

            Start_Search (Search, Path, "*");
            while More_Entries (Search) loop
               Get_Next_Entry (Search, Item);
               declare
                  Name : constant String := Simple_Name (Item);
               begin
                  if Name /= "." and then Name /= ".." then
                     if Kind (Item) = Directory then
                        Count_Recursive (Full_Name (Item), Depth + 1);
                     elsif Kind (Item) = Ordinary_File then
                        Count := Count + 1;
                     end if;
                  end if;
               end;
            end loop;
            End_Search (Search);
         exception
            when Ada.Directories.Name_Error | Ada.Directories.Use_Error =>
               --  DELIBERATE: Skip inaccessible directories silently
               --  Some system directories may be permission-protected;
               --  continuing count of accessible directories
               null;
         end Count_Recursive;

      begin
         Count_Recursive (Dir_Path, 0);
         return Count;
      end Count_Zones;

      --  Helper: Scan single path for sources
      procedure Scan_Path
        (Path_Str : Application.Port.Inbound.Discover_Sources.Path_String)
      is
         Path : constant String :=
           Application.Port.Inbound.Discover_Sources.Path_Strings.To_String
             (Path_Str);
      begin
         if not Exists (Path) then
            Error_Vectors.Append
              (Data.Errors,
               Error_Type'
                 (Kind    => IO_Error,
                  Message =>
                    Error_Strings.To_Bounded_String
                      ("Path not found: " & Path)));
            return;
         end if;

         if Kind (Path) /= Directory then
            Error_Vectors.Append
              (Data.Errors,
               Error_Type'
                 (Kind    => Validation_Error,
                  Message =>
                    Error_Strings.To_Bounded_String
                      ("Not a directory: " & Path)));
            return;
         end if;

         --  Check for VERSION file
         declare
            Version_File : constant String := Path & "/+VERSION";
         begin
            if not Exists (Version_File) then
               Error_Vectors.Append
                 (Data.Errors,
                  Error_Type'
                    (Kind    => Validation_Error,
                     Message =>
                       Error_Strings.To_Bounded_String
                         ("No +VERSION file in: " & Path)));
               return;
            end if;
         end;

         --  Valid source found - collect metadata
         declare
            ULID : constant ULID_Type := TZif.Infrastructure.ULID.Generate;
            Path_Val    : constant Path_String_Type    := Make_Path (Path);
            Version_Str : constant String              := Read_Version (Path);
            Version     : constant Version_String_Type :=
              Make_Version (Version_Str);
            Zone_Count  : constant Natural             := Count_Zones (Path);
            Source      : constant Source_Info_Type    :=
              Make_Source_Info (ULID, Path_Val, Version, Zone_Count);
         begin
            --  Check for duplicates by path
            for Existing of Data.Sources loop
               if Get_Path (Existing) = Path_Val then
                  return;  -- Already have this source

               end if;
            end loop;

            Application.Port.Inbound.Discover_Sources.Source_Info_Vectors
              .Append
              (Data.Sources, Source);
         end;

      exception
         when E : others =>
            Error_Vectors.Append
              (Data.Errors,
               Error_Type'
                 (Kind    => IO_Error,
                  Message =>
                    Error_Strings.To_Bounded_String
                      ("Error scanning " & Path & ": " &
                       Ada.Exceptions.Exception_Message (E))));
      end Scan_Path;

      Task_Count : constant Natural :=
        TZif.Infrastructure.CPU.Get_Optimal_Task_Count;
      pragma Unreferenced (Task_Count);

   begin
      --  Check if empty path list
      if Path_Vectors.Is_Empty (Search_Paths) then
         return
           Discovery_Result_Package.Error
             (Kind => Validation_Error, Message => "No search paths provided");
      end if;

      --  Sequential scanning for now (parallel implementation TBD)
      for Path of Search_Paths loop
         Scan_Path (Path);
      end loop;

      --  Return results
      if Application.Port.Inbound.Discover_Sources.Source_Info_Vectors.Is_Empty
          (Data.Sources)
        and then not Error_Vectors.Is_Empty (Data.Errors)
      then
         --  All paths failed
         return
           Discovery_Result_Package.Error
             (Kind    => IO_Error,
              Message =>
                "No sources found. Errors:" &
                Error_Vectors.Length (Data.Errors)'Image);
      else
         --  Partial or full success
         return Discovery_Result_Package.Ok (Data);
      end if;

   end Discover_Sources;

   --  ========================================================================
   --  11. Load_Source
   --  ========================================================================

   function Load_Source
     (Path : Application.Port.Inbound.Load_Source.Path_String)
      return Application.Port.Inbound.Load_Source.Load_Source_Result
   is
      use Ada.Directories;
      use TZif.Application.Port.Inbound.Load_Source;
      Path_Str : constant String :=
        Application.Port.Inbound.Load_Source.Path_Strings.To_String (Path);
   begin
      if not Exists (Path_Str) then
         return
           Load_Source_Result_Package.Error
             (Not_Found_Error, "Path not found: " & Path_Str);
      end if;

      if Kind (Path_Str) /= Directory then
         return
           Load_Source_Result_Package.Error
             (Validation_Error, "Path is not a directory: " & Path_Str);
      end if;

      declare
         use Ada.Text_IO;
         ULID : constant ULID_Type        := TZif.Infrastructure.ULID.Generate;
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
                                 --  DELIBERATE: Skip non-directory/non-file
                                 --  Only regular files and dirs are relevant
                                 null;
                           end case;
                        end if;
                     end;
                  end loop;
                  End_Search (S);
               exception
                  when Ada.Directories.Name_Error
                    | Ada.Directories.Use_Error =>
                     --  DELIBERATE: Skip inaccessible directories silently
                     --  Permission-protected dirs don't affect count
                     null;
               end Count_Recursive;

            begin
               Count_Recursive (Dir_Path);
            end Count_Zones;

            Source : Source_Info_Type;
         begin
            Count_Zones (Path_Str);
            Source := Make_Source_Info (ULID, Path_Val, Version, Zone_Count);
            return Load_Source_Result_Package.Ok (Source);
         end;
      exception
         when E : others =>
            if Is_Open (File) then
               Close (File);
            end if;
            return
              Load_Source_Result_Package.Error
                (IO_Error,
                 "Error loading source: " &
                 Ada.Exceptions.Exception_Message (E));
      end;

   exception
      when E : others =>
         return
           Load_Source_Result_Package.Error
             (IO_Error,
              "Unexpected error: " & Ada.Exceptions.Exception_Message (E));
   end Load_Source;

   --  ========================================================================
   --  12. Validate_Source
   --  ========================================================================

   function Validate_Source
     (Path : Application.Port.Inbound.Validate_Source.Path_String)
      return Application.Port.Inbound.Validate_Source.Validation_Result
   is
      use Ada.Directories;
      use TZif.Application.Port.Inbound.Validate_Source;
      Path_Str : constant String :=
        Application.Port.Inbound.Validate_Source.Path_Strings.To_String (Path);
   begin
      if not Exists (Path_Str) then
         return Validation_Result_Package.Ok (False);
      end if;

      if Kind (Path_Str) /= Directory then
         return Validation_Result_Package.Ok (False);
      end if;

      --  Check for at least one TZif file
      declare
         Search : Search_Type;
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

         return Validation_Result_Package.Ok (Found_TZif);
      exception
         when Name_Error | Use_Error =>
            return Validation_Result_Package.Ok (False);
      end;

   exception
      when E : others =>
         return
           Validation_Result_Package.Error
             (IO_Error,
              "Error validating source: " &
              Ada.Exceptions.Exception_Message (E));
   end Validate_Source;

end TZif.Infrastructure.Adapter.File_System.Repository;
