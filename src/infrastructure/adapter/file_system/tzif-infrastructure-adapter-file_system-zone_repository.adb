pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.Adapter.File_System.Zone_Repository
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Zone Repository infrastructure adapter.
--
--  Architecture:
--    Infrastructure layer adapter (hexagonal architecture).
--    Implements outbound ports for external systems.
--
--  ===========================================================================

with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;
with Functional.Try;
with TZif_Config;
with TZif.Infrastructure.TZif_Parser;
with TZif.Infrastructure.ULID;
with TZif.Domain.Error;
with TZif.Domain.TZif_Data;
with TZif.Domain.Entity.Zone;
with TZif.Domain.Value_Object.TZif_Header;
with TZif.Domain.Value_Object.Transition_Info;
with TZif.Domain.Value_Object.Timezone_Type;
with TZif.Domain.Value_Object.Source_Info;

package body TZif.Infrastructure.Adapter.File_System.Zone_Repository is

   use Ada.Directories;
   use TZif.Infrastructure.TZif_Parser;
   use TZif.Domain.Error;
   use TZif.Domain.TZif_Data;
   use TZif.Domain.Entity.Zone;
   use TZif.Domain.Value_Object.TZif_Header;
   use TZif.Domain.Value_Object.Transition_Info;
   use TZif.Domain.Value_Object.Timezone_Type;

   --  ===========================================================
   --  Standard TZif Search Paths (POSIX systems)
   --  ===========================================================

   type Search_Path_Array is
     array (Positive range <>) of access constant String;

   Z1 : aliased constant String := "/usr/share/zoneinfo";
   Z2 : aliased constant String := "/var/db/timezone/zoneinfo";
   Z3 : aliased constant String := "/usr/lib/zoneinfo";
   Z4 : aliased constant String := "/etc/zoneinfo";

   Search_Paths : constant Search_Path_Array :=
     [Z1'Access, Z2'Access, Z3'Access, Z4'Access];

   --  ===========================================================
   --  Find_TZif_File
   --  ===========================================================
   --  Search for TZif file in standard paths
   --  Returns full path if found, empty string otherwise
   --  ===========================================================

   function Find_TZif_File (Zone_Id : String) return String is
   begin
      --  Try each search path in order
      for Path of Search_Paths loop
         declare
            Full_Path : constant String := Path.all & "/" & Zone_Id;
         begin
            --  Check if file exists
            if Exists (Full_Path) and then Kind (Full_Path) = Ordinary_File
            then
               return Full_Path;
            end if;
         end;
      end loop;

      --  Not found
      return "";
   end Find_TZif_File;

   --  ===========================================================
   --  Find_By_Id
   --  ===========================================================

   function Find_By_Id (Id : Zone_Id_Type) return Repository_Zone_Result is
      Zone_Id_Str : constant String := To_String (Id);
      File_Path   : constant String := Find_TZif_File (Zone_Id_Str);
   begin
      --  Check if file was found
      if File_Path'Length = 0 then
         return
           Zone_Result.Error
             (Not_Found_Error,
              "Zone '" & Zone_Id_Str & "' not found in system paths");
      end if;

      --  Parse the TZif file
      declare
         Parse_Result : constant Parse_Result_Type :=
           Parse_From_File (File_Path);
      begin
         --  Check parse result
         if not Infrastructure.TZif_Parser.Parse_Result.Is_Ok (Parse_Result)
         then
            --  Extract error from parse result
            declare
               Parse_Error : constant Error_Type :=
                 Infrastructure.TZif_Parser.Parse_Result.Error_Info
                   (Parse_Result);
               Error_Msg   : constant String     :=
                 Error_Strings.To_String (Parse_Error.Message);
            begin
               return
                 Zone_Result.Error
                   (Parse_Error.Kind,
                    "Failed to parse " & Zone_Id_Str & ": " & Error_Msg);
            end;
         end if;

         --  Success: Extract parsed data and create Zone entity
         declare
            TZif_Data : constant TZif_Data_Type :=
              Infrastructure.TZif_Parser.Parse_Result.Value (Parse_Result);
            Zone      : constant Zone_Type      :=
              Make_Zone (Id => Id, Data => TZif_Data);
         begin
            return Zone_Result.Ok (Zone);
         end;
      end;
   end Find_By_Id;

   --  ===========================================================
   --  Exists
   --  ===========================================================

   function Exists (Id : Zone_Id_Type) return Repository_Boolean_Result is
      Zone_Id_Str : constant String := To_String (Id);

      --  Map exception to Error_Type
      function Map_Exists_Exception
        (Occ : Ada.Exceptions.Exception_Occurrence)
         return Error_Type
      is
      begin
         return (Kind    => IO_Error,
                 Message => Error_Strings.To_Bounded_String
                   ("Error checking existence of " & Zone_Id_Str & ": " &
                    Ada.Exceptions.Exception_Message (Occ)));
      end Map_Exists_Exception;

      --  Raw action that may raise exceptions
      function Raw_Check_Exists return Boolean is
         File_Path : constant String := Find_TZif_File (Zone_Id_Str);
      begin
         return File_Path'Length > 0;
      end Raw_Check_Exists;

      --  Instantiate Functional.Try
      function Try_Check_Exists is new Functional.Try.Try_To_Result
        (T             => Boolean,
         E             => Error_Type,
         Result_Type   => Repository_Boolean_Result,
         Ok            => Boolean_Result.Ok,
         New_Error     => Boolean_Result.From_Error,
         Map_Exception => Map_Exists_Exception,
         Action        => Raw_Check_Exists);
   begin
      return Try_Check_Exists;
   end Exists;

   --  ===========================================================
   --  Get_Version
   --  ===========================================================

   function Get_Version (Id : Zone_Id_Type) return Repository_Version_Result is
      Zone_Id_Str : constant String := To_String (Id);
      File_Path   : constant String := Find_TZif_File (Zone_Id_Str);
   begin
      --  Check if file was found
      if File_Path'Length = 0 then
         return
           Version_Result.Error
             (IO_Error,
              "Zone '" & Zone_Id_Str & "' not found in system paths");
      end if;

      --  Parse the TZif file
      declare
         Parse_Result : constant Parse_Result_Type :=
           Parse_From_File (File_Path);
      begin
         --  Check parse result
         if not Infrastructure.TZif_Parser.Parse_Result.Is_Ok (Parse_Result)
         then
            --  Extract error from parse result
            declare
               Parse_Error : constant Error_Type :=
                 Infrastructure.TZif_Parser.Parse_Result.Error_Info
                   (Parse_Result);
               Error_Msg   : constant String     :=
                 Error_Strings.To_String (Parse_Error.Message);
            begin
               return
                 Version_Result.Error
                   (Parse_Error.Kind,
                    "Failed to parse " & Zone_Id_Str & ": " & Error_Msg);
            end;
         end if;

         --  Success: Extract version from parsed data
         declare
            TZif_Data : constant TZif_Data_Type    :=
              Infrastructure.TZif_Parser.Parse_Result.Value (Parse_Result);
            Version   : constant TZif_Version_Type := TZif_Data.Header.Version;
         begin
            return Version_Result.Ok (Version);
         end;
      end;
   end Get_Version;

   --  ===========================================================
   --  Get_Transition_At_Epoch
   --  ===========================================================

   function Get_Transition_At_Epoch
     (Id : Zone_Id_Type; Epoch_Time : Epoch_Seconds_Type)
      return Repository_Transition_Info_Result
   is
      Zone_Id_Str : constant String := To_String (Id);
      File_Path   : constant String := Find_TZif_File (Zone_Id_Str);
   begin
      --  Check if file was found
      if File_Path'Length = 0 then
         return
           Transition_Info_Result.Error
             (IO_Error,
              "Zone '" & Zone_Id_Str & "' not found in system paths");
      end if;

      --  Parse the TZif file
      declare
         Parse_Result : constant Parse_Result_Type :=
           Parse_From_File (File_Path);
      begin
         --  Check parse result
         if not Infrastructure.TZif_Parser.Parse_Result.Is_Ok (Parse_Result)
         then
            --  Extract error from parse result
            declare
               Parse_Error : constant Error_Type :=
                 Infrastructure.TZif_Parser.Parse_Result.Error_Info
                   (Parse_Result);
               Error_Msg   : constant String     :=
                 Error_Strings.To_String (Parse_Error.Message);
            begin
               return
                 Transition_Info_Result.Error
                   (Parse_Error.Kind,
                    "Failed to parse " & Zone_Id_Str & ": " & Error_Msg);
            end;
         end if;

         --  Success: Extract transition info from parsed data
         declare
            use TZif.Domain.TZif_Data.Type_Index_Options;
            TZif_Data : constant TZif_Data_Type :=
              Infrastructure.TZif_Parser.Parse_Result.Value (Parse_Result);

            --  Find timezone type in effect at the given epoch time
            Type_Index_Opt : constant Type_Index_Option :=
              Find_Type_At_Time (TZif_Data, Epoch_Time);
            Tz_Length    : constant Natural :=
              Timezone_Type_Vectors.Length (TZif_Data.Timezone_Types);
         begin
            --  Validate type index before accessing
            if Tz_Length = 0 then
               return
                 Transition_Info_Result.Error
                   (Parse_Error, "No timezone types in zone file");
            elsif Is_None (Type_Index_Opt) then
               return
                 Transition_Info_Result.Error
                   (Parse_Error, "No timezone type found for given time");
            end if;

            declare
               --  Note: TZif uses 0-based indices; Get_Type expects 0-based
               --  and adds 1 internally for vector access
               Type_Index : constant Natural := Value (Type_Index_Opt);
            begin
               if Type_Index >= Tz_Length then
                  return
                    Transition_Info_Result.Error
                      (Parse_Error, "Invalid type index in zone file");
               end if;

               declare
                  TZ_Type : constant Timezone_Type_Record :=
                    Get_Type (TZif_Data, Type_Index);

                  --  Build transition info result
                  Info : constant Transition_Info_Type :=
                    Make_Transition_Info
                      (Epoch_Time   => Epoch_Time,
                       UTC_Offset   => TZ_Type.UTC_Offset,
                       Is_DST       => TZ_Type.Is_DST,
                       Abbreviation => Get_Abbreviation (TZ_Type));
               begin
                  return Transition_Info_Result.Ok (Info);
               end;
            end;
         end;
      end;
   end Get_Transition_At_Epoch;

   --  ===========================================================
   --  Find_My_Id
   --  ===========================================================
   --  Determines local timezone by reading /etc/localtime
   --  On Unix/Linux/macOS, this is typically a symlink to a zoneinfo file
   --  ===========================================================

   function Find_My_Id return Repository_Zone_Id_Result is
      Localtime_Path : constant String := "/etc/localtime";

      --  Map exception to error
      function Map_Find_My_Id_Exception
        (Occ : Ada.Exceptions.Exception_Occurrence)
         return Repository_Zone_Id_Result
      is
      begin
         return
           Zone_Id_Result.Error
             (IO_Error,
              "/etc/localtime is not a symlink-cannot determine zone ID: " &
              Ada.Exceptions.Exception_Message (Occ));
      end Map_Find_My_Id_Exception;

      --  Core logic
      function Raw_Find_My_Id return Repository_Zone_Id_Result is
      begin
         --  Check if /etc/localtime exists
         if not Exists (Localtime_Path) then
            return
              Zone_Id_Result.Error
                (IO_Error,
                 "/etc/localtime not found - cannot determine local timezone");
         end if;

         --  Try to read the symlink target using injected platform operations
         declare
            Link_Result :
              constant Infrastructure.Platform.Platform_String_Result :=
              Platform_Ops.Read_Link (Localtime_Path);
         begin
            if not Infrastructure.Platform.String_Result.Is_Ok (Link_Result)
            then
               return
                 Zone_Id_Result.Error
                   (IO_Error,
                    "/etc/localtime is not a symlink or cannot be read");
            end if;

            declare
               Link_Target_Bounded :
                 constant Infrastructure.Platform.Platform_String :=
                 Infrastructure.Platform.String_Result.Value (Link_Result);
               Link_Target         : constant String :=
                 Infrastructure.Platform.Platform_Strings.To_String
                   (Link_Target_Bounded);
               Zoneinfo_Marker     : constant String := "zoneinfo/";
               Marker_Index        : Natural         := 0;
            begin
               --  Find "zoneinfo/" in the path
               for I in
                 Link_Target'First ..
                   Link_Target'Last - Zoneinfo_Marker'Length + 1
               loop
                  if Link_Target (I .. I + Zoneinfo_Marker'Length - 1) =
                    Zoneinfo_Marker
                  then
                     Marker_Index := I;
                     exit;
                  end if;
               end loop;

               if Marker_Index = 0 then
                  return
                    Zone_Id_Result.Error
                      (IO_Error,
                       "Could not extract zone ID from symlink: " &
                       Link_Target);
               end if;

               --  Extract everything after "zoneinfo/"
               declare
                  Zone_Id_Start : constant Positive :=
                    Marker_Index + Zoneinfo_Marker'Length;
                  Zone_Id_Str   : constant String   :=
                    Link_Target (Zone_Id_Start .. Link_Target'Last);
               begin
                  --  Note: If Zone_Id_Str exceeds bounded length, Make_Zone_Id
                  --  will raise Constraint_Error which is caught by Try wrapper
                  --  and mapped to IO_Error. This is acceptable - the error
                  --  message includes the exception details.
                  return Zone_Id_Result.Ok (Make_Zone_Id (Zone_Id_Str));
               end;
            end;
         end;
      end Raw_Find_My_Id;

      --  Try wrapper
      function Try_Find_My_Id is new Functional.Try.Try_To_Any_Result
        (Result_Type   => Repository_Zone_Id_Result,
         Map_Exception => Map_Find_My_Id_Exception,
         Action        => Raw_Find_My_Id);
   begin
      return Try_Find_My_Id;
   end Find_My_Id;

   --  ===========================================================
   --  List_All_Zones
   --  ===========================================================
   --  Scan filesystem and return all zone IDs with specified sort order
   --  ===========================================================

   function List_All_Zones
     (Sort_Order : Sort_Order_Type) return Repository_Zone_List_Result
   is
      use Zone_Id_Vectors;

      Zones : Zone_Id_List;

      --  ===========================================================
      --  Recursive directory walker
      --  ===========================================================
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
               --  Skip hidden files and special directories
               if Name'Length > 0 and then Name (Name'First) /= '.' then
                  declare
                     Full_Path : constant String := Full_Name (Item);
                     Zone_Name : constant String :=
                       (if Prefix = "" then Name else Prefix & "/" & Name);
                  begin
                     case Kind (Item) is
                        when Directory =>
                           --  Recursively scan subdirectory
                           Scan_Directory (Full_Path, Zone_Name);

                        when Ordinary_File =>
                           --  Add as zone ID (exclude some special files)
                           if Name /= "zone.tab"
                             and then Name /= "zone1970.tab"
                             and then Name /= "iso3166.tab"
                             and then Name /= "leapseconds"
                             and then Name /= "tzdata.zi"
                             and then Name /= "+VERSION"
                             and then Zone_Name'Length <=
                               TZif_Config.Max_Zone_ID_Length
                           then
                              begin
                                 if not Zone_Id_Vectors.Is_Full (Zones) then
                                    Zone_Id_Vectors.Unchecked_Append
                                      (Zones, Make_Zone_Id (Zone_Name));
                                 end if;
                              exception
                                 when Constraint_Error =>
                                    --  DESIGN DECISION: Skip invalid zone names
                                    --  Malformed entries silently skipped
                                    null;
                              end;
                           end if;

                        when others =>
                           --  DESIGN DECISION: Skip non-directory/non-file entries
                           --  Only regular files and directories are relevant
                           null;
                     end case;
                  end;
               end if;
            end;
         end loop;

         End_Search (Search);
      exception
         when Name_Error =>
            --  DESIGN DECISION: Skip directories that don't exist
            --  Missing paths don't affect the overall scan
            null;
         when Use_Error  =>
            --  DESIGN DECISION: Skip permission-denied directories
            --  Protected system directories don't affect scan
            null;
      end Scan_Directory;

      --  ===========================================================
      --  Comparison function for sorting
      --  ===========================================================
      function Less_Than (Left, Right : Zone_Id_Type) return Boolean is
        (To_String (Left) < To_String (Right));

      procedure Sort_Zones is new Zone_Id_Vectors.Generic_Sort
        ("<" => Less_Than);

   begin
      --  Scan only the first search path that exists
      --  This avoids duplicates and ensures consistent versioning
      for Path of Search_Paths loop
         if Exists (Path.all) and then Kind (Path.all) = Directory then
            Scan_Directory (Path.all);
            exit;  --  Use first available source only
         end if;
      end loop;

      --  Sort the results
      Sort_Zones (Zones);
      if Sort_Order = Descending then
         Zone_Id_Vectors.Reverse_Elements (Zones);
      end if;

      return Zone_List_Result.Ok (Zones);

   exception
      when E : others =>
         return
           Zone_List_Result.Error
             (IO_Error,
              "Failed to list zones: " & Ada.Exceptions.Exception_Message (E));
   end List_All_Zones;

   --  ===========================================================
   --  List_Sources
   --  ===========================================================
   --  Discover all available timezone database sources on the system
   --  Generate ULID for each, detect version, count zones
   --  ===========================================================

   function List_Sources return Repository_Source_List_Result is
      use TZif.Domain.Value_Object.Source_Info;

      Sources : Source_Info_List;

      --  ===========================================================
      --  Read version from VERSION file in zoneinfo directory
      --  ===========================================================
      function Read_Version (Dir_Path : String) return String is
         Version_File : constant String := Dir_Path & "/+VERSION";
         use Ada.Text_IO;
         File        : File_Type;
         Version_Str : String (1 .. 32);
         Last        : Natural;
      begin
         if Exists (Version_File) and then Kind (Version_File) = Ordinary_File
         then
            Open (File, In_File, Version_File);
            Get_Line (File, Version_Str, Last);
            Close (File);
            return Version_Str (1 .. Last);
         else
            --  Fallback: use modification time as version indicator
            return "unknown";
         end if;
      exception
         when others =>
            return "unknown";
      end Read_Version;

      --  ===========================================================
      --  Count zones in directory (quick count, not full scan)
      --  ===========================================================
      function Count_Zones (Dir_Path : String) return Natural is
         Count : Natural := 0;

         procedure Count_Recursive (Path : String) is
            Sub_Search : Search_Type;
            pragma Warnings (Off, Sub_Search);
            Sub_Item : Directory_Entry_Type;
         begin
            if Count > 1_000 then
               return;  --  Stop counting after 1000 for performance

            end if;

            Start_Search (Sub_Search, Path, "*");
            while More_Entries (Sub_Search) loop
               Get_Next_Entry (Sub_Search, Sub_Item);
               declare
                  Name : constant String := Simple_Name (Sub_Item);
               begin
                  if Name'Length > 0 and then Name (Name'First) /= '.' then
                     case Kind (Sub_Item) is
                        when Directory =>
                           Count_Recursive (Full_Name (Sub_Item));

                        when Ordinary_File =>
                           if Name /= "zone.tab"
                             and then Name /= "zone1970.tab"
                             and then Name /= "iso3166.tab"
                             and then Name /= "leapseconds"
                             and then Name /= "tzdata.zi"
                             and then Name /= "+VERSION"
                           then
                              Count := Count + 1;
                           end if;

                        when others =>
                           null;
                     end case;
                  end if;
               end;
            end loop;
            End_Search (Sub_Search);
         exception
            when Name_Error | Use_Error =>
               null;
         end Count_Recursive;

      begin
         Count_Recursive (Dir_Path);
         return Count;
      end Count_Zones;

   begin
      --  Scan all search paths and create Source_Info for each existing source
      for Path of Search_Paths loop
         if Exists (Path.all) and then Kind (Path.all) = Directory then
            declare
               ULID : constant ULID_Type := TZif.Infrastructure.ULID.Generate;
               Path_Str    : constant Path_String_Type := Make_Path (Path.all);
               Version_Str : constant String := Read_Version (Path.all);
               Version     : constant Version_String_Type :=
                 Make_Version (Version_Str);
               Zone_Count  : constant Natural := Count_Zones (Path.all);

               Source : constant Source_Info_Type :=
                 Make_Source_Info
                   (ULID       => ULID, Path => Path_Str, Version => Version,
                    Zone_Count => Zone_Count);
            begin
               if not Source_Info_Vectors.Is_Full (Sources) then
                  Source_Info_Vectors.Unchecked_Append (Sources, Source);
               end if;
            end;
         end if;
      end loop;

      return Source_List_Result.Ok (Sources);

   exception
      when E : others =>
         return
           Source_List_Result.Error
             (IO_Error,
              "Failed to list sources: " &
              Ada.Exceptions.Exception_Message (E));
   end List_Sources;

end TZif.Infrastructure.Adapter.File_System.Zone_Repository;
