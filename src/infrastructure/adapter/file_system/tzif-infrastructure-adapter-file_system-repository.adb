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
with Functional.Scoped;
with Functional.Try;
with Functional.Try.Map_To_Result;
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
   --  1. Find_By_Id (Uses port's canonical Result type)
   --  ========================================================================

   function Find_By_Id
     (Id : Application.Port.Inbound.Find_By_Id.Zone_Id_Input_Type)
      return Application.Port.Inbound.Find_By_Id.Find_By_Id_Result_Type
   is
      use TZif.Application.Port.Inbound.Find_By_Id;
      Zone_Id_Str : constant String := To_String (Id);

      --  Make_Error for find operation
      function Make_Find_Error
        (Kind : Error_Kind; Message : String)
         return Find_By_Id_Result_Type
      is
      begin
         case Kind is
            when Not_Found_Error =>
               return Find_By_Id_Result.Error
                 (Not_Found_Error, "Zone not found: " & Message);
            when others =>
               return Find_By_Id_Result.Error
                 (IO_Error, "Find error: " & Message);
         end case;
      end Make_Find_Error;

      --  Core lookup logic
      function Raw_Find return Find_By_Id_Result_Type is
         File_Path_Opt : constant Path_String_Option :=
           Find_TZif_File (Zone_Id_Str);
      begin
         if Path_String_Options.Is_None (File_Path_Opt) then
            return
              Find_By_Id_Result.Error
                (Not_Found_Error, "Zone not found: " & Zone_Id_Str);
         end if;

         declare
            File_Path    : constant String :=
              Path_Strings.To_String
                (Path_String_Options.Value (File_Path_Opt));
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
               Zone      : constant Zone_Type :=
                 Make_Zone (Id => Id, Data => TZif_Data);
            begin
               Zones.Insert (Id, TZif_Data);
               return Find_By_Id_Result.Ok (Zone);
            end;
         end;
      end Raw_Find;

      --  Map_To_Result wrapper
      package Try_Find is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => Error_Kind,
         Result_Type        => Find_By_Id_Result_Type,
         Make_Error         => Make_Find_Error,
         Default_Error_Kind => IO_Error,
         Action             => Raw_Find);

      Find_Mappings : constant Try_Find.Mapping_Array :=
        Try_Find.Empty_Mappings;
   begin
      return Try_Find.Run (Find_Mappings);
   end Find_By_Id;

   --  ========================================================================
   --  2. Exists_By_Id
   --  ========================================================================

   function Exists_By_Id (Id : Zone_Id_String) return Exists_Result is
      Zone_Id_Str : constant String := To_String (Id);

      --  Make_Error for exists check
      function Make_Exists_Error
        (Kind : Error_Kind; Message : String) return Exists_Result
      is
         pragma Unreferenced (Kind);
      begin
         return Boolean_Result.Error (IO_Error, Message);
      end Make_Exists_Error;

      --  Raw check - returns Result for Map_To_Result
      function Raw_Check return Exists_Result is
         File_Path_Opt : constant Path_String_Option :=
           Find_TZif_File (Zone_Id_Str);
         Found : constant Boolean :=
           Path_String_Options.Is_Some (File_Path_Opt);
      begin
         return Boolean_Result.Ok (Found);
      end Raw_Check;

      --  Map_To_Result wrapper
      package Try_Check is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => Error_Kind,
         Result_Type        => Exists_Result,
         Make_Error         => Make_Exists_Error,
         Default_Error_Kind => IO_Error,
         Action             => Raw_Check);

      Exists_Mappings : constant Try_Check.Mapping_Array :=
        Try_Check.Empty_Mappings;
   begin
      return Try_Check.Run (Exists_Mappings);
   end Exists_By_Id;

   --  ===================================================================
   --  3. Get_Transition_At_Epoch (Uses port's canonical Result type)
   --  ===================================================================

   function Get_Transition_At_Epoch
     (Id    : TZif.Application.Port.Inbound.Get_Transition_At_Epoch
        .Zone_Id_String;
      Epoch : Epoch_Seconds_Type)
      return Application.Port.Inbound.Get_Transition_At_Epoch
     .Get_Transition_Result
   is
      use TZif.Application.Port.Inbound.Get_Transition_At_Epoch;
      Zone_Id_Str : constant String :=
        Application.Port.Inbound.Get_Transition_At_Epoch.Zone_Id_Strings
          .To_String (Id);

      --  Make_Error for transition operation
      function Make_Transition_Error
        (Kind : Error_Kind; Message : String) return Get_Transition_Result
      is
      begin
         case Kind is
            when Not_Found_Error =>
               return Get_Transition_Result_Package.Error
                 (Not_Found_Error, "Zone not found: " & Message);
            when Parse_Error =>
               return Get_Transition_Result_Package.Error
                 (Parse_Error, "Parse error: " & Message);
            when others =>
               return Get_Transition_Result_Package.Error
                 (IO_Error, "Transition error: " & Message);
         end case;
      end Make_Transition_Error;

      --  Core logic
      function Raw_Get_Transition return Get_Transition_Result is
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
              Path_Strings.To_String
                (Path_String_Options.Value (File_Path_Opt));
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
                       "Parse failed: " &
                       Error_Strings.To_String (Err.Message));
               end;
            end if;

            declare
               use TZif.Domain.Value_Object.Timezone_Type;
               use TZif.Domain.TZif_Data.Type_Index_Options;
               TZif_Data      : constant TZif_Data_Type :=
                 Infrastructure.TZif_Parser.Parse_Result.Value (Parse_Result);
               Type_Index_Opt : constant Type_Index_Option :=
                 Find_Type_At_Time (TZif_Data, Epoch);
               Tz_Length      : constant Natural :=
                 Timezone_Type_Vectors.Length (TZif_Data.Timezone_Types);
            begin
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
                         (Epoch_Time   => Epoch,
                          UTC_Offset   => TZ_Type.UTC_Offset,
                          Is_DST       => TZ_Type.Is_DST,
                          Abbreviation => Get_Abbreviation (TZ_Type));
                  begin
                     return Get_Transition_Result_Package.Ok (Info);
                  end;
               end;
            end;
         end;
      end Raw_Get_Transition;

      --  Map_To_Result wrapper
      package Try_Get_Transition is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => Error_Kind,
         Result_Type        => Get_Transition_Result,
         Make_Error         => Make_Transition_Error,
         Default_Error_Kind => IO_Error,
         Action             => Raw_Get_Transition);

      Transition_Mappings : constant Try_Get_Transition.Mapping_Array :=
        Try_Get_Transition.Empty_Mappings;
   begin
      return Try_Get_Transition.Run (Transition_Mappings);
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

      --  Scoped guard for file cleanup
      procedure Close_File (F : in out File_Type) renames Close;
      package Text_File_Guard is new Functional.Scoped.Conditional_Guard_For
        (Resource       => File_Type,
         Should_Release => Is_Open,
         Release        => Close_File);

      --  Make error Result from kind and message
      function Make_Version_Error
        (Kind : Error_Kind; Message : String) return Version_Result
      is
      begin
         return Version_Result_Package.Error (Kind, Message);
      end Make_Version_Error;

      --  Core logic
      function Raw_Get_Version return Version_Result is
         File  : aliased File_Type;
         Guard : Text_File_Guard.Guard (File'Access);
         pragma Unreferenced (Guard);
         Line  : String (1 .. 32);
         Last  : Natural;
      begin
         if not Exists (Version_File)
           or else Kind (Version_File) /= Ordinary_File
         then
            return
              Version_Result_Package.Error
                (Not_Found_Error, "Version file not found: " & Version_File);
         end if;

         Open (File, In_File, Version_File);
         Get_Line (File, Line, Last);
         --  Guard handles Close on exit
         return
           Version_Result_Package.Ok
             (Application.Port.Inbound.Get_Version.Version_Strings
                .To_Bounded_String
                (Line (1 .. Last)));
      end Raw_Get_Version;

      --  Declarative exception-to-Result mapping
      package Try_Get_Version is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => Error_Kind,
         Result_Type        => Version_Result,
         Make_Error         => Make_Version_Error,
         Default_Error_Kind => IO_Error,
         Action             => Raw_Get_Version);

      --  Name_Error -> Not_Found, all others -> IO_Error (default)
      Version_Mappings : constant Try_Get_Version.Mapping_Array :=
        [(Ada.Text_IO.Name_Error'Identity, Not_Found_Error)];
   begin
      return Try_Get_Version.Run (Version_Mappings);
   end Get_Version;

   --  ========================================================================
   --  5. Find_My_Id (GPT-5 Pattern: Uses port's canonical Result type)
   --  ========================================================================

   function Find_My_Id return Application.Port.Inbound.Find_My_Id.Result is
      use Ada.Directories;
      use TZif.Application.Port.Inbound.Find_My_Id;
      Localtime_Path : constant String := "/etc/localtime";

      --  Make error Result from kind and message
      function Make_Find_My_Id_Error
        (Kind : Error_Kind; Message : String)
         return Application.Port.Inbound.Find_My_Id.Result
      is
      begin
         return Result_Zone_Id.Error (Kind, Message);
      end Make_Find_My_Id_Error;

      --  Core logic
      function Raw_Find_My_Id
         return Application.Port.Inbound.Find_My_Id.Result
      is
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
            if not Infrastructure.Platform.String_Result.Is_Ok (Link_Result)
            then
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
               for I in
                 Link_Target'First .. Link_Target'Last - Marker'Length + 1
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
                  Zone_Id_Start : constant Positive :=
                    Marker_Pos + Marker'Length;
                  Zone_Id_Str   : constant String   :=
                    Link_Target (Zone_Id_Start .. Link_Target'Last);
               begin
                  return
                    Result_Zone_Id.Ok
                      (Domain.Value_Object.Zone_Id.Make_Zone_Id (Zone_Id_Str));
               end;
            end;
         end;
      end Raw_Find_My_Id;

      --  Declarative exception handling
      package Try_Find_My_Id is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => Error_Kind,
         Result_Type        => Application.Port.Inbound.Find_My_Id.Result,
         Make_Error         => Make_Find_My_Id_Error,
         Default_Error_Kind => IO_Error,
         Action             => Raw_Find_My_Id);

      --  All exceptions map to IO_Error (default), so empty mappings
      Find_My_Id_Mappings : constant Try_Find_My_Id.Mapping_Array :=
        Try_Find_My_Id.Empty_Mappings;
   begin
      return Try_Find_My_Id.Run (Find_My_Id_Mappings);
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
                                 if not Zone_Id_Vectors.Is_Full (Zones) then
                                    Zone_Id_Vectors.Unchecked_Append
                                      (Zones, Make_Zone_Id (Zone_Name));
                                 end if;
                              exception
                                 when Constraint_Error =>
                                    --  DESIGN DECISION: Skip invalid zone
                                    --  names. Malformed entries silently
                                    --  skipped.
                                    null;
                              end;
                           end if;

                        when others =>
                           --  DESIGN DECISION: Skip non-directory/non-file
                           --  entries. Only regular files and directories
                           --  are relevant.
                           null;
                     end case;
                  end;
               end if;
            end;
         end loop;

         End_Search (Search);
      exception
         when Ada.Directories.Name_Error | Ada.Directories.Use_Error =>
            --  DESIGN DECISION: Skip inaccessible directories silently
            --  Some system directories may be permission-protected;
            --  continuing scan of other accessible directories
            null;
      end Scan_Directory;

      function Less_Than (Left, Right : Zone_Id_Type) return Boolean is
        (To_String (Left) < To_String (Right));

      procedure Sort_Zones is new Zone_Id_Vectors.Generic_Sort
        ("<" => Less_Than);

      --  Make error Result from kind and message
      function Make_List_Zones_Error
        (Kind : Error_Kind; Message : String)
         return List_All_Zones_Result
      is
      begin
         return List_All_Zones_Result_Package.Error (Kind, Message);
      end Make_List_Zones_Error;

      --  Core logic
      function Raw_List_All_Zones return List_All_Zones_Result is
      begin
         if not Exists (Path_Str) or else Kind (Path_Str) /= Directory then
            return
              List_All_Zones_Result_Package.Error
                (Not_Found_Error, "Source path not found: " & Path_Str);
         end if;

         Scan_Directory (Path_Str);

         Sort_Zones (Zones);
         if Descending then
            Zone_Id_Vectors.Reverse_Elements (Zones);
         end if;

         return List_All_Zones_Result_Package.Ok (Zones);
      end Raw_List_All_Zones;

      --  Declarative exception handling
      package Try_List_All_Zones is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => Error_Kind,
         Result_Type        => List_All_Zones_Result,
         Make_Error         => Make_List_Zones_Error,
         Default_Error_Kind => IO_Error,
         Action             => Raw_List_All_Zones);

      --  All exceptions map to IO_Error (default), so empty mappings
      List_Zones_Mappings : constant Try_List_All_Zones.Mapping_Array :=
        Try_List_All_Zones.Empty_Mappings;
   begin
      return Try_List_All_Zones.Run (List_Zones_Mappings);
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
                           --  DESIGN DECISION: Skip non-directory/non-file
                           --  entries. Only regular files and directories
                           --  are relevant.
                           null;
                     end case;
                  end;
               end if;
            end;
         end loop;

         End_Search (Search);
      exception
         when Ada.Directories.Name_Error | Ada.Directories.Use_Error =>
            --  DESIGN DECISION: Skip inaccessible directories silently.
            --  Some system directories may be permission-protected;
            --  continuing scan of other accessible directories
            null;
      end Scan_Directory;

      --  Make error Result from kind and message
      function Make_Find_Pattern_Error
        (Kind : Error_Kind; Message : String)
         return Find_By_Pattern_Result
      is
      begin
         return Find_By_Pattern_Result_Package.Error (Kind, Message);
      end Make_Find_Pattern_Error;

      --  Core logic
      function Raw_Find_By_Pattern return Find_By_Pattern_Result is
      begin
         --  Scan all search paths
         for Path of Search_Paths loop
            if Exists (Path.all) and then Kind (Path.all) = Directory then
               Scan_Directory (Path.all);
            end if;
         end loop;

         return
           Find_By_Pattern_Result_Package.Ok (Domain.Value_Object.Unit.Unit);
      end Raw_Find_By_Pattern;

      --  Declarative exception handling
      package Try_Find_By_Pattern is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => Error_Kind,
         Result_Type        => Find_By_Pattern_Result,
         Make_Error         => Make_Find_Pattern_Error,
         Default_Error_Kind => IO_Error,
         Action             => Raw_Find_By_Pattern);

      --  All exceptions map to IO_Error (default), so empty mappings
      Find_Pattern_Mappings : constant Try_Find_By_Pattern.Mapping_Array :=
        Try_Find_By_Pattern.Empty_Mappings;
   begin
      return Try_Find_By_Pattern.Run (Find_Pattern_Mappings);
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
                           --  DESIGN DECISION: Skip non-directory/non-file
                           --  entries. Only regular files and directories
                           --  are relevant.
                           null;
                     end case;
                  end;
               end if;
            end;
         end loop;

         End_Search (Search);
      exception
         when Ada.Directories.Name_Error | Ada.Directories.Use_Error =>
            --  DESIGN DECISION: Skip inaccessible directories silently.
            --  Some system directories may be permission-protected;
            --  continuing scan of other accessible directories
            null;
      end Scan_Directory;

      --  Make error Result from kind and message
      function Make_Find_Region_Error
        (Kind : Error_Kind; Message : String)
         return Find_By_Region_Result
      is
      begin
         return Find_By_Region_Result_Package.Error (Kind, Message);
      end Make_Find_Region_Error;

      --  Core logic
      function Raw_Find_By_Region return Find_By_Region_Result is
      begin
         for Path of Search_Paths loop
            if Exists (Path.all) and then Kind (Path.all) = Directory then
               Scan_Directory (Path.all);
            end if;
         end loop;

         return
           Find_By_Region_Result_Package.Ok (Domain.Value_Object.Unit.Unit);
      end Raw_Find_By_Region;

      --  Declarative exception handling
      package Try_Find_By_Region is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => Error_Kind,
         Result_Type        => Find_By_Region_Result,
         Make_Error         => Make_Find_Region_Error,
         Default_Error_Kind => IO_Error,
         Action             => Raw_Find_By_Region);

      --  All exceptions map to IO_Error (default), so empty mappings
      Find_Region_Mappings : constant Try_Find_By_Region.Mapping_Array :=
        Try_Find_By_Region.Empty_Mappings;
   begin
      return Try_Find_By_Region.Run (Find_Region_Mappings);
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
                           --  DESIGN DECISION: Skip non-directory/non-file
                           --  entries. Only regular files and directories
                           --  are relevant.
                           null;
                     end case;
                  end;
               end if;
            end;
         end loop;

         End_Search (Search);
      exception
         when Ada.Directories.Name_Error | Ada.Directories.Use_Error =>
            --  DESIGN DECISION: Skip inaccessible directories silently.
            --  Some system directories may be permission-protected;
            --  continuing scan of other accessible directories
            null;
      end Scan_Directory;

      --  Make error Result from kind and message
      function Make_Find_Regex_Error
        (Kind : Error_Kind; Message : String)
         return Find_By_Regex_Result
      is
      begin
         return Find_By_Regex_Result_Package.Error (Kind, Message);
      end Make_Find_Regex_Error;

      --  Core logic
      function Raw_Find_By_Regex return Find_By_Regex_Result is
         Pattern : constant Pattern_Matcher := Compile (Regex_Str);
      begin
         for Path of Search_Paths loop
            if Exists (Path.all) and then Kind (Path.all) = Directory then
               Scan_Directory (Path.all, "", Pattern);
            end if;
         end loop;

         return
           Find_By_Regex_Result_Package.Ok (Domain.Value_Object.Unit.Unit);
      end Raw_Find_By_Regex;

      --  Declarative exception handling
      package Try_Find_By_Regex is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => Error_Kind,
         Result_Type        => Find_By_Regex_Result,
         Make_Error         => Make_Find_Regex_Error,
         Default_Error_Kind => IO_Error,
         Action             => Raw_Find_By_Regex);

      --  Expression_Error -> Validation_Error; others -> IO_Error
      Find_Regex_Mappings : constant Try_Find_By_Regex.Mapping_Array :=
        [(Expression_Error'Identity, Validation_Error)];
   begin
      return Try_Find_By_Regex.Run (Find_Regex_Mappings);
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
               --  DESIGN DECISION: Skip inaccessible directories silently
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
            if not Error_Vectors.Is_Full (Data.Errors) then
               Error_Vectors.Unchecked_Append
                 (Data.Errors,
                  Error_Type'
                    (Kind    => IO_Error,
                     Message =>
                       Error_Strings.To_Bounded_String
                         ("Path not found: " & Path)));
            end if;
            return;
         end if;

         if Kind (Path) /= Directory then
            if not Error_Vectors.Is_Full (Data.Errors) then
               Error_Vectors.Unchecked_Append
                 (Data.Errors,
                  Error_Type'
                    (Kind    => Validation_Error,
                     Message =>
                       Error_Strings.To_Bounded_String
                         ("Not a directory: " & Path)));
            end if;
            return;
         end if;

         --  Check for VERSION file
         declare
            Version_File : constant String := Path & "/+VERSION";
         begin
            if not Exists (Version_File) then
               if not Error_Vectors.Is_Full (Data.Errors) then
                  Error_Vectors.Unchecked_Append
                    (Data.Errors,
                     Error_Type'
                       (Kind    => Validation_Error,
                        Message =>
                          Error_Strings.To_Bounded_String
                            ("No +VERSION file in: " & Path)));
               end if;
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
            Is_Duplicate : Boolean                     := False;
         begin
            --  Check for duplicates by path using index-based iteration
            for I in 1 .. Source_Info_Vectors.Length (Data.Sources) loop
               declare
                  Existing : constant Source_Info_Type :=
                    Source_Info_Vectors.Unchecked_Element (Data.Sources, I);
               begin
                  if Get_Path (Existing) = Path_Val then
                     Is_Duplicate := True;
                     exit;
                  end if;
               end;
            end loop;

            if not Is_Duplicate
              and then not Source_Info_Vectors.Is_Full (Data.Sources)
            then
               Source_Info_Vectors.Unchecked_Append (Data.Sources, Source);
            end if;
         end;

      exception
         when E : others =>
            if not Error_Vectors.Is_Full (Data.Errors) then
               Error_Vectors.Unchecked_Append
                 (Data.Errors,
                  Error_Type'
                    (Kind    => IO_Error,
                     Message =>
                       Error_Strings.To_Bounded_String
                         ("Error scanning " & Path & ": " &
                          Ada.Exceptions.Exception_Message (E))));
            end if;
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

      --  Sequential scanning using index-based iteration
      for I in 1 .. Path_Vectors.Length (Search_Paths) loop
         Scan_Path (Path_Vectors.Unchecked_Element (Search_Paths, I));
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
      use Ada.Text_IO;
      use TZif.Application.Port.Inbound.Load_Source;
      Path_Str : constant String :=
        Application.Port.Inbound.Load_Source.Path_Strings.To_String (Path);

      --  Scoped guard for file cleanup
      procedure Close_File (F : in out File_Type) renames Close;
      package Text_File_Guard is new Functional.Scoped.Conditional_Guard_For
        (Resource       => File_Type,
         Should_Release => Is_Open,
         Release        => Close_File);

      --  Make error Result from kind and message
      function Make_Load_Source_Error
        (Kind : Error_Kind; Message : String)
         return Load_Source_Result
      is
      begin
         return Load_Source_Result_Package.Error (Kind, Message);
      end Make_Load_Source_Error;

      --  Core logic
      function Raw_Load_Source return Load_Source_Result is
         ULID         : ULID_Type;
         Path_Val     : Path_String_Type;
         Version_File : constant String := Path_Str & "/+VERSION";
         Version_Str  : String (1 .. 32);
         Last         : Natural;
         File         : aliased File_Type;
         Guard        : Text_File_Guard.Guard (File'Access);
         pragma Unreferenced (Guard);
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

         ULID     := TZif.Infrastructure.ULID.Generate;
         Path_Val := Make_Path (Path_Str);

         if Exists (Version_File) and then Kind (Version_File) = Ordinary_File
         then
            Open (File, In_File, Version_File);
            Get_Line (File, Version_Str, Last);
            --  Guard handles Close on exit
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
                                 --  DESIGN DECISION: Skip non-directory/
                                 --  non-file entries. Only regular files
                                 --  and dirs are relevant.
                                 null;
                           end case;
                        end if;
                     end;
                  end loop;
                  End_Search (S);
               exception
                  when Ada.Directories.Name_Error
                    | Ada.Directories.Use_Error =>
                     --  DESIGN DECISION: Skip inaccessible directories
                     --  silently. Permission-protected dirs don't affect
                     --  count.
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
      end Raw_Load_Source;

      --  Declarative exception handling
      package Try_Load_Source is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => Error_Kind,
         Result_Type        => Load_Source_Result,
         Make_Error         => Make_Load_Source_Error,
         Default_Error_Kind => IO_Error,
         Action             => Raw_Load_Source);

      --  All exceptions map to IO_Error (default), so empty mappings
      Load_Source_Mappings : constant Try_Load_Source.Mapping_Array :=
        Try_Load_Source.Empty_Mappings;
   begin
      return Try_Load_Source.Run (Load_Source_Mappings);
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

      --  Make error Result from kind and message
      function Make_Validate_Error
        (Kind : Error_Kind; Message : String)
         return Validation_Result
      is
      begin
         return Validation_Result_Package.Error (Kind, Message);
      end Make_Validate_Error;

      --  Core logic
      function Raw_Validate_Source return Validation_Result is
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
               --  DESIGN DECISION: Inaccessible directories are invalid
               return Validation_Result_Package.Ok (False);
         end;
      end Raw_Validate_Source;

      --  Declarative exception handling
      package Try_Validate_Source is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => Error_Kind,
         Result_Type        => Validation_Result,
         Make_Error         => Make_Validate_Error,
         Default_Error_Kind => IO_Error,
         Action             => Raw_Validate_Source);

      --  All exceptions map to IO_Error (default), so empty mappings
      Validate_Mappings : constant Try_Validate_Source.Mapping_Array :=
        Try_Validate_Source.Empty_Mappings;
   begin
      return Try_Validate_Source.Run (Validate_Mappings);
   end Validate_Source;

end TZif.Infrastructure.Adapter.File_System.Repository;
