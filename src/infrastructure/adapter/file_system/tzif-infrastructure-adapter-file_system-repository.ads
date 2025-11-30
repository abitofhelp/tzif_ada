pragma Ada_2022;
--  ===========================================================================
--  Tzif.Infrastructure.Adapter.File_System.Repository
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
--  Key Types:
--    Zone_Id_String
--    Pattern_String
--    Region_String
--    Regex_String
--    Path_String
--    ... and 4 more
--
--  Dependencies:
--    TZif.Domain.Error.Result
--    TZif.Domain.Value_Object.Epoch_Seconds
--    TZif.Domain.Value_Object.Source_Info
--
--  ===========================================================================

with Ada.Strings.Bounded;
with Ada.Containers.Vectors;
with TZif.Domain.Error.Result;
with TZif.Domain.Value_Object.Epoch_Seconds;
with TZif.Domain.Value_Object.Source_Info;

--  Import all inbound ports for canonical Result types (GPT-5 pattern)
with TZif.Application.Port.Inbound.Find_By_Id;
with TZif.Application.Port.Inbound.Find_By_Pattern;
with TZif.Application.Port.Inbound.Find_By_Region;
with TZif.Application.Port.Inbound.Find_By_Regex;
with TZif.Application.Port.Inbound.Find_My_Id;
with TZif.Application.Port.Inbound.Get_Version;
with TZif.Application.Port.Inbound.Get_Transition_At_Epoch;
with TZif.Application.Port.Inbound.List_All_Order_By_Id;
with TZif.Application.Port.Inbound.Discover_Sources;
with TZif.Application.Port.Inbound.Load_Source;
with TZif.Application.Port.Inbound.Validate_Source;

package TZif.Infrastructure.Adapter.File_System.Repository is

   use TZif.Domain.Value_Object.Epoch_Seconds;
   use TZif.Domain.Value_Object.Source_Info;

   --  ========================================================================
   --  String Types (must match use case signatures)
   --  ========================================================================

   Max_Zone_Id_Length : constant := 256;
   package Zone_Id_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max_Zone_Id_Length);
   subtype Zone_Id_String is Zone_Id_Strings.Bounded_String;

   Max_Pattern_Length : constant := 256;
   package Pattern_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max_Pattern_Length);
   subtype Pattern_String is Pattern_Strings.Bounded_String;

   Max_Region_Length : constant := 256;
   package Region_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max_Region_Length);
   subtype Region_String is Region_Strings.Bounded_String;

   Max_Regex_Length : constant := 512;
   package Regex_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max_Regex_Length);
   subtype Regex_String is Regex_Strings.Bounded_String;

   Max_Path_Length : constant := 4_096;
   package Path_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max_Path_Length);
   subtype Path_String is Path_Strings.Bounded_String;

   --  Version_String types removed - use canonical types from
   --  Application.Port.Inbound.Get_Version

   Max_Zone_Name_Length : constant := 256;
   package Zone_Name_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max_Zone_Name_Length);
   subtype Zone_Name_String is Zone_Name_Strings.Bounded_String;

   --  ========================================================================
   --  Collection Types
   --  ========================================================================

   --  Zone_Id_List removed - use canonical type from
   --  Application.Port.Inbound.List_All_Order_By_Id

   package Source_Info_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Source_Info_Type, "=" => "=");
   subtype Source_Info_List is Source_Info_Vectors.Vector;

   --  ========================================================================
   --  Result Types - ALL REMOVED (GPT-5 Pattern)
   --  ========================================================================
   --
   --  Per GPT-5 recommendation: Repository does NOT define Result
   --  types. All functions return canonical Result types from their
   --  respective ports. This eliminates nominal typing conflicts and
   --  ensures type compatibility.
   --
   --  Mapping:
   --    Find_By_Id â Application.Port.Inbound.Find_By_Id.Find_Zone_Result
   --    Find_My_Id â Application.Port.Inbound.Find_My_Id.Result
   --    Get_Version â Application.Port.Inbound.Get_Version.Version_Result
   --    Find_By_Pattern â
   --      Application.Port.Inbound.Find_By_Pattern.Find_By_Pattern_Result
   --    Find_By_Region â
   --      Application.Port.Inbound.Find_By_Region.Find_By_Region_Result
   --    Find_By_Regex â
   --      Application.Port.Inbound.Find_By_Regex.Find_By_Regex_Result
   --    Discover_Sources â
   --      Application.Port.Inbound.Discover_Sources.Discovery_Result
   --    Load_Source â
   --      Application.Port.Inbound.Load_Source.Load_Source_Result
   --    Validate_Source â
   --      Application.Port.Inbound.Validate_Source.Validation_Result
   --
   --  Note: Exists_By_Id deleted (obsolete), Get_Transition_At_Epoch
   --  & List_All_Order_By_Id ports need to be created before their
   --  functions can use canonical types.
   --  ========================================================================

   --  Temporary: These Result types remain until Exists_By_Id is
   --  deleted
   package Boolean_Result is new Domain.Error.Result.Generic_Result
     (T => Boolean);
   subtype Exists_Result is Boolean_Result.Result;
   --  DELETE when Exists_By_Id function deleted

   --  ========================================================================
   --  Repository Functions (13 commands)
   --  ========================================================================

   --  1. Find zone by exact ID (GPT-5 pattern: uses port's
   --     canonical types)
   function Find_By_Id
     (Id : Application.Port.Inbound.Find_By_Id.Zone_Id_Input_Type)
      return TZif.Application.Port.Inbound.Find_By_Id.Find_By_Id_Result_Type;

   --  2. Check if zone exists (DEPRECATED - use Find_By_Id
   --     instead)
   function Exists_By_Id (Id : Zone_Id_String) return Exists_Result;

   --  3. Get transition info at epoch (GPT-5 pattern: uses port's
   --     canonical type)
   function Get_Transition_At_Epoch
     (Id    : TZif.Application.Port.Inbound.Get_Transition_At_Epoch
        .Zone_Id_String;
      Epoch : Epoch_Seconds_Type)
      return Application.Port.Inbound.Get_Transition_At_Epoch
     .Get_Transition_Result;

   --  4. Get database version from source (GPT-5 pattern: uses
   --     port's canonical type)
   function Get_Version
     (Source : Source_Info_Type)
      return TZif.Application.Port.Inbound.Get_Version.Version_Result;

   --  5. Find local system timezone (GPT-5 pattern: uses port's
   --     canonical type)
   function Find_My_Id return TZif.Application.Port.Inbound.Find_My_Id.Result;

   --  6. List all zones in source (GPT-5 pattern: uses port's
   --     canonical type)
   function List_All_Zones
     (Source : Source_Info_Type; Descending : Boolean)
      return Application.Port.Inbound.List_All_Order_By_Id
     .List_All_Zones_Result;

   --  7. Find zones by substring pattern (GPT-5 pattern: uses
   --     port's canonical type)
   function Find_By_Pattern
     (Pattern : TZif.Application.Port.Inbound.Find_By_Pattern.Pattern_String;
      Yield   : Application.Port.Inbound.Find_By_Pattern.Yield_Callback_Access)
      return TZif.Application.Port.Inbound.Find_By_Pattern
     .Find_By_Pattern_Result;

   --  8. Find zones by region prefix (GPT-5 pattern: uses port's
   --     canonical type)
   function Find_By_Region
     (Region : TZif.Application.Port.Inbound.Find_By_Region.Region_String;
      Yield  : Application.Port.Inbound.Find_By_Region.Yield_Callback_Access)
      return TZif.Application.Port.Inbound.Find_By_Region
     .Find_By_Region_Result;

   --  9. Find zones by regex (GPT-5 pattern: uses port's
   --     canonical type)
   function Find_By_Regex
     (Regex : TZif.Application.Port.Inbound.Find_By_Regex.Regex_String;
      Yield : Application.Port.Inbound.Find_By_Regex.Yield_Callback_Access)
      return TZif.Application.Port.Inbound.Find_By_Regex.Find_By_Regex_Result;

   --  10. Discover timezone sources from paths (GPT-5 pattern:
   --      uses port's canonical type)
   function Discover_Sources
     (Search_Paths : Application.Port.Inbound.Discover_Sources.Path_List)
      return TZif.Application.Port.Inbound.Discover_Sources.Discovery_Result;

   --  11. Load source metadata from path (GPT-5 pattern: uses
   --      port's canonical type)
   function Load_Source
     (Path : Application.Port.Inbound.Load_Source.Path_String)
      return TZif.Application.Port.Inbound.Load_Source.Load_Source_Result;

   --  12. Validate source path (GPT-5 pattern: uses port's
   --      canonical type)
   function Validate_Source
     (Path : Application.Port.Inbound.Validate_Source.Path_String)
      return TZif.Application.Port.Inbound.Validate_Source.Validation_Result;

end TZif.Infrastructure.Adapter.File_System.Repository;
