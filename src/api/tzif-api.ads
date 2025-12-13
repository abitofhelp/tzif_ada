pragma Ada_2022;
--  ===========================================================================
--  TZif.Api
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Public API facade - delegates all operations through Desktop.API.Facade
--    following the hexagonal architecture pattern.
--
--  Key Types:
--    Zone_Id_Type
--    Zone_Id_Result
--    Epoch_Seconds_Type
--    Error_Type
--    Error_Kind
--    ... and 28 more
--
--  Dependencies:
--    TZif.API.Desktop (canonical entry point via Facade)
--    TZif.Application.Port.Inbound.* (canonical types)
--
--  ===========================================================================

--  Import inbound ports for canonical types
with TZif.Application.Port.Inbound.Find_By_Id;
with TZif.Application.Port.Inbound.Find_By_Pattern;
with TZif.Application.Port.Inbound.Find_By_Region;
with TZif.Application.Port.Inbound.Find_By_Regex;
with TZif.Application.Port.Inbound.Find_My_Id;
with TZif.Application.Port.Inbound.Get_Transition_At_Epoch;
with TZif.Application.Port.Inbound.Get_Version;
with TZif.Application.Port.Inbound.List_All_Order_By_Id;
with TZif.Application.Port.Inbound.Discover_Sources;
with TZif.Application.Port.Inbound.Load_Source;
with TZif.Application.Port.Inbound.Validate_Source;
--  ROADMAP: Deferred pending user demand (see roadmap.md)
--  with TZif.Application.Port.Inbound.Import_Cache;
--  with TZif.Application.Port.Inbound.Export_Cache;

--  Import domain value objects and entities
with TZif.Domain.Error;
with TZif.Domain.Value_Object.Zone_Id;
with TZif.Domain.Value_Object.Zone_Id.Result;
with TZif.Domain.Value_Object.Epoch_Seconds;
with TZif.Domain.Value_Object.Source_Info;
with TZif.Domain.Entity.Zone;

package TZif.API is

   --  ========================================================================
   --  Core Value Objects (Re-exported from Domain)
   --  ========================================================================

   --  Zone ID type and operations
   subtype Zone_Id_Type is TZif.Domain.Value_Object.Zone_Id.Zone_Id_Type;

   --  Result type for validated Zone_Id construction
   subtype Zone_Id_Result is TZif.Domain.Value_Object.Zone_Id.Result.Result;

   --  Smart constructor: validates and creates Zone_Id, returns Result with
   --  error details on failure. Validation logic is in the value object.
   function Make_Zone_Id (Id : String) return Zone_Id_Result renames
     TZif.Domain.Value_Object.Zone_Id.Result.Make_Zone_Id;

   function To_String (Id : Zone_Id_Type) return String renames
     TZif.Domain.Value_Object.Zone_Id.To_String;

   --  Epoch time type
   subtype Epoch_Seconds_Type is
     TZif.Domain.Value_Object.Epoch_Seconds.Epoch_Seconds_Type;

   --  Error type and operations
   subtype Error_Type is TZif.Domain.Error.Error_Type;
   subtype Error_Kind is TZif.Domain.Error.Error_Kind;

   package Error_Strings renames TZif.Domain.Error.Error_Strings;

   --  Source metadata type
   subtype Source_Info_Type is
     TZif.Domain.Value_Object.Source_Info.Source_Info_Type;

   --  Zone entity type
   subtype Zone_Type is TZif.Domain.Entity.Zone.Zone_Type;

   --  ========================================================================
   --  Find By ID - Lookup zone by exact identifier
   --  ========================================================================

   --  Re-export canonical types from port
   package Find_By_Id_Port renames TZif.Application.Port.Inbound.Find_By_Id;

   subtype Zone_Result is Find_By_Id_Port.Find_By_Id_Result_Type;

   --  Result operations
   function Is_Ok (R : Zone_Result) return Boolean renames
     Find_By_Id_Port.Find_By_Id_Result.Is_Ok;

   function Is_Error (R : Zone_Result) return Boolean renames
     Find_By_Id_Port.Find_By_Id_Result.Is_Error;

   function Value (R : Zone_Result) return Zone_Type renames
     Find_By_Id_Port.Find_By_Id_Result.Value;

   --  Main operation: Find zone by ID
   function Find_By_Id (Id : Zone_Id_Type) return Zone_Result;

   --  ========================================================================
   --  Get Version - Query database version
   --  ========================================================================

   package Get_Version_Port renames TZif.Application.Port.Inbound.Get_Version;

   subtype Version_String is Get_Version_Port.Version_String;
   subtype Version_Result is Get_Version_Port.Version_Result;

   function Get_Version
     (Source : Source_Info_Type) return Version_Result;

   --  ========================================================================
   --  Find My ID - Discover local system timezone
   --  ========================================================================

   package Find_My_Id_Port renames TZif.Application.Port.Inbound.Find_My_Id;

   subtype My_Zone_Result is Find_My_Id_Port.Result;

   --  Result operations for My_Zone_Result
   function Is_Ok (R : My_Zone_Result) return Boolean renames
     Find_My_Id_Port.Result_Zone_Id.Is_Ok;

   function Is_Error (R : My_Zone_Result) return Boolean renames
     Find_My_Id_Port.Result_Zone_Id.Is_Error;

   function Value (R : My_Zone_Result) return Zone_Id_Type renames
     Find_My_Id_Port.Result_Zone_Id.Value;

   function Find_My_Id return My_Zone_Result;

   --  ========================================================================
   --  Get Transition At Epoch - Query timezone offset at specific time
   --  ========================================================================

   package Get_Transition_Port renames
     TZif.Application.Port.Inbound.Get_Transition_At_Epoch;

   subtype Zone_Id_String is Get_Transition_Port.Zone_Id_String;
   subtype Transition_Result is Get_Transition_Port.Get_Transition_Result;

   --  Helper to create Zone_Id_String
   function Make_Zone_Id_String (Id : String) return Zone_Id_String;

   --  Result operations for Transition_Result
   function Is_Ok (R : Transition_Result) return Boolean renames
     Get_Transition_Port.Get_Transition_Result_Package.Is_Ok;

   function Is_Error (R : Transition_Result) return Boolean renames
     Get_Transition_Port.Get_Transition_Result_Package.Is_Error;

   function Error_Info (R : Transition_Result) return Error_Type renames
     Get_Transition_Port.Get_Transition_Result_Package.Error_Info;

   function Get_Transition_At_Epoch
     (Id : Zone_Id_String; Epoch : Epoch_Seconds_Type)
      return Transition_Result;

   --  ========================================================================
   --  List All Zones - Enumerate all available timezones
   --  ========================================================================

   package List_Zones_Port renames
     TZif.Application.Port.Inbound.List_All_Order_By_Id;

   subtype Zone_List_Result is List_Zones_Port.List_All_Zones_Result;

   function List_All_Zones
     (Source : Source_Info_Type; Descending : Boolean := False)
      return Zone_List_Result;

   --  ========================================================================
   --  Find By Pattern - Search zones by substring
   --  ========================================================================

   package Find_Pattern_Port renames
     TZif.Application.Port.Inbound.Find_By_Pattern;

   subtype Pattern_String is Find_Pattern_Port.Pattern_String;
   subtype Pattern_Callback is Find_Pattern_Port.Yield_Callback_Access;
   subtype Pattern_Result is Find_Pattern_Port.Find_By_Pattern_Result;

   function Find_By_Pattern
     (Pattern : Pattern_String; Yield : Pattern_Callback)
      return Pattern_Result;

   --  ========================================================================
   --  Find By Region - Search zones by geographic region
   --  ========================================================================

   package Find_Region_Port renames
     TZif.Application.Port.Inbound.Find_By_Region;

   subtype Region_String is Find_Region_Port.Region_String;
   subtype Region_Callback is Find_Region_Port.Yield_Callback_Access;
   subtype Region_Result is Find_Region_Port.Find_By_Region_Result;

   function Find_By_Region
     (Region : Region_String; Yield : Region_Callback)
      return Region_Result;

   --  ========================================================================
   --  Find By Regex - Search zones by regular expression
   --  ========================================================================

   package Find_Regex_Port renames TZif.Application.Port.Inbound.Find_By_Regex;

   subtype Regex_String is Find_Regex_Port.Regex_String;
   subtype Regex_Callback is Find_Regex_Port.Yield_Callback_Access;
   subtype Regex_Result is Find_Regex_Port.Find_By_Regex_Result;

   function Find_By_Regex
     (Regex : Regex_String; Yield : Regex_Callback) return Regex_Result;

   --  ========================================================================
   --  Source Management - Discover, load, and validate timezone sources
   --  ========================================================================

   package Discover_Port renames
     TZif.Application.Port.Inbound.Discover_Sources;

   subtype Path_List is Discover_Port.Path_List;
   subtype Discovery_Result is Discover_Port.Discovery_Result;

   function Discover_Sources
     (Search_Paths : Path_List) return Discovery_Result;

   package Load_Port renames TZif.Application.Port.Inbound.Load_Source;

   subtype Path_String is Load_Port.Path_String;
   subtype Load_Source_Result is Load_Port.Load_Source_Result;

   function Load_Source
     (Path : Path_String) return Load_Source_Result;

   package Validate_Port renames TZif.Application.Port.Inbound.Validate_Source;

   subtype Validate_Path_String is Validate_Port.Path_String;
   subtype Validation_Result is Validate_Port.Validation_Result;

   function Validate_Source
     (Path : Validate_Path_String) return Validation_Result;

   --  ========================================================================
   --  ROADMAP: Deferred pending user demand (see roadmap.md)
   --  ========================================================================
   --  Cache Management - Import and export timezone data caches
   --  ========================================================================

   --  package Import_Port renames TZif.Application.Port.Inbound.Import_Cache;

   --  subtype Import_Path_String is Import_Port.Path_String;
   --  subtype Import_Cache_Result is Import_Port.Import_Cache_Result;

   --  function Import_Cache
   --    (Path : Import_Path_String) return Import_Cache_Result;

   --  package Export_Port renames TZif.Application.Port.Inbound.Export_Cache;

   --  subtype Export_Path_String is Export_Port.Path_String;
   --  subtype Export_Cache_Result is Export_Port.Export_Cache_Result;

   --  function Export_Cache
   --    (Path : Export_Path_String; Overwrite : Boolean := False)
   --     return Export_Cache_Result;

end TZif.API;
