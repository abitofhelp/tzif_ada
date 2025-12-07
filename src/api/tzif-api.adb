pragma Ada_2022;
--  ===========================================================================
--  TZif.Api
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Public API implementation - delegates all operations to Desktop.API
--    (instantiated Facade) following hexagonal architecture with DIP.
--
--  ===========================================================================

with TZif.API.Desktop;

package body TZif.API is

   --  ========================================================================
   --  Helper Functions
   --  ========================================================================

   function Make_Zone_Id_String (Id : String) return Zone_Id_String is
   begin
      return Get_Transition_Port.Zone_Id_Strings.To_Bounded_String (Id);
   end Make_Zone_Id_String;

   --  ========================================================================
   --  Find By ID - Lookup zone by exact identifier
   --  ========================================================================

   function Find_By_Id (Id : Zone_Id_Type) return Zone_Result is
   begin
      return TZif.API.Desktop.API.Find_By_Id (Id);
   end Find_By_Id;

   --  ========================================================================
   --  Get Version - Query database version
   --  ========================================================================

   function Get_Version
     (Source : Source_Info_Type) return Version_Result
   is
   begin
      return TZif.API.Desktop.API.Get_Version (Source);
   end Get_Version;

   --  ========================================================================
   --  Find My ID - Discover local system timezone
   --  ========================================================================

   function Find_My_Id return My_Zone_Result is
   begin
      return TZif.API.Desktop.API.Find_My_Id;
   end Find_My_Id;

   --  ========================================================================
   --  Get Transition At Epoch - Query timezone offset at specific time
   --  ========================================================================

   function Get_Transition_At_Epoch
     (Id : Zone_Id_String; Epoch : Epoch_Seconds_Type)
      return Transition_Result
   is
   begin
      return TZif.API.Desktop.API.Get_Transition_At_Epoch (Id, Epoch);
   end Get_Transition_At_Epoch;

   --  ========================================================================
   --  List All Zones - Enumerate all available timezones
   --  ========================================================================

   function List_All_Zones
     (Source : Source_Info_Type; Descending : Boolean := False)
      return Zone_List_Result
   is
   begin
      return TZif.API.Desktop.API.List_All_Zones (Source, Descending);
   end List_All_Zones;

   --  ========================================================================
   --  Find By Pattern - Search zones by substring
   --  ========================================================================

   function Find_By_Pattern
     (Pattern : Pattern_String; Yield : Pattern_Callback)
      return Pattern_Result
   is
   begin
      return TZif.API.Desktop.API.Find_By_Pattern (Pattern, Yield);
   end Find_By_Pattern;

   --  ========================================================================
   --  Find By Region - Search zones by geographic region
   --  ========================================================================

   function Find_By_Region
     (Region : Region_String; Yield : Region_Callback)
      return Region_Result
   is
   begin
      return TZif.API.Desktop.API.Find_By_Region (Region, Yield);
   end Find_By_Region;

   --  ========================================================================
   --  Find By Regex - Search zones by regular expression
   --  ========================================================================

   function Find_By_Regex
     (Regex : Regex_String; Yield : Regex_Callback) return Regex_Result
   is
   begin
      return TZif.API.Desktop.API.Find_By_Regex (Regex, Yield);
   end Find_By_Regex;

   --  ========================================================================
   --  Source Management - Discover, load, and validate timezone sources
   --  ========================================================================

   function Discover_Sources
     (Search_Paths : Path_List) return Discovery_Result
   is
   begin
      return TZif.API.Desktop.API.Discover_Sources (Search_Paths);
   end Discover_Sources;

   function Load_Source
     (Path : Path_String) return Load_Source_Result
   is
   begin
      return TZif.API.Desktop.API.Load_Source (Path);
   end Load_Source;

   function Validate_Source
     (Path : Validate_Path_String) return Validation_Result
   is
   begin
      return TZif.API.Desktop.API.Validate_Source (Path);
   end Validate_Source;

   --  ========================================================================
   --  ROADMAP: Deferred pending user demand (see roadmap.md)
   --  ========================================================================
   --  Cache Management - Import and export timezone data caches
   --  ========================================================================

   --  function Import_Cache
   --    (Path : Import_Path_String) return Import_Cache_Result
   --  is
   --  begin
   --     return TZif.API.Desktop.API.Import_Cache (Path);
   --  end Import_Cache;

   --  function Export_Cache
   --    (Path : Export_Path_String; Overwrite : Boolean := False)
   --     return Export_Cache_Result
   --  is
   --  begin
   --     return TZif.API.Desktop.API.Export_Cache (Path, Overwrite);
   --  end Export_Cache;

end TZif.API;
