pragma Ada_2022;
--  ===========================================================================
--  Tzif.Api.Operations
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Operations implementation.
--
--  ===========================================================================

with TZif.Infrastructure.Adapter.File_System.Repository;

package body TZif.API.Operations is

   package body Facade is

      -------------------------------------------------------------------
      --  (*) Find_By_Id - Delegated to All_Operations
      -------------------------------------------------------------------
      function Find_By_Id
        (Id : Zone_Id_Input_Type) return Find_By_Id_Result_Type
      is
         Result : Find_By_Id_Result_Type;
      begin
         Ops.Find_By_Id (Id, Result);
         return Result;
      end Find_By_Id;

      -------------------------------------------------------------------
      --  Get_Version - Calls Infrastructure (TODO: migrate to All_Ops)
      -------------------------------------------------------------------
      function Get_Version
        (Source : TZif.Domain.Value_Object.Source_Info.Source_Info_Type)
         return Version_Result
      is
      begin
         return TZif.Infrastructure.Adapter.File_System.Repository.Get_Version
                  (Source);
      end Get_Version;

      -------------------------------------------------------------------
      --  Find_My_Id - Calls Infrastructure (TODO: migrate to All_Ops)
      -------------------------------------------------------------------
      function Find_My_Id return My_Zone_Result is
      begin
         return TZif.Infrastructure.Adapter.File_System.Repository.Find_My_Id;
      end Find_My_Id;

      -------------------------------------------------------------------
      --  Get_Transition_At_Epoch - Calls Infrastructure
      -------------------------------------------------------------------
      function Get_Transition_At_Epoch
        (Id : Zone_Id_String; Epoch : TZif.Domain.Value_Object.Epoch_Seconds
           .Epoch_Seconds_Type) return Transition_Result
      is
      begin
         return TZif.Infrastructure.Adapter.File_System.Repository
                  .Get_Transition_At_Epoch
                  (Id, Epoch);
      end Get_Transition_At_Epoch;

      -------------------------------------------------------------------
      --  List_All_Zones - Calls Infrastructure
      -------------------------------------------------------------------
      function List_All_Zones
        (Source     : TZif.Domain.Value_Object.Source_Info.Source_Info_Type;
         Descending : Boolean := False) return Zone_List_Result
      is
      begin
         return TZif.Infrastructure.Adapter.File_System.Repository
                  .List_All_Zones
                  (Source, Descending);
      end List_All_Zones;

      -------------------------------------------------------------------
      --  Find_By_Pattern - Calls Infrastructure
      -------------------------------------------------------------------
      function Find_By_Pattern
        (Pattern : Pattern_String; Yield : Pattern_Callback)
         return Pattern_Result
      is
      begin
         return TZif.Infrastructure.Adapter.File_System.Repository
                  .Find_By_Pattern
                  (Pattern, Yield);
      end Find_By_Pattern;

      -------------------------------------------------------------------
      --  Find_By_Region - Calls Infrastructure
      -------------------------------------------------------------------
      function Find_By_Region
        (Region : Region_String; Yield : Region_Callback) return Region_Result
      is
      begin
         return TZif.Infrastructure.Adapter.File_System.Repository
                  .Find_By_Region
                  (Region, Yield);
      end Find_By_Region;

      -------------------------------------------------------------------
      --  Find_By_Regex - Calls Infrastructure
      -------------------------------------------------------------------
      function Find_By_Regex
        (Regex : Regex_String; Yield : Regex_Callback) return Regex_Result
      is
      begin
         return TZif.Infrastructure.Adapter.File_System.Repository
                  .Find_By_Regex
                  (Regex, Yield);
      end Find_By_Regex;

      -------------------------------------------------------------------
      --  (*) Discover_Sources - Delegated to All_Operations
      -------------------------------------------------------------------
      function Discover_Sources
        (Search_Paths : Discover_Path_List_Type) return Discovery_Result_Type
      is
         Result : Discovery_Result_Type;
      begin
         Ops.Discover_Sources (Search_Paths, Result);
         return Result;
      end Discover_Sources;

      -------------------------------------------------------------------
      --  Load_Source - Calls Infrastructure
      -------------------------------------------------------------------
      function Load_Source
        (Path : Load_Path_String) return Load_Source_Result
      is
      begin
         return TZif.Infrastructure.Adapter.File_System.Repository.Load_Source
                  (Path);
      end Load_Source;

      -------------------------------------------------------------------
      --  Validate_Source - Calls Infrastructure
      -------------------------------------------------------------------
      function Validate_Source
        (Path : Validate_Path_String) return Validation_Result
      is
      begin
         return TZif.Infrastructure.Adapter.File_System.Repository
                  .Validate_Source
                  (Path);
      end Validate_Source;

      -------------------------------------------------------------------
      --  NOTE: Deferred to post-1.0 pending user demand (see roadmap.md)
      -------------------------------------------------------------------
      --  (*) Import_Cache - Delegated to All_Operations
      -------------------------------------------------------------------
      --  function Import_Cache
      --    (Path : Import_Path_String) return Import_Cache_Result_Type
      --  is
      --     Result : Import_Cache_Result_Type;
      --  begin
      --     Ops.Import_Cache (Path, Result);
      --     return Result;
      --  end Import_Cache;

      -------------------------------------------------------------------
      --  (*) Export_Cache - Delegated to All_Operations
      -------------------------------------------------------------------
      --  function Export_Cache
      --    (Path : Export_Path_String; Overwrite : Boolean := False)
      --     return Export_Cache_Result_Type
      --  is
      --     Result : Export_Cache_Result_Type;
      --  begin
      --     Ops.Export_Cache (Path, Overwrite, Result);
      --     return Result;
      --  end Export_Cache;

   end Facade;

end TZif.API.Operations;
