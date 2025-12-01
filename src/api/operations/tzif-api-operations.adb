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
      --  (*) Get_Version - Delegated to All_Operations
      -------------------------------------------------------------------
      function Get_Version
        (Source : TZif.Domain.Value_Object.Source_Info.Source_Info_Type)
         return Version_Result
      is
         Result : Version_Result;
      begin
         Ops.Get_Version (Source, Result);
         return Result;
      end Get_Version;

      -------------------------------------------------------------------
      --  (*) Find_My_Id - Delegated to All_Operations
      -------------------------------------------------------------------
      function Find_My_Id return My_Zone_Result is
         Result : My_Zone_Result;
      begin
         Ops.Find_My_Id (Result);
         return Result;
      end Find_My_Id;

      -------------------------------------------------------------------
      --  (*) Get_Transition_At_Epoch - Delegated to All_Operations
      -------------------------------------------------------------------
      function Get_Transition_At_Epoch
        (Id : Zone_Id_String; Epoch : TZif.Domain.Value_Object.Epoch_Seconds
           .Epoch_Seconds_Type) return Transition_Result
      is
         Result : Transition_Result;
      begin
         Ops.Get_Transition_At_Epoch (Id, Epoch, Result);
         return Result;
      end Get_Transition_At_Epoch;

      -------------------------------------------------------------------
      --  (*) List_All_Zones - Delegated to All_Operations
      -------------------------------------------------------------------
      function List_All_Zones
        (Source     : TZif.Domain.Value_Object.Source_Info.Source_Info_Type;
         Descending : Boolean := False) return Zone_List_Result
      is
         Result : Zone_List_Result;
      begin
         Ops.List_All_Zones (Source, Descending, Result);
         return Result;
      end List_All_Zones;

      -------------------------------------------------------------------
      --  (*) Find_By_Pattern - Delegated to All_Operations
      -------------------------------------------------------------------
      function Find_By_Pattern
        (Pattern : Pattern_String; Yield : Pattern_Callback)
         return Pattern_Result
      is
         Result : Pattern_Result;
      begin
         Ops.Find_By_Pattern (Pattern, Yield, Result);
         return Result;
      end Find_By_Pattern;

      -------------------------------------------------------------------
      --  (*) Find_By_Region - Delegated to All_Operations
      -------------------------------------------------------------------
      function Find_By_Region
        (Region : Region_String; Yield : Region_Callback) return Region_Result
      is
         Result : Region_Result;
      begin
         Ops.Find_By_Region (Region, Yield, Result);
         return Result;
      end Find_By_Region;

      -------------------------------------------------------------------
      --  (*) Find_By_Regex - Delegated to All_Operations
      -------------------------------------------------------------------
      function Find_By_Regex
        (Regex : Regex_String; Yield : Regex_Callback) return Regex_Result
      is
         Result : Regex_Result;
      begin
         Ops.Find_By_Regex (Regex, Yield, Result);
         return Result;
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
      --  (*) Load_Source - Delegated to All_Operations
      -------------------------------------------------------------------
      function Load_Source
        (Path : Load_Path_String) return Load_Source_Result
      is
         Result : Load_Source_Result;
      begin
         Ops.Load_Source (Path, Result);
         return Result;
      end Load_Source;

      -------------------------------------------------------------------
      --  (*) Validate_Source - Delegated to All_Operations
      -------------------------------------------------------------------
      function Validate_Source
        (Path : Validate_Path_String) return Validation_Result
      is
         Result : Validation_Result;
      begin
         Ops.Validate_Source (Path, Result);
         return Result;
      end Validate_Source;

      -------------------------------------------------------------------
      --  ROADMAP: Deferred pending user demand (see roadmap.md)
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
