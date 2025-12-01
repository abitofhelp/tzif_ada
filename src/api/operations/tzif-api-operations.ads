pragma Ada_2022;
--  ===========================================================================
--  Tzif.Api.Operations
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Operations interface and type definitions.
--
--  Key Types:
--    Zone_Id_Input_Type
--    Find_By_Id_Result_Type
--    Discover_Path_List_Type
--    Discovery_Result_Type
--    Import_Path_String
--    ... and 3 more
--
--  Dependencies:
--    TZif.Application.Operations
--    SPARK_Mode => On
--    package Ops is new TZif.Application.Operations.All_Operations (<>)
--
--  ===========================================================================

with TZif.Application.Operations;
--  with TZif.Application.Port.Inbound.Find_By_Id;  -- Types from Operations
with TZif.Application.Port.Inbound.Get_Version;
with TZif.Application.Port.Inbound.Find_My_Id;
with TZif.Application.Port.Inbound.Get_Transition_At_Epoch;
with TZif.Application.Port.Inbound.List_All_Order_By_Id;
with TZif.Application.Port.Inbound.Find_By_Pattern;
with TZif.Application.Port.Inbound.Find_By_Region;
with TZif.Application.Port.Inbound.Find_By_Regex;
--  with TZif.Application.Port.Inbound.Discover_Sources;
with TZif.Application.Port.Inbound.Load_Source;
with TZif.Application.Port.Inbound.Validate_Source;
--  with TZif.Application.Port.Inbound.Import_Cache;
--  with TZif.Application.Port.Inbound.Export_Cache;
with TZif.Domain.Value_Object.Source_Info;
with TZif.Domain.Value_Object.Epoch_Seconds;

package TZif.API.Operations is

   --  Bring in the canonical types from ports via the operations package.
   --  Any All_Operations instance will be parameterized on the same types.

   generic
      with package Ops is new TZif.Application.Operations.All_Operations (<>);
   package Facade is
      --  ====================================================================
      --  Canonical Type Aliases (from Inbound Ports)
      --  ====================================================================

      --  Find_By_Id types
      subtype Zone_Id_Input_Type is
        TZif.Application.Operations.Zone_Id_Input_Type;
      subtype Find_By_Id_Result_Type is
        TZif.Application.Operations.Find_By_Id_Result_Type;

      --  Discover_Sources types
      subtype Discover_Path_List_Type is
        TZif.Application.Operations.Discover_Path_List_Type;
      subtype Discovery_Result_Type is
        TZif.Application.Operations.Discovery_Result_Type;

      --  ====================================================================
      --  ROADMAP: Deferred pending user demand (see roadmap.md)
      --  ====================================================================
      --  Import/Export Cache types
      --  subtype Import_Path_String is
      --    TZif.Application.Operations.Import_Path_String;
      --  subtype Import_Cache_Result_Type is
      --    TZif.Application.Operations.Import_Cache_Result_Type;
      --  subtype Export_Path_String is
      --    TZif.Application.Operations.Export_Path_String;
      --  subtype Export_Cache_Result_Type is
      --    TZif.Application.Operations.Export_Cache_Result_Type;

      --  Get_Version types
      subtype Version_String is
        TZif.Application.Port.Inbound.Get_Version.Version_String;
      subtype Version_Result is
        TZif.Application.Port.Inbound.Get_Version.Version_Result;

      --  Find_My_Id types
      subtype My_Zone_Result is
        TZif.Application.Port.Inbound.Find_My_Id.Result;

      --  Get_Transition types
      subtype Zone_Id_String is
        TZif.Application.Port.Inbound.Get_Transition_At_Epoch.Zone_Id_String;
      subtype Transition_Result is
        TZif.Application.Port.Inbound.Get_Transition_At_Epoch
          .Get_Transition_Result;

      --  List_All_Zones types
      subtype Zone_List_Result is
        TZif.Application.Port.Inbound.List_All_Order_By_Id
          .List_All_Zones_Result;

      --  Find_By_Pattern types
      subtype Pattern_String is
        TZif.Application.Port.Inbound.Find_By_Pattern.Pattern_String;
      subtype Pattern_Callback is
        TZif.Application.Port.Inbound.Find_By_Pattern.Yield_Callback_Access;
      subtype Pattern_Result is
        TZif.Application.Port.Inbound.Find_By_Pattern.Find_By_Pattern_Result;

      --  Find_By_Region types
      subtype Region_String is
        TZif.Application.Port.Inbound.Find_By_Region.Region_String;
      subtype Region_Callback is
        TZif.Application.Port.Inbound.Find_By_Region.Yield_Callback_Access;
      subtype Region_Result is
        TZif.Application.Port.Inbound.Find_By_Region.Find_By_Region_Result;

      --  Find_By_Regex types
      subtype Regex_String is
        TZif.Application.Port.Inbound.Find_By_Regex.Regex_String;
      subtype Regex_Callback is
        TZif.Application.Port.Inbound.Find_By_Regex.Yield_Callback_Access;
      subtype Regex_Result is
        TZif.Application.Port.Inbound.Find_By_Regex.Find_By_Regex_Result;

      --  Load_Source types
      subtype Load_Path_String is
        TZif.Application.Port.Inbound.Load_Source.Path_String;
      subtype Load_Source_Result is
        TZif.Application.Port.Inbound.Load_Source.Load_Source_Result;

      --  Validate_Source types
      subtype Validate_Path_String is
        TZif.Application.Port.Inbound.Validate_Source.Path_String;
      subtype Validation_Result is
        TZif.Application.Port.Inbound.Validate_Source.Validation_Result;

      --  ====================================================================
      --  Operations (All 11 API operations)
      --  ====================================================================
      --
      --  All operations follow hexagonal architecture:
      --  API.Facade -> Inbound Port -> UseCase -> Domain/Outbound Ports
      --
      --  ====================================================================

      -------------------------------------------------------------------
      --  (*) Find_By_Id - Delegated to All_Operations
      -------------------------------------------------------------------
      function Find_By_Id
        (Id : Zone_Id_Input_Type) return Find_By_Id_Result_Type;

      -------------------------------------------------------------------
      --  (*) Get_Version - Delegated to All_Operations
      -------------------------------------------------------------------
      function Get_Version
        (Source : TZif.Domain.Value_Object.Source_Info.Source_Info_Type)
         return Version_Result;

      -------------------------------------------------------------------
      --  (*) Find_My_Id - Delegated to All_Operations
      -------------------------------------------------------------------
      function Find_My_Id return My_Zone_Result;

      -------------------------------------------------------------------
      --  (*) Get_Transition_At_Epoch - Delegated to All_Operations
      -------------------------------------------------------------------
      function Get_Transition_At_Epoch
        (Id : Zone_Id_String; Epoch : TZif.Domain.Value_Object.Epoch_Seconds
           .Epoch_Seconds_Type) return Transition_Result;

      -------------------------------------------------------------------
      --  (*) List_All_Zones - Delegated to All_Operations
      -------------------------------------------------------------------
      function List_All_Zones
        (Source     : TZif.Domain.Value_Object.Source_Info.Source_Info_Type;
         Descending : Boolean := False) return Zone_List_Result;

      -------------------------------------------------------------------
      --  (*) Find_By_Pattern - Delegated to All_Operations
      -------------------------------------------------------------------
      function Find_By_Pattern
        (Pattern : Pattern_String; Yield : Pattern_Callback)
         return Pattern_Result;

      -------------------------------------------------------------------
      --  (*) Find_By_Region - Delegated to All_Operations
      -------------------------------------------------------------------
      function Find_By_Region
        (Region : Region_String; Yield : Region_Callback)
         return Region_Result;

      -------------------------------------------------------------------
      --  (*) Find_By_Regex - Delegated to All_Operations
      -------------------------------------------------------------------
      function Find_By_Regex
        (Regex : Regex_String; Yield : Regex_Callback) return Regex_Result;

      -------------------------------------------------------------------
      --  (*) Discover_Sources - Delegated to All_Operations
      -------------------------------------------------------------------
      function Discover_Sources
        (Search_Paths : Discover_Path_List_Type) return Discovery_Result_Type;

      -------------------------------------------------------------------
      --  (*) Load_Source - Delegated to All_Operations
      -------------------------------------------------------------------
      function Load_Source
        (Path : Load_Path_String) return Load_Source_Result;

      -------------------------------------------------------------------
      --  (*) Validate_Source - Delegated to All_Operations
      -------------------------------------------------------------------
      function Validate_Source
        (Path : Validate_Path_String) return Validation_Result;

      -------------------------------------------------------------------
      --  ROADMAP: Deferred pending user demand (see roadmap.md)
      -------------------------------------------------------------------
      --  (*) Import_Cache - Delegated to All_Operations
      -------------------------------------------------------------------
      --  function Import_Cache
      --    (Path : Import_Path_String) return Import_Cache_Result_Type;

      -------------------------------------------------------------------
      --  (*) Export_Cache - Delegated to All_Operations
      -------------------------------------------------------------------
      --  function Export_Cache
      --    (Path : Export_Path_String; Overwrite : Boolean := False)
      --     return Export_Cache_Result_Type;

   end Facade;

end TZif.API.Operations;
