pragma Ada_2022;
pragma Unevaluated_Use_Of_Old (Allow);
--  ===========================================================================
--  TZif.Application.Port.Outbound.Zone_Repository
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Outbound port for Zone Repository.
--
--  Architecture:
--    Application layer port (hexagonal architecture).
--    Defines interface for infrastructure adapters.
--
--  Key Types:
--    Sort_Order_Type
--    Zone_Id_List
--    Source_Info_List
--    Repository_Zone_Result
--    Repository_Boolean_Result
--    ... and 5 more
--
--  Dependencies:
--    TZif.Domain.Entity.Zone
--    TZif.Domain.Value_Object.Zone_Id
--    TZif.Domain.Value_Object.TZif_Header
--    TZif.Domain.Types.Bounded_Vector
--
--  SPARK Compatibility:
--    Uses bounded vectors instead of Ada.Containers.Vectors for SPARK
--    formal verification. Capacity limits defined in TZif_Config.
--
--  ===========================================================================

with TZif_Config;
with TZif.Domain.Entity.Zone;
with TZif.Domain.Types.Bounded_Vector;
with TZif.Domain.Value_Object.Zone_Id;
with TZif.Domain.Value_Object.TZif_Header;
with TZif.Domain.Value_Object.Epoch_Seconds;
with TZif.Domain.Value_Object.Transition_Info;
with TZif.Domain.Value_Object.Source_Info;
with TZif.Domain.Error.Result;

package TZif.Application.Port.Outbound.Zone_Repository with
  SPARK_Mode => On,
  Preelaborate
is

   use TZif.Domain.Entity.Zone;
   use TZif.Domain.Value_Object.Zone_Id;
   use TZif.Domain.Value_Object.TZif_Header;
   use TZif.Domain.Value_Object.Epoch_Seconds;
   use TZif.Domain.Value_Object.Transition_Info;
   use TZif.Domain.Value_Object.Source_Info;

   --  ========================================================================
   --  Sort Order Type
   --  ========================================================================

   type Sort_Order_Type is (Ascending, Descending);

   --  ========================================================================
   --  Zone ID Collection Type - SPARK-compatible bounded
   --  ========================================================================

   package Zone_Id_Vectors is new TZif.Domain.Types.Bounded_Vector
     (Element_Type  => Zone_Id_Type,
      Capacity      => TZif_Config.Max_Zone_Ids,
      Default_Value => Default_Zone_Id);

   subtype Zone_Id_List is Zone_Id_Vectors.Vector;

   --  ========================================================================
   --  Source Info Collection Type - SPARK-compatible bounded
   --  ========================================================================

   package Source_Info_Vectors is new TZif.Domain.Types.Bounded_Vector
     (Element_Type  => Source_Info_Type,
      Capacity      => TZif_Config.Max_Sources,
      Default_Value => Default_Source_Info);

   subtype Source_Info_List is Source_Info_Vectors.Vector;

   --  ========================================================================
   --  Result Types
   --  ========================================================================

   package Zone_Result is new Domain.Error.Result.Generic_Result (Zone_Type);
   subtype Repository_Zone_Result is Zone_Result.Result;

   package Boolean_Result is new Domain.Error.Result.Generic_Result (Boolean);
   subtype Repository_Boolean_Result is Boolean_Result.Result;

   package Version_Result is new Domain.Error.Result.Generic_Result
     (TZif_Version_Type);
   subtype Repository_Version_Result is Version_Result.Result;

   package Transition_Info_Result is new Domain.Error.Result.Generic_Result
     (Transition_Info_Type);
   subtype Repository_Transition_Info_Result is Transition_Info_Result.Result;

   package Zone_Id_Result is new Domain.Error.Result.Generic_Result
     (Zone_Id_Type);
   subtype Repository_Zone_Id_Result is Zone_Id_Result.Result;

   package Zone_List_Result is new Domain.Error.Result.Generic_Result
     (Zone_Id_List);
   subtype Repository_Zone_List_Result is Zone_List_Result.Result;

   package Source_List_Result is new Domain.Error.Result.Generic_Result
     (Source_Info_List);
   subtype Repository_Source_List_Result is Source_List_Result.Result;

   --  ========================================================================
   --  Repository Interface (Generic Port)
   --  ========================================================================

   generic
      --  Find zone by ID
      with function Find_By_Id
        (Id : Zone_Id_Type) return Repository_Zone_Result;

      --  Check if zone exists
      with function Exists
        (Id : Zone_Id_Type) return Repository_Boolean_Result;

      --  Get TZif version for a zone
      with function Get_Version
        (Id : Zone_Id_Type) return Repository_Version_Result;

      --  Get transition info at specific epoch time
      with function Get_Transition_At_Epoch
        (Id : Zone_Id_Type; Epoch_Time : Epoch_Seconds_Type)
         return Repository_Transition_Info_Result;

      --  Find local system timezone ID
      with function Find_My_Id return Repository_Zone_Id_Result;

      --  List all available timezone IDs
      with function List_All_Zones
        (Sort_Order : Sort_Order_Type) return Repository_Zone_List_Result;

      --  List all available timezone database sources
      with function List_Sources return Repository_Source_List_Result;

   package Repository_Port is

      --  Expose the injected operations
      function Find_Zone_By_Id
        (Id : Zone_Id_Type) return Repository_Zone_Result is
        (Find_By_Id (Id));

      function Zone_Exists
        (Id : Zone_Id_Type) return Repository_Boolean_Result is
        (Exists (Id));

      function Get_Zone_Version
        (Id : Zone_Id_Type) return Repository_Version_Result is
        (Get_Version (Id));

      function Get_Zone_Transition_At_Epoch
        (Id : Zone_Id_Type; Epoch_Time : Epoch_Seconds_Type)
         return Repository_Transition_Info_Result is
        (Get_Transition_At_Epoch (Id, Epoch_Time));

      function Find_Local_Zone_Id return Repository_Zone_Id_Result is
        (Find_My_Id);

      function List_Zones
        (Sort_Order : Sort_Order_Type := Ascending)
         return Repository_Zone_List_Result renames
        List_All_Zones;

      function Get_Sources return Repository_Source_List_Result renames
        List_Sources;

   end Repository_Port;

end TZif.Application.Port.Outbound.Zone_Repository;
