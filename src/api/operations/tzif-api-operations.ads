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

package TZif.API.Operations with
  SPARK_Mode => On
is

   --  Bring in the canonical types from ports via the operations package.
   --  Any All_Operations instance will be parameterized on the same types.

   generic
      with package Ops is new TZif.Application.Operations.All_Operations (<>);
   package Facade with
     SPARK_Mode => On
   is
      --  Public type aliases (canonical types)
      subtype Zone_Id_Input_Type is
        TZif.Application.Operations.Zone_Id_Input_Type;
      subtype Find_By_Id_Result_Type is
        TZif.Application.Operations.Find_By_Id_Result_Type;

      subtype Discover_Path_List_Type is
        TZif.Application.Operations.Discover_Path_List_Type;
      subtype Discovery_Result_Type is
        TZif.Application.Operations.Discovery_Result_Type;

      subtype Import_Path_String is
        TZif.Application.Operations.Import_Path_String;
      subtype Import_Cache_Result_Type is
        TZif.Application.Operations.Import_Cache_Result_Type;

      subtype Export_Path_String is
        TZif.Application.Operations.Export_Path_String;
      subtype Export_Cache_Result_Type is
        TZif.Application.Operations.Export_Cache_Result_Type;

      -------------------------------------------------------------------
      --  Find_By_Id
      -------------------------------------------------------------------
      procedure Find_By_Id
        (Id : Zone_Id_Input_Type; Result : out Find_By_Id_Result_Type);

      -------------------------------------------------------------------
      --  Discover_Sources
      -------------------------------------------------------------------
      procedure Discover_Sources
        (Search_Paths :     Discover_Path_List_Type;
         Result       : out Discovery_Result_Type);

      -------------------------------------------------------------------
      --  Import_Cache
      -------------------------------------------------------------------
      procedure Import_Cache
        (Path : Import_Path_String; Result : out Import_Cache_Result_Type);

      -------------------------------------------------------------------
      --  Export_Cache
      -------------------------------------------------------------------
      procedure Export_Cache
        (Path   :     Export_Path_String; Overwrite : Boolean;
         Result : out Export_Cache_Result_Type);

      --  Additional operations (Get_Version, Validate_Source, etc.) can be
      --  added here as they are migrated to the All_Operations generic.

   end Facade;

end TZif.API.Operations;
