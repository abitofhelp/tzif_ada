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

package body TZif.API.Operations with
  SPARK_Mode => On
is

   package body Facade is
      -------------------------------------------------------------------
      --  Find_By_Id
      -------------------------------------------------------------------
      procedure Find_By_Id
        (Id : Zone_Id_Input_Type; Result : out Find_By_Id_Result_Type)
      is
      begin
         Ops.Find_By_Id (Id, Result);
      end Find_By_Id;

      -------------------------------------------------------------------
      --  Discover_Sources
      -------------------------------------------------------------------
      procedure Discover_Sources
        (Search_Paths :     Discover_Path_List_Type;
         Result       : out Discovery_Result_Type)
      is
      begin
         Ops.Discover_Sources (Search_Paths, Result);
      end Discover_Sources;

      -------------------------------------------------------------------
      --  Import_Cache
      -------------------------------------------------------------------
      procedure Import_Cache
        (Path : Import_Path_String; Result : out Import_Cache_Result_Type)
      is
      begin
         Ops.Import_Cache (Path, Result);
      end Import_Cache;

      -------------------------------------------------------------------
      --  Export_Cache
      -------------------------------------------------------------------
      procedure Export_Cache
        (Path   :     Export_Path_String; Overwrite : Boolean;
         Result : out Export_Cache_Result_Type)
      is
      begin
         Ops.Export_Cache (Path, Overwrite, Result);
      end Export_Cache;

   end Facade;

end TZif.API.Operations;
