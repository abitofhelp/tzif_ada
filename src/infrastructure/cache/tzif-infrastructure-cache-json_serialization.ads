pragma Ada_2022;
--  ===========================================================================
--  Tzif.Infrastructure.Cache.Json_Serialization
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Json Serialization for performance optimization.
--
--  Key Types:
--    Cache_Header
--
--  Dependencies:
--    GNATCOLL.JSON
--    TZif.Domain.Value_Object.Source_Info
--    TZif.Domain.TZif_Data
--
--  ===========================================================================

with GNATCOLL.JSON;
with TZif.Domain.Value_Object.Source_Info;
with TZif.Domain.TZif_Data;
with TZif.Domain.Value_Object.TZif_Header;
with TZif.Domain.Value_Object.Transition;
with TZif.Domain.Value_Object.Timezone_Type;

package TZif.Infrastructure.Cache.JSON_Serialization is

   use TZif.Domain.Value_Object.Source_Info;
   use TZif.Domain.TZif_Data;
   use TZif.Domain.Value_Object.TZif_Header;
   use TZif.Domain.Value_Object.Transition;
   use TZif.Domain.Value_Object.Timezone_Type;
   use GNATCOLL.JSON;

   --  ========================================================================
   --  Source_Info Serialization
   --  ========================================================================

   function To_JSON (Source : Source_Info_Type) return JSON_Value;
   function From_JSON (J : JSON_Value) return Source_Info_Type;

   --  ========================================================================
   --  TZif_Data Serialization
   --  ========================================================================

   function To_JSON (Data : TZif_Data_Type) return JSON_Value;
   function From_JSON (J : JSON_Value) return TZif_Data_Type;

   --  Helper serialization functions for TZif_Data components
   function To_JSON (Header : TZif_Header_Type) return JSON_Value;
   function From_JSON (J : JSON_Value) return TZif_Header_Type;

   function To_JSON (Trans : Transition_Type) return JSON_Value;
   function From_JSON (J : JSON_Value) return Transition_Type;

   function To_JSON (TZ_Type : Timezone_Type_Record) return JSON_Value;
   function From_JSON (J : JSON_Value) return Timezone_Type_Record;

   function To_JSON (Leap : Leap_Second_Type) return JSON_Value;
   function From_JSON (J : JSON_Value) return Leap_Second_Type;

   --  ========================================================================
   --  Cache Header
   --  ========================================================================

   type Cache_Header is record
      Magic           : String (1 .. 10) := "TZIF_CACHE";
      Version         : Positive := 2;  -- Bumped to v2 for metadata fields
      Platform        : String (1 .. 10);  --  darwin/linux/windows
      Library_Version : String (1 .. 10) := "0.1.0     ";

      --  Metadata for cache integrity validation
      Expected_Sources : Natural := 0;  -- Number of sources when exported
      Expected_Zones   : Natural := 0;  -- Number of zones when exported
      Actual_Sources   : Natural := 0;  -- Sources actually written to cache
      Actual_Zones     : Natural := 0;  -- Zones actually written to cache
   end record;

   function To_JSON (Header : Cache_Header) return JSON_Value;
   function From_JSON (J : JSON_Value) return Cache_Header;

   --  ========================================================================
   --  Validation
   --  ========================================================================

   function Is_Valid_Cache_Header (J : JSON_Value) return Boolean;
   function Get_Platform
      return String;  --  Returns "darwin", "linux", or "windows"

end TZif.Infrastructure.Cache.JSON_Serialization;
