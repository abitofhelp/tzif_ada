pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.Cache.Zone_Cache
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Zone Cache for performance optimization.
--
--  Key Types:
--    Zone_Data_Option_Type
--    Zone_Cache_Entry
--    Zone_Data_Map_Type
--    Zone_Cache_Type
--
--  Dependencies:
--    TZif.Domain.Value_Object.Zone_Id
--    TZif.Domain.TZif_Data
--    Functional.Option
--
--  ===========================================================================

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Calendar;
with TZif.Domain.Value_Object.Zone_Id;
with TZif.Domain.TZif_Data;
with Functional.Option;

package TZif.Infrastructure.Cache.Zone_Cache with
  Elaborate_Body
is

   use TZif.Domain.Value_Object.Zone_Id;
   use TZif.Domain.TZif_Data;

   --  ========================================================================
   --  Option Type for TZif_Data (None if not in cache)
   --  ========================================================================

   package TZif_Data_Option is new Functional.Option (TZif_Data_Type);

   subtype Zone_Data_Option_Type is TZif_Data_Option.Option;

   --  ========================================================================
   --  Cache Entry (Data + LRU Metadata)
   --  ========================================================================

   type Zone_Cache_Entry is record
      Data          : TZif_Data_Type;
      Last_Accessed : Ada.Calendar.Time;
   end record;

   --  ========================================================================
   --  Hash Map for Zone Cache Entries
   --  ========================================================================

   function Hash_Zone_ID (ID : Zone_Id_Type) return Ada.Containers.Hash_Type;

   function Zone_ID_Equal (Left, Right : Zone_Id_Type) return Boolean;

   package Zone_Data_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Zone_Id_Type, Element_Type => Zone_Cache_Entry,
      Hash     => Hash_Zone_ID, Equivalent_Keys => Zone_ID_Equal);

   subtype Zone_Data_Map_Type is Zone_Data_Maps.Map;

   --  Maximum cache size (LRU eviction threshold)
   Max_Cache_Size : constant := 25;

   --  ========================================================================
   --  Zone_Cache Protected Type
   --  ========================================================================

   protected type Zone_Cache_Type is

      --  ===================================================================
      --  Lock-Free Read Operations (No Task Suspension)
      --  ===================================================================

      --  Check if zone data is in cache
      function Contains (ID : Zone_Id_Type) return Boolean;

      --  Get zone data from cache (returns None if not found)
      function Get (ID : Zone_Id_Type) return Zone_Data_Option_Type;

      --  Get all zone data as a map
      function Get_All return Zone_Data_Map_Type;

      --  Get number of zones in cache
      function Size return Natural;

      --  Check if cache is empty
      function Is_Empty return Boolean;

      --  ===================================================================
      --  Synchronized Write Operations (Serialized Access)
      --  ===================================================================

      --  Insert or update zone data in cache
      procedure Insert (ID : Zone_Id_Type; Data : TZif_Data_Type);

      --  Remove zone data from cache
      procedure Remove (ID : Zone_Id_Type);

      --  Clear all zone data from cache
      procedure Clear;

   private
      Zones : Zone_Data_Map_Type;
   end Zone_Cache_Type;

end TZif.Infrastructure.Cache.Zone_Cache;
