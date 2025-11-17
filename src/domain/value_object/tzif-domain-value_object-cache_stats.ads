pragma Ada_2022;
--  ===========================================================================
--  Tzif.Domain.Value_Object.Cache_Stats
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Cache Stats value object - immutable domain data.
--
--  Responsibilities:
--    - Define Cache Stats type and operations
--
--  Key Types:
--    Import_Stats_Type
--    Export_Stats_Type
--
--  Dependencies:
--    Pure
--
--  ===========================================================================

package TZif.Domain.Value_Object.Cache_Stats with
  Pure
is

   --  ========================================================================
   --  Import Statistics Type
   --  ========================================================================

   --  Statistics from cache import operation
   type Import_Stats_Type is record
      --  Number of sources successfully loaded from cache
      Sources_Loaded : Natural := 0;

      --  Number of zones successfully loaded from cache
      Zones_Loaded : Natural := 0;

      --  Number of sources removed (paths no longer exist)
      Sources_Removed : Natural := 0;
   end record;

   --  ========================================================================
   --  Export Statistics Type
   --  ========================================================================

   --  Statistics from cache export operation
   type Export_Stats_Type is record
      --  Number of sources exported to cache
      Sources_Exported : Natural := 0;

      --  Number of zones exported to cache
      Zones_Exported : Natural := 0;
   end record;

end TZif.Domain.Value_Object.Cache_Stats;
