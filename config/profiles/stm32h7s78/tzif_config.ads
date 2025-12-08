pragma Ada_2022;
--  ==========================================================================
--  TZif_Config - STM32H7S78-DK Profile
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Configuration optimized for STM32H7S78-DK Discovery Kit.
--    Leverages 620KB internal SRAM + 32MB external PSRAM.
--
--  Target Hardware:
--    - STM32H7S7L8H6H (Cortex-M7 @ 600 MHz)
--    - Internal SRAM: 620 KB
--    - External PSRAM: 256 Mbit (32 MB)
--    - External Flash: 1 Gbit (128 MB)
--
--  Design Philosophy:
--    High-performance embedded with external RAM:
--    - Internal SRAM: Critical parsing data structures
--    - External PSRAM: Full timezone data cache
--    - Can cache hundreds of parsed TZif files
--
--  Memory Strategy:
--    - Parser state: Internal SRAM (minimal)
--    - Parsed timezone data: External PSRAM (generous cache)
--    - Total library footprint: ~500KB - 2MB depending on cache
--  ==========================================================================

with Ada.Containers;

package TZif_Config is

   pragma Pure;

   use Ada.Containers;

   --  =======================================================================
   --  Profile Metadata
   --  =======================================================================

   Profile_Name : constant String := "stm32h7s78";
   Target_Platform : constant String := "STM32H7S78-DK";
   Target_RAM_KB : constant Positive := 620;
   External_PSRAM_MB : constant Positive := 32;

   --  =======================================================================
   --  TZif Binary Parser Configuration (Generous)
   --  =======================================================================

   --  Maximum length of timezone identifiers (e.g., "America/Los_Angeles")
   --  IANA maximum: 30 characters
   --  STM32H7S78 profile: 64 characters (high-performance, generous)
   Max_Zone_ID_Length : constant := 64;

   --  Maximum length of timezone abbreviations (e.g., "PDT", "EST")
   --  IANA maximum: 9 characters
   --  STM32H7S78 profile: 16 characters
   Max_Abbreviation_Length : constant := 16;

   --  Maximum transitions per zone (DST changes over time)
   --  High-performance board: 500 transitions (covers >100 years)
   Max_Transitions_Per_Zone : constant := 500;

   --  Maximum timezone types per zone (different UTC offsets)
   --  STM32H7S78 profile: 30 types (generous for complex zones)
   Max_Types_Per_Zone : constant := 30;

   --  Maximum leap seconds in TZif file
   --  Historical: 27 leap seconds since 1972
   --  STM32H7S78 profile: 75 (generous future-proofing)
   Max_Leap_Seconds : constant := 75;

   --  =======================================================================
   --  Collection Capacity Hints (External PSRAM Available)
   --  =======================================================================

   --  These are capacity hints for bounded containers.
   --  STM32H7S78 profile uses large caches leveraging external PSRAM.

   --  Cache capacity: Large (32MB PSRAM available)
   --  Can hold hundreds of parsed TZif files
   Default_Cache_Capacity : constant Count_Type := 300;

   --  Zone capacity: Large subset of IANA database
   Default_Zone_Capacity : constant Count_Type := 300;

   --  =======================================================================
   --  Application Layer Bounded Vector Capacities
   --  =======================================================================
   --  These capacities support SPARK-compatible bounded vectors in the
   --  application layer, replacing unbounded Ada.Containers.Vectors.

   --  Maximum search paths for source discovery
   Max_Search_Paths : constant := 50;

   --  Maximum discovered timezone sources
   Max_Sources : constant := 50;

   --  Maximum non-fatal errors during discovery
   Max_Discovery_Errors : constant := 50;

   --  Maximum zone IDs in a list result
   Max_Zone_Ids : constant := 600;

end TZif_Config;
