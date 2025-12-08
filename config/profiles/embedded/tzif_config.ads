pragma Ada_2022;
--  ==========================================================================
--  TZif.Config - Embedded Profile
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Configuration profile for Ravenscar-compatible embedded systems.
--    Balanced configuration for memory-constrained devices.
--
--  Target Hardware:
--    - STM32F769 or similar (Cortex-M7 @ 200+ MHz)
--    - RAM: 512KB - 1MB
--    - Ravenscar runtime
--
--  Design Philosophy:
--    Conservative sizing with safety margins for GPS devices, IoT sensors,
--    and industrial control systems. Limits based on typical use cases
--    (small subset of timezones, 50-100 years of data).
--  ==========================================================================

with Ada.Containers;

package TZif_Config is

   pragma Pure;

   use Ada.Containers;

   --  =======================================================================
   --  Profile Metadata
   --  =======================================================================

   Profile_Name : constant String := "embedded";
   Target_Platform : constant String := "Embedded (Ravenscar)";

   --  =======================================================================
   --  TZif Binary Parser Configuration
   --  =======================================================================

   --  Maximum length of timezone identifiers (e.g., "America/Los_Angeles")
   --  IANA maximum: 30 characters
   --  Embedded profile: 48 characters (60% margin)
   Max_Zone_ID_Length : constant := 48;

   --  Maximum length of timezone abbreviations (e.g., "PDT", "EST")
   --  IANA maximum: 9 characters
   --  Embedded profile: 12 characters (33% margin)
   Max_Abbreviation_Length : constant := 12;

   --  Maximum transitions per zone (DST changes over time)
   --  Typical need for embedded: 50-100 years = ~200 transitions
   --  Embedded profile: 300 transitions (provides 100+ years)
   Max_Transitions_Per_Zone : constant := 300;

   --  Maximum timezone types per zone (different UTC offsets)
   --  Typical zone: 5-10 types
   --  Embedded profile: 20 types (conservative)
   Max_Types_Per_Zone : constant := 20;

   --  Maximum leap seconds in TZif file
   --  Historical: 27 leap seconds since 1972
   --  Embedded profile: 50 (future-proof for next 50 years)
   Max_Leap_Seconds : constant := 50;

   --  =======================================================================
   --  Collection Capacity Hints
   --  =======================================================================

   --  These are capacity hints for bounded containers.
   --  Embedded profile uses conservative sizes for memory efficiency.

   Default_Cache_Capacity : constant Count_Type := 10;
   Default_Zone_Capacity : constant Count_Type := 25;

   --  =======================================================================
   --  Application Layer Bounded Vector Capacities
   --  =======================================================================
   --  These capacities support SPARK-compatible bounded vectors in the
   --  application layer, replacing unbounded Ada.Containers.Vectors.

   --  Maximum search paths for source discovery
   Max_Search_Paths : constant := 10;

   --  Maximum discovered timezone sources
   Max_Sources : constant := 10;

   --  Maximum non-fatal errors during discovery
   Max_Discovery_Errors : constant := 25;

   --  Maximum zone IDs in a list result
   Max_Zone_Ids : constant := 100;

end TZif_Config;
