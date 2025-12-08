pragma Ada_2022;
--  ==========================================================================
--  TZif.Config - Bare Metal Profile
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Configuration profile for Zero Footprint Profile (ZFP) systems.
--    Minimal configuration for bare metal microcontrollers.
--
--  Target Hardware:
--    - STM32F4xx or similar (Cortex-M4 @ 100+ MHz)
--    - RAM: 128KB - 256KB
--    - ZFP runtime (no OS, minimal stdlib)
--
--  Design Philosophy:
--    Minimal sizing for single-timezone devices (e.g., GPS trackers that
--    only need one region). Extremely conservative to fit in constrained
--    memory environments.
--  ==========================================================================

with Ada.Containers;

package TZif_Config is

   pragma Pure;

   use Ada.Containers;

   --  =======================================================================
   --  Profile Metadata
   --  =======================================================================

   Profile_Name : constant String := "baremetal";
   Target_Platform : constant String := "Bare Metal (ZFP)";

   --  =======================================================================
   --  TZif Binary Parser Configuration
   --  =======================================================================

   --  Maximum length of timezone identifiers (e.g., "America/Los_Angeles")
   --  IANA maximum: 30 characters
   --  Bare metal profile: 40 characters (minimal margin)
   Max_Zone_ID_Length : constant := 40;

   --  Maximum length of timezone abbreviations (e.g., "PDT", "EST")
   --  IANA maximum: 9 characters
   --  Bare metal profile: 10 characters (minimal margin)
   Max_Abbreviation_Length : constant := 10;

   --  Maximum transitions per zone (DST changes over time)
   --  Bare metal need: 25-50 years = ~100 transitions
   --  Bare metal profile: 150 transitions (provides 50+ years)
   Max_Transitions_Per_Zone : constant := 150;

   --  Maximum timezone types per zone (different UTC offsets)
   --  Bare metal profile: 10 types (minimal but sufficient)
   Max_Types_Per_Zone : constant := 10;

   --  Maximum leap seconds in TZif file
   --  Historical: 27 leap seconds since 1972
   --  Bare metal profile: 30 (minimal future-proofing)
   Max_Leap_Seconds : constant := 30;

   --  =======================================================================
   --  Collection Capacity Hints
   --  =======================================================================

   --  These are capacity hints for bounded containers.
   --  Bare metal profile uses minimal sizes for tight memory budgets.

   Default_Cache_Capacity : constant Count_Type := 5;
   Default_Zone_Capacity : constant Count_Type := 5;

   --  =======================================================================
   --  Application Layer Bounded Vector Capacities
   --  =======================================================================
   --  These capacities support SPARK-compatible bounded vectors in the
   --  application layer, replacing unbounded Ada.Containers.Vectors.

   --  Maximum search paths for source discovery
   Max_Search_Paths : constant := 5;

   --  Maximum discovered timezone sources
   Max_Sources : constant := 5;

   --  Maximum non-fatal errors during discovery
   Max_Discovery_Errors : constant := 10;

   --  Maximum zone IDs in a list result
   Max_Zone_Ids : constant := 50;

end TZif_Config;
