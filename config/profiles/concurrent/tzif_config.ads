pragma Ada_2022;
--  ==========================================================================
--  TZif.Config - Concurrent Profile
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Configuration profile for multi-threaded applications.
--    Optimized for concurrent access patterns on multi-core systems.
--
--  Target Platform:
--    - Linux / macOS / Windows (multi-core)
--    - RAM: 1+ GB
--    - Full Ada runtime with tasking
--
--  Design Philosophy:
--    Generous limits for high-throughput concurrent servers. Sized for
--    applications handling multiple timezones simultaneously across many
--    threads.
--  ==========================================================================

with Ada.Containers;

package TZif_Config is

   pragma Pure;

   use Ada.Containers;

   --  =======================================================================
   --  Profile Metadata
   --  =======================================================================

   Profile_Name : constant String := "concurrent";
   Target_Platform : constant String := "Multi-threaded Server";

   --  =======================================================================
   --  TZif Binary Parser Configuration
   --  =======================================================================

   --  Maximum length of timezone identifiers (e.g., "America/Los_Angeles")
   --  IANA maximum: 30 characters
   --  Concurrent profile: 64 characters (matches standard)
   Max_Zone_ID_Length : constant := 64;

   --  Maximum length of timezone abbreviations (e.g., "PDT", "EST")
   --  IANA maximum: 9 characters
   --  Concurrent profile: 16 characters
   Max_Abbreviation_Length : constant := 16;

   --  Maximum transitions per zone (DST changes over time)
   --  Concurrent profile: 1000 transitions (covers >200 years)
   Max_Transitions_Per_Zone : constant := 1_000;

   --  Maximum timezone types per zone (different UTC offsets)
   --  Concurrent profile: 50 types
   Max_Types_Per_Zone : constant := 50;

   --  Maximum leap seconds in TZif file
   --  Concurrent profile: 100 (future-proof)
   Max_Leap_Seconds : constant := 100;

   --  =======================================================================
   --  Collection Capacity Hints
   --  =======================================================================

   --  These are capacity hints for bounded containers.
   --  Concurrent profile uses larger caches for multi-threaded access.

   Default_Cache_Capacity : constant Count_Type := 200;
   Default_Zone_Capacity : constant Count_Type := 500;

   --  =======================================================================
   --  Application Layer Bounded Vector Capacities
   --  =======================================================================
   --  These capacities support SPARK-compatible bounded vectors in the
   --  application layer, replacing unbounded Ada.Containers.Vectors.

   --  Maximum search paths for source discovery
   Max_Search_Paths : constant := 100;

   --  Maximum discovered timezone sources
   Max_Sources : constant := 100;

   --  Maximum non-fatal errors during discovery
   Max_Discovery_Errors : constant := 100;

   --  Maximum zone IDs in a list result
   Max_Zone_Ids : constant := 1_000;

end TZif_Config;
