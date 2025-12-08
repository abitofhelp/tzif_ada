pragma Ada_2022;
--  ==========================================================================
--  TZif.Config - Standard Profile
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Configuration profile for desktop/server environments.
--    Maximum flexibility, no memory constraints.
--
--  Target Platform:
--    - Linux / macOS / Windows
--    - RAM: 1+ GB
--    - Full Ada runtime
--
--  Design Philosophy:
--    Generous limits suitable for desktop applications with no memory
--    pressure. All limits based on IANA tzdata maximums plus margins.
--  ==========================================================================

with Ada.Containers;

package TZif_Config is

   pragma Pure;

   use Ada.Containers;

   --  =======================================================================
   --  Profile Metadata
   --  =======================================================================

   Profile_Name    : constant String := "standard";
   Target_Platform : constant String := "Desktop/Server";

   --  =======================================================================
   --  TZif Binary Parser Configuration
   --  =======================================================================

   --  Maximum length of timezone identifiers (e.g., "America/Los_Angeles")
   --  IANA maximum: 30 characters
   --  Standard profile: 64 characters (generous margin)
   Max_Zone_ID_Length : constant := 64;

   --  Maximum length of timezone abbreviations (e.g., "PDT", "EST")
   --  IANA maximum: 9 characters
   --  Standard profile: 16 characters
   Max_Abbreviation_Length : constant := 16;

   --  Maximum length of filesystem paths
   --  POSIX PATH_MAX: typically 4096
   --  Standard profile: 4096 characters
   Max_Path_Length : constant := 4_096;

   --  Maximum transitions per zone (DST changes over time)
   --  Typical zone: 200-300 transitions for 100 years
   --  Standard profile: 1000 transitions (covers >200 years)
   Max_Transitions_Per_Zone : constant := 1_000;

   --  Maximum timezone types per zone (different UTC offsets)
   --  Typical zone: 5-10 types
   --  Standard profile: 50 types
   Max_Types_Per_Zone : constant := 50;

   --  Maximum leap seconds in TZif file
   --  Historical: 27 leap seconds since 1972
   --  Standard profile: 100 (future-proof)
   Max_Leap_Seconds : constant := 100;

   --  =======================================================================
   --  Collection Capacity Hints
   --  =======================================================================

   --  These are capacity hints for bounded containers.
   --  Standard profile uses generous sizes for flexibility.

   Default_Cache_Capacity : constant Count_Type := 100;
   Default_Zone_Capacity  : constant Count_Type := 500;

   --  =======================================================================
   --  Application Layer Bounded Vector Capacities
   --  =======================================================================
   --  These capacities support SPARK-compatible bounded vectors in the
   --  application layer, replacing unbounded Ada.Containers.Vectors.

   --  Maximum search paths for source discovery
   --  Typical usage: 1-10 paths
   --  Standard profile: 100 paths
   Max_Search_Paths : constant := 100;

   --  Maximum discovered timezone sources
   --  Typical system: 1-3 sources
   --  Standard profile: 100 sources
   Max_Sources : constant := 100;

   --  Maximum non-fatal errors during discovery
   --  Standard profile: 100 errors
   Max_Discovery_Errors : constant := 100;

   --  Maximum zone IDs in a list result
   --  IANA tzdata: ~600 zones
   --  Standard profile: 1000 zones (generous headroom)
   Max_Zone_Ids : constant := 1_000;

end TZif_Config;
