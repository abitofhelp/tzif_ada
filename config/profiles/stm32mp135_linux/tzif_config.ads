pragma Ada_2022;
--  ==========================================================================
--  TZif_Config - STM32MP135F-DK Profile (Linux)
--  ==========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Configuration for STM32MP135F-DK running Linux (OpenSTLinux).
--    Server-class configuration with generous memory allocation.
--
--  Target Hardware:
--    - STM32MP135FAF7 (Cortex-A7 @ 1 GHz) - Microprocessor (MPU)
--    - External DDR3L: 4 Gbit (512 MB)
--    - Operating System: Linux (OpenSTLinux distribution)
--
--  Design Philosophy:
--    Server/desktop-class configuration:
--    - No memory constraints (512 MB RAM)
--    - Full IANA database support
--    - Extensive caching for high performance
--    - Maximum compatibility
--
--  Use Cases:
--    - IoT gateway with timezone services
--    - Embedded Linux server
--    - Development and testing platform
--    - Full-featured TZif parsing
--  ==========================================================================

with Ada.Containers;

package TZif_Config is

   pragma Pure;

   use Ada.Containers;

   --  =======================================================================
   --  Profile Metadata
   --  =======================================================================

   Profile_Name : constant String := "stm32mp135_linux";
   Target_Platform : constant String := "STM32MP135F-DK (Linux MPU)";
   Target_RAM_KB : constant Positive := 524_288;  -- 512 MB
   Operating_System : constant String := "Linux (OpenSTLinux)";

   --  =======================================================================
   --  TZif Binary Parser Configuration (Very Generous)
   --  =======================================================================

   --  Maximum length of timezone identifiers (e.g., "America/Los_Angeles")
   --  IANA maximum: 30 characters
   --  Linux MPU profile: 128 characters (server-class, maximum compatibility)
   Max_Zone_ID_Length : constant := 128;

   --  Maximum length of timezone abbreviations (e.g., "PDT", "EST")
   --  IANA maximum: 9 characters
   --  Linux MPU profile: 32 characters (very generous)
   Max_Abbreviation_Length : constant := 32;

   --  Maximum transitions per zone (DST changes over time)
   --  Linux server: 2000 transitions (covers >400 years)
   Max_Transitions_Per_Zone : constant := 2_000;

   --  Maximum timezone types per zone (different UTC offsets)
   --  Linux MPU profile: 100 types (maximum compatibility)
   Max_Types_Per_Zone : constant := 100;

   --  Maximum leap seconds in TZif file
   --  Historical: 27 leap seconds since 1972
   --  Linux MPU profile: 200 (extreme future-proofing)
   Max_Leap_Seconds : constant := 200;

   --  =======================================================================
   --  Collection Capacity Hints (Server-Class)
   --  =======================================================================

   --  These are capacity hints for bounded containers.
   --  Linux MPU profile uses very large caches (512MB RAM available).

   --  Cache capacity: Very large (entire IANA database)
   --  Can hold all ~600 TZif files in memory
   Default_Cache_Capacity : constant Count_Type := 600;

   --  Zone capacity: Entire IANA database
   Default_Zone_Capacity : constant Count_Type := 600;

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
