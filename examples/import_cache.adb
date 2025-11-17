pragma Ada_2022;
--  ===========================================================================
--  Import_Cache - Import zone cache from JSON
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Demonstrates importing a timezone zone cache from JSON file.
--    Allows pre-loading zones for fast startup.
--
--  API Signature:
--    function Import_Cache (Path : Import_Path_String)
--      return Import_Cache_Result;
--
--  Note:
--    This example demonstrates the API signature and purpose.
--    For detailed usage, see: test/integration/test_import_cache.adb
--  ===========================================================================

with Ada.Text_IO;

procedure Import_Cache is
   use Ada.Text_IO;
begin
   Put_Line
     ("============================================"
      & "==========================");
   Put_Line
     ("| Import_Cache - Import zone cache from JSON              |");
   Put_Line
     ("============================================"
      & "==========================");
   New_Line;

   Put_Line ("API: function Import_Cache (Path : Import_Path_String)");
   Put_Line ("       return Import_Cache_Result;");
   New_Line;
   Put_Line ("Purpose:");
   Put_Line ("  - Import pre-parsed timezone data from JSON");
   Put_Line ("  - Enables fast application startup");
   Put_Line ("  - Loads zones into in-memory cache");
   New_Line;
   Put_Line ("Typical workflow:");
   Put_Line ("  1. Export cache during build/initialization");
   Put_Line ("  2. Import cache at application startup");
   New_Line;
   Put_Line ("For detailed usage, see test/integration/test_import_cache.adb");

   New_Line;
   Put_Line ("Done.");
end Import_Cache;
