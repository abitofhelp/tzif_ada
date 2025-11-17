pragma Ada_2022;
--  ===========================================================================
--  Cache_Export_Import - Demonstrate cache serialization
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Demonstrates exporting and importing timezone zone caches.
--    Shows round-trip cache serialization to/from JSON.
--
--  API Signatures:
--    function Export_Cache (Path : Export_Path_String;
--                           Overwrite : Boolean := False)
--      return Export_Cache_Result;
--
--    function Import_Cache (Path : Import_Path_String)
--      return Import_Cache_Result;
--
--  Note:
--    This example demonstrates the API signatures and purpose.
--    For detailed usage, see: test/integration/test_cache*.adb
--  ===========================================================================

with Ada.Text_IO;

procedure Cache_Export_Import is
   use Ada.Text_IO;
begin
   Put_Line
     ("============================================"
      & "==========================");
   Put_Line
     ("| Cache_Export_Import - Cache serialization round-trip    |");
   Put_Line
     ("============================================"
      & "==========================");
   New_Line;

   Put_Line ("Export API:");
   Put_Line ("  function Export_Cache (Path : Export_Path_String;");
   Put_Line ("                         Overwrite : Boolean := False)");
   Put_Line ("    return Export_Cache_Result;");
   New_Line;
   Put_Line ("Import API:");
   Put_Line ("  function Import_Cache (Path : Import_Path_String)");
   Put_Line ("    return Import_Cache_Result;");
   New_Line;
   Put_Line ("Purpose:");
   Put_Line ("  - Export: Serialize in-memory zones to JSON");
   Put_Line ("  - Import: Load zones from JSON into cache");
   Put_Line ("  - Dramatically speeds up application startup");
   New_Line;
   Put_Line ("Typical workflow:");
   Put_Line ("  1. Load timezone data during initialization");
   Put_Line ("  2. Export_Cache to save to JSON");
   Put_Line ("  3. Import_Cache on subsequent startups");
   New_Line;
   Put_Line ("For detailed usage, see test/integration/test_cache*.adb");

   New_Line;
   Put_Line ("Done.");
end Cache_Export_Import;
