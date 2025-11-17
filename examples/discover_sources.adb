pragma Ada_2022;
--  ===========================================================================
--  Discover_Sources - Scan filesystem for timezone sources
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Demonstrates discovering timezone data sources on the filesystem.
--    Scans standard locations for tzdata files.
--
--  API Signature:
--    function Discover_Sources (Search_Paths : Path_List)
--      return Discovery_Result;
--
--  Note:
--    This example demonstrates the API signature and purpose.
--    For detailed usage with Path_List construction, see:
--      test/integration/test_discover_sources.adb
--  ===========================================================================

with Ada.Text_IO;

procedure Discover_Sources is
   use Ada.Text_IO;
begin
   Put_Line
     ("============================================"
      & "==========================");
   Put_Line
     ("| Discover_Sources - Find timezone data sources           |");
   Put_Line
     ("============================================"
      & "==========================");
   New_Line;

   Put_Line ("API: function Discover_Sources (Search_Paths : Path_List)");
   Put_Line ("       return Discovery_Result;");
   New_Line;
   Put_Line ("Purpose:");
   Put_Line ("  - Scan filesystem for timezone data directories");
   Put_Line ("  - Returns Source_Info_List with discovered sources");
   Put_Line ("  - Used to initialize timezone data");
   New_Line;
   Put_Line ("Typical paths:");
   Put_Line ("  - Linux/BSD: /usr/share/zoneinfo");
   Put_Line ("  - macOS: /usr/share/zoneinfo, /var/db/timezone/zoneinfo");
   New_Line;
   Put_Line
     ("For detailed usage, see test/integration/test_discover_sources.adb");

   New_Line;
   Put_Line ("Done.");
end Discover_Sources;
