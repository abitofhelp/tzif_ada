pragma Ada_2022;
--  ===========================================================================
--  Find_By_Region - Search zones by geographic region
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Demonstrates finding timezones by geographic region.
--    Uses callback-based iteration for results.
--
--  API Signature:
--    function Find_By_Region (Region : Region_String;
--                             Yield : Region_Callback)
--      return Region_Result;
--
--  Note:
--    This example demonstrates the API signature and purpose.
--    For detailed usage with callbacks, see:
--      test/integration/test_find_by_region.adb
--  ===========================================================================

with Ada.Text_IO;

procedure Find_By_Region is
   use Ada.Text_IO;
begin
   Put_Line
     ("============================================"
      & "==========================");
   Put_Line
     ("| Find_By_Region - Search zones by geographic region      |");
   Put_Line
     ("============================================"
      & "==========================");
   New_Line;

   Put_Line ("API: function Find_By_Region (Region : Region_String;");
   Put_Line ("                               Yield : Region_Callback)");
   Put_Line ("       return Region_Result;");
   New_Line;
   Put_Line ("Purpose:");
   Put_Line ("  - Search for zones in a geographic region");
   Put_Line ("  - Uses callback for each matching zone");
   Put_Line ("  - Example: ""America"" returns America/*, but not");
   Put_Line ("    zones that just contain the word");
   New_Line;
   Put_Line
     ("For detailed usage, see test/integration/test_find_by_region.adb");

   New_Line;
   Put_Line ("Done.");
end Find_By_Region;
