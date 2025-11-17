pragma Ada_2022;
--  ===========================================================================
--  Find_By_Pattern - Search zones by substring
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Demonstrates finding timezones by substring pattern matching.
--    Uses callback-based iteration for results.
--
--  API Signature:
--    function Find_By_Pattern (Pattern : Pattern_String;
--                              Yield : Pattern_Callback)
--      return Pattern_Result;
--
--  Note:
--    This example demonstrates the API signature and purpose.
--    For detailed usage with callbacks, see:
--      test/integration/test_find_by_pattern.adb
--  ===========================================================================

with Ada.Text_IO;

procedure Find_By_Pattern is
   use Ada.Text_IO;
begin
   Put_Line
     ("============================================"
      & "==========================");
   Put_Line
     ("| Find_By_Pattern - Search zones by substring             |");
   Put_Line
     ("============================================"
      & "==========================");
   New_Line;

   Put_Line ("API: function Find_By_Pattern (Pattern : Pattern_String;");
   Put_Line ("                                Yield : Pattern_Callback)");
   Put_Line ("       return Pattern_Result;");
   New_Line;
   Put_Line ("Purpose:");
   Put_Line ("  - Search for zones containing a substring");
   Put_Line ("  - Uses callback for each matching zone");
   Put_Line ("  - Example: ""America"" matches all America/* zones");
   New_Line;
   Put_Line ("Callback signature:");
   Put_Line ("  procedure My_Callback (Zone_Id : Zone_Id_Type);");
   New_Line;
   Put_Line
     ("For detailed usage, see test/integration/test_find_by_pattern.adb");

   New_Line;
   Put_Line ("Done.");
end Find_By_Pattern;
