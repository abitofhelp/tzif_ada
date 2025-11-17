pragma Ada_2022;
--  ===========================================================================
--  Find_By_Regex - Search zones by regular expression
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Demonstrates finding timezones using regular expression matching.
--    Uses callback-based iteration for results.
--
--  API Signature:
--    function Find_By_Regex (Regex : Regex_String;
--                            Yield : Regex_Callback)
--      return Regex_Result;
--
--  Note:
--    This example demonstrates the API signature and purpose.
--    For detailed usage with callbacks, see:
--      test/integration/test_find_by_regex.adb
--  ===========================================================================

with Ada.Text_IO;

procedure Find_By_Regex is
   use Ada.Text_IO;
begin
   Put_Line
     ("============================================"
      & "==========================");
   Put_Line
     ("| Find_By_Regex - Search zones by regular expression      |");
   Put_Line
     ("============================================"
      & "==========================");
   New_Line;

   Put_Line ("API: function Find_By_Regex (Regex : Regex_String;");
   Put_Line ("                              Yield : Regex_Callback)");
   Put_Line ("       return Regex_Result;");
   New_Line;
   Put_Line ("Purpose:");
   Put_Line ("  - Search for zones using regular expressions");
   Put_Line ("  - Uses callback for each matching zone");
   Put_Line ("  - Example: ""^America/.*"" matches all America/* zones");
   New_Line;
   Put_Line
     ("For detailed usage, see test/integration/test_find_by_regex.adb");

   New_Line;
   Put_Line ("Done.");
end Find_By_Regex;
