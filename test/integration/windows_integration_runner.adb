pragma Ada_2022;
--  ======================================================================
--  Windows_Integration_Runner - Windows-specific test runner
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Windows-specific integration test runner that excludes POSIX-only
--    tests (those using POSIX_Repository or POSIX_Zone_Repository).
--    Tests requiring POSIX symlink operations are not applicable on Windows.
--  ======================================================================

with Ada.Command_Line;
with Ada.Text_IO;
with Test_Framework;

--  Import Windows-compatible integration tests only
--  Note: Tests using POSIX_Repository are excluded on Windows
with Test_API;
with Test_Query_Timezone_Info;
with Test_TZif_Parser_Errors;

procedure Windows_Integration_Runner is

   use Ada.Text_IO;
   use Ada.Command_Line;

   Total  : Natural;
   Passed : Natural;

begin
   Put_Line ("");
   Put_Line ("========================================");
   Put_Line ("    TZIF INTEGRATION TEST SUITE");
   Put_Line ("    (Windows - POSIX tests excluded)");
   Put_Line ("========================================");
   Put_Line ("");

   --  Reset test framework before running tests
   Test_Framework.Reset;

   --  Run Windows-compatible integration tests
   --  POSIX-specific tests excluded:
   --    Test_Discover_Sources
   --    Test_Find_By_Id
   --    Test_Find_By_Pattern
   --    Test_Find_By_Regex
   --    Test_Find_By_Region
   --    Test_Find_My_Id
   --    Test_Get_Transition_At_Epoch
   --    Test_Get_Version
   --    Test_List_All_Order_By_Id
   --    Test_Load_Source
   --    Test_Validate_Source
   --    Test_Zone_Repository_Errors

   Test_API;
   Test_Query_Timezone_Info;
   Test_TZif_Parser_Errors;

   --  Get cumulative results
   Total  := Test_Framework.Grand_Total_Tests;
   Passed := Test_Framework.Grand_Total_Passed;

   --  Print grand summary
   Put_Line ("");
   Put_Line ("========================================");
   Put_Line ("   GRAND TOTAL - WINDOWS INTEGRATION");
   Put_Line ("========================================");
   Put_Line ("Total tests:  " & Total'Image);
   Put_Line ("Passed:       " & Passed'Image);
   Put_Line ("Failed:       " & Natural'Image (Total - Passed));

   --  Print professional color-coded summary and get exit status
   declare
      Exit_Code : constant Integer :=
        Test_Framework.Print_Category_Summary
          ("INTEGRATION TESTS", Total, Passed);
   begin
      Set_Exit_Status (if Exit_Code = 0 then Success else Failure);
   end;

end Windows_Integration_Runner;
