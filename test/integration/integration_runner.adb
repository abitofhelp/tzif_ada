pragma Ada_2022;
--  ======================================================================
--  Integration_Runner - Main test runner for integration tests
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Runs all integration tests and reports cumulative results.
--    Integration tests verify cross-layer interactions with real
--    infrastructure adapters.
--  ======================================================================

with Ada.Command_Line;
with Ada.Text_IO;
with Test_Framework;

--  Import all integration test procedures
with Test_API;
with Test_Discover_Sources;
with Test_Find_By_Id;
with Test_Find_By_Pattern;
with Test_Find_By_Regex;
with Test_Find_By_Region;
with Test_Find_My_Id;
with Test_Get_Transition_At_Epoch;
with Test_Get_Version;
with Test_List_All_Order_By_Id;
with Test_Load_Source;
--  Note: Test_Platform_Stubs removed - Windows code now excluded on Unix
with Test_Query_Timezone_Info;
with Test_TZif_Parser_Errors;
with Test_Validate_Source;
with Test_Zone_Repository_Errors;

procedure Integration_Runner is

   use Ada.Text_IO;
   use Ada.Command_Line;

   Total  : Natural;
   Passed : Natural;

begin
   Put_Line ("");
   Put_Line ("========================================");
   Put_Line ("    TZIF INTEGRATION TEST SUITE");
   Put_Line ("========================================");
   Put_Line ("");

   --  Reset test framework before running tests
   Test_Framework.Reset;

   --  Run all integration test procedures
   --  Each test registers its results with Test_Framework

   Test_API;
   Test_Discover_Sources;
   Test_Find_By_Id;
   Test_Find_By_Pattern;
   Test_Find_By_Regex;
   Test_Find_By_Region;
   Test_Find_My_Id;
   Test_Get_Transition_At_Epoch;
   Test_Get_Version;
   Test_List_All_Order_By_Id;
   Test_Load_Source;
   Test_Query_Timezone_Info;
   Test_TZif_Parser_Errors;
   Test_Validate_Source;
   Test_Zone_Repository_Errors;

   --  Get cumulative results
   Total  := Test_Framework.Grand_Total_Tests;
   Passed := Test_Framework.Grand_Total_Passed;

   --  Print grand summary
   Put_Line ("");
   Put_Line ("========================================");
   Put_Line ("   GRAND TOTAL - ALL INTEGRATION TESTS");
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

end Integration_Runner;
