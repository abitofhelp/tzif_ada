pragma Ada_2022;
--  ======================================================================
--  Integration_Runner
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Test runner for executing test suites.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Test_Framework;
--  Test procedures
with Test_Cache_Export_Import;
with Test_Cache_Validation_Errors;
with Test_Cache_With_Zones;
with Test_Discover_Sources;
with Test_Export_Cache;
with Test_Find_By_Id;
with Test_Find_By_Pattern;
with Test_Find_By_Regex;
with Test_Find_By_Region;
with Test_Find_My_Id;
with Test_Get_Transition_At_Epoch;
with Test_Get_Version;
with Test_Import_Cache;
with Test_List_All_Order_By_Id;
with Test_Load_Source;
with Test_Platform_Stubs;
with Test_Query_Timezone_Info;
with Test_TZif_Parser_Errors;
with Test_Validate_Source;
with Test_Zone_Repository_Errors;
procedure Integration_Runner is
   Overall_Failed : Boolean := False;
   Grand_Total    : Natural;
   Grand_Passed   : Natural;
   procedure Run_Test (Name : String; Test : access procedure) is
   begin
      New_Line;
      Put_Line ("====================================================");
      Put_Line ("  Running: " & Name);
      Put_Line ("====================================================");
      begin
         Test.all;
      exception
         when others => Put_Line ("  [EXCEPTION] Test suite crashed!");
            Overall_Failed := True;
      end;
   end Run_Test;
begin
   Put_Line ("========================================================");
   Put_Line ("  TZif Library - Integration Test Suite");
   Put_Line ("========================================================");
   --  Reset framework counters
   Test_Framework.Reset;
   --  Run all test suites
   Run_Test ("Cache Export/Import Tests", Test_Cache_Export_Import'Access);
   Run_Test
     ("Cache Validation Error Tests", Test_Cache_Validation_Errors'Access);
   Run_Test ("Cache With Zones Tests", Test_Cache_With_Zones'Access);
   Run_Test ("Discover Sources Tests", Test_Discover_Sources'Access);
   Run_Test ("Export Cache Tests", Test_Export_Cache'Access);
   Run_Test ("Find By ID Tests", Test_Find_By_Id'Access);
   Run_Test ("Find By Pattern Tests", Test_Find_By_Pattern'Access);
   Run_Test ("Find By Regex Tests", Test_Find_By_Regex'Access);
   Run_Test ("Find By Region Tests", Test_Find_By_Region'Access);
   Run_Test ("Find My ID Tests", Test_Find_My_Id'Access);
   Run_Test
     ("Get Transition At Epoch Tests", Test_Get_Transition_At_Epoch'Access);
   Run_Test ("Get Version Tests", Test_Get_Version'Access);
   Run_Test ("Import Cache Tests", Test_Import_Cache'Access);
   Run_Test ("List All Order By ID Tests", Test_List_All_Order_By_Id'Access);
   Run_Test ("Load Source Tests", Test_Load_Source'Access);
   Run_Test ("Platform Stub Tests", Test_Platform_Stubs'Access);
   Run_Test ("Query Timezone Info Tests", Test_Query_Timezone_Info'Access);
   Run_Test ("TZif Parser Error Tests", Test_TZif_Parser_Errors'Access);
   Run_Test ("Validate Source Tests", Test_Validate_Source'Access);
   Run_Test
     ("Zone Repository Error Tests", Test_Zone_Repository_Errors'Access);
   --  Get grand totals
   Grand_Total := Test_Framework.Grand_Total_Tests;
   Grand_Passed := Test_Framework.Grand_Total_Passed;
   --  Final summary with visual indicator
   New_Line;
   Put_Line ("========================================================");
   if Grand_Passed = Grand_Total and then not Overall_Failed then
      Put_Line
        ("  [PASS] GRAND TOTAL: "
         & Grand_Passed'Image
         & " /"
         & Grand_Total'Image
         & " TESTS PASSED");
      Put_Line ("========================================================");
      Ada.Command_Line.Set_Exit_Status (0);
   else
      Put_Line
        ("  [FAIL] GRAND TOTAL: "
         & Grand_Passed'Image
         & " /"
         & Grand_Total'Image
         & " TESTS (FAILURES DETECTED)");
      Put_Line ("========================================================");
      Ada.Command_Line.Set_Exit_Status (1);
   end if;
end Integration_Runner;