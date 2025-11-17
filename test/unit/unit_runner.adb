pragma Ada_2022;
--  ======================================================================
--  Unit_Runner
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
with Test_Timezone_Lookup;
with Test_TZif_Data;
with Test_Zone_Id;
with Test_Value_Object_Accessors;
with Test_IANA_Releases;
with Test_Zone_Entity;
with Test_Source_Cache;
with Test_JSON_Serialization;
with Test_ULID;
procedure Unit_Runner is
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
   Put_Line ("  TZif Library - Unit Test Suite");
   Put_Line ("========================================================");
   --  Reset framework counters
   Test_Framework.Reset;
   --  Run all test suites
   Run_Test ("Timezone Lookup Tests", Test_Timezone_Lookup'Access);
   Run_Test ("TZif Data Tests", Test_TZif_Data'Access);
   Run_Test ("Zone ID Tests", Test_Zone_Id'Access);
   Run_Test
     ("Value Object Accessor Tests", Test_Value_Object_Accessors'Access);
   Run_Test ("IANA Releases Tests", Test_IANA_Releases'Access);
   Run_Test ("Zone Entity Tests", Test_Zone_Entity'Access);
   Run_Test ("Source Cache Tests", Test_Source_Cache'Access);
   Run_Test ("JSON Serialization Tests", Test_JSON_Serialization'Access);
   Run_Test ("ULID Tests", Test_ULID'Access);
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
end Unit_Runner;