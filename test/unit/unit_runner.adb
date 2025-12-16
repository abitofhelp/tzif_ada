pragma Ada_2022;
--  ======================================================================
--  Unit_Runner - Main test runner for unit tests
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Runs all unit tests and reports cumulative results.
--  ======================================================================

with Ada.Command_Line;
with Ada.Text_IO;
with Test_Framework;

--  Import all test procedures
with Test_Bounded_Vector;
with Test_Canonical;
with Test_Option_Combinators;
with Test_Result_Combinators;
with Test_Timezone_Lookup;
with Test_TZif_Data;
with Test_Zone_Id;
with Test_Value_Object_Accessors;
with Test_IANA_Releases;
with Test_Zone_Entity;
with Test_Source_Cache;
with Test_ULID;
with Test_Version;
with Test_Zone_Cache;

procedure Unit_Runner is

   use Ada.Text_IO;
   use Ada.Command_Line;

   Total  : Natural;
   Passed : Natural;

begin
   Put_Line ("");
   Put_Line ("========================================");
   Put_Line ("        TZIF UNIT TEST SUITE");
   Put_Line ("========================================");
   Put_Line ("");

   --  Reset test framework before running tests
   Test_Framework.Reset;

   --  Run all unit test procedures
   --  Each test registers its results with Test_Framework

   Test_Bounded_Vector;
   Test_Canonical;
   Test_Option_Combinators;
   Test_Result_Combinators;
   Test_Timezone_Lookup;
   Test_TZif_Data;
   Test_Zone_Id;
   Test_Value_Object_Accessors;
   Test_IANA_Releases;
   Test_Zone_Entity;
   Test_Source_Cache;
   Test_ULID;
   Test_Version;
   Test_Zone_Cache;

   --  Get cumulative results
   Total  := Test_Framework.Grand_Total_Tests;
   Passed := Test_Framework.Grand_Total_Passed;

   --  Print grand summary
   Put_Line ("");
   Put_Line ("========================================");
   Put_Line ("        GRAND TOTAL - ALL UNIT TESTS");
   Put_Line ("========================================");
   Put_Line ("Total tests:  " & Total'Image);
   Put_Line ("Passed:       " & Passed'Image);
   Put_Line ("Failed:       " & Natural'Image (Total - Passed));

   --  Print professional color-coded summary and get exit status
   declare
      Exit_Code : constant Integer :=
        Test_Framework.Print_Category_Summary ("UNIT TESTS", Total, Passed);
   begin
      Set_Exit_Status (if Exit_Code = 0 then Success else Failure);
   end;

end Unit_Runner;
