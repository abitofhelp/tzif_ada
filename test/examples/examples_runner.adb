pragma Ada_2022;
--  ======================================================================
--  Examples_Runner - Test runner for example programs
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Runs all example programs and validates successful execution.
--    Uses Test_Framework for consistent output with unit/integration tests.
--  ======================================================================

with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;
with GNAT.OS_Lib;
with Test_Framework;

procedure Examples_Runner is

   use Ada.Command_Line;
   use Ada.Directories;
   use Ada.Text_IO;
   use GNAT.OS_Lib;

   --  List of all example programs (must match examples.gpr Main list)
   type Example_Name is
     (Discover_Sources,
      Find_By_Id,
      Find_By_Pattern,
      Find_By_Regex,
      Find_By_Region,
      Find_My_Id,
      Get_Transition_At_Epoch,
      Get_Version,
      List_All_Zones,
      Load_Source,
      Validate_Source);

   --  Get the executable name for an example
   function Executable_Name (Example : Example_Name) return String is
   begin
      case Example is
         when Discover_Sources      => return "discover_sources";
         when Find_By_Id            => return "find_by_id";
         when Find_By_Pattern       => return "find_by_pattern";
         when Find_By_Regex         => return "find_by_regex";
         when Find_By_Region        => return "find_by_region";
         when Find_My_Id            => return "find_my_id";
         when Get_Transition_At_Epoch => return "get_transition_at_epoch";
         when Get_Version           => return "get_version";
         when List_All_Zones        => return "list_all_zones";
         when Load_Source           => return "load_source";
         when Validate_Source       => return "validate_source";
      end case;
   end Executable_Name;

   --  Test counters
   Total_Tests  : Natural := 0;
   Passed_Tests : Natural := 0;

   --  Path to examples directory (relative to project root where make runs)
   Examples_Dir : constant String := "bin/examples";

   --  Run a single example and check for success
   procedure Run_Example (Example : Example_Name) is
      Name      : constant String := Executable_Name (Example);
      Full_Path : constant String := Examples_Dir & "/" & Name;
      Args      : Argument_List (1 .. 0);  -- No arguments
      Success   : Boolean;
   begin
      Total_Tests := Total_Tests + 1;

      --  Check if executable exists
      if not Exists (Full_Path) then
         Put_Line ("  [SKIP] " & Name & " - executable not found");
         return;
      end if;

      --  Spawn the example and wait for completion
      Spawn
        (Program_Name => Full_Path,
         Args         => Args,
         Success      => Success);

      if Success then
         Put_Line ("  [PASS] " & Name);
         Passed_Tests := Passed_Tests + 1;
      else
         Put_Line ("  [FAIL] " & Name & " - execution failed");
      end if;
   end Run_Example;

begin
   Put_Line ("");
   Put_Line ("========================================");
   Put_Line ("      TZIF EXAMPLES TEST SUITE");
   Put_Line ("========================================");
   Put_Line ("");

   --  Reset test framework before running tests
   Test_Framework.Reset;

   Put_Line ("Running example programs...");
   Put_Line ("Examples directory: " & Examples_Dir);
   Put_Line ("");

   --  Run all examples
   for Example in Example_Name loop
      Run_Example (Example);
   end loop;

   --  Register results with test framework
   Test_Framework.Register_Results (Total_Tests, Passed_Tests);

   --  Get cumulative results (same as what we registered)
   declare
      Total  : constant Natural := Test_Framework.Grand_Total_Tests;
      Passed : constant Natural := Test_Framework.Grand_Total_Passed;
   begin
      --  Print grand summary
      Put_Line ("");
      Put_Line ("========================================");
      Put_Line ("     GRAND TOTAL - ALL EXAMPLE TESTS");
      Put_Line ("========================================");
      Put_Line ("Total tests:  " & Total'Image);
      Put_Line ("Passed:       " & Passed'Image);
      Put_Line ("Failed:       " & Natural'Image (Total - Passed));

      --  Print professional color-coded summary and get exit status
      declare
         Exit_Code : constant Integer :=
           Test_Framework.Print_Category_Summary
             ("EXAMPLE TESTS", Total, Passed);
      begin
         Set_Exit_Status (if Exit_Code = 0 then Success else Failure);
      end;
   end;

end Examples_Runner;
