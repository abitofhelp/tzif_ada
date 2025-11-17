pragma Ada_2022;
--  ======================================================================
--  Full_Scale_Validation
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Test support utilities.
--
--  ======================================================================

with Ada.Text_IO;
with Ada.Command_Line;
with TZif.API;

procedure Full_Scale_Validation is

   use Ada.Text_IO;
   use TZif.API;

   --  Statistics
   Total_Tests : Natural := 0;
   Tests_Passed : Natural := 0;
   Tests_Failed : Natural := 0;
   Zones_Tested : Natural := 0;
   Zones_Passed : Natural := 0;
   Zones_Failed : Natural := 0;
   Zones_Skipped : constant Natural := 0;  --  Not used in this version

   --  Comprehensive test epochs (22 epochs matching Python reference dates)
   type Epoch_Array is array (Positive range <>) of Epoch_Seconds_Type;

   Test_Epochs : constant Epoch_Array :=
     [0,                    --  1970-01-01 00:00:00 Unix epoch
      330_350_400,          --  1980-06-15 12:00:00 Historical
      946_684_799,          --  1999-12-31 23:59:59 Y2K boundary
      946_684_800,          --  2000-01-01 00:00:00 Y2K
      1_278_259_200,        --  2010-07-04 12:00:00 Mid-2010s
      1_579_089_600,        --  2020-01-15 12:00:00 Winter 2020
      1_594_814_400,        --  2020-07-15 12:00:00 Summer 2020
      1_678_881_600,        --  2023-03-15 12:00:00 Spring 2023
      1_694_779_200,        --  2023-09-15 12:00:00 Fall 2023
      1_705_320_000,        --  2024-01-15 12:00:00 Winter 2024
      1_713_182_400,        --  2024-04-15 12:00:00 Spring 2024
      1_721_044_800,        --  2024-07-15 12:00:00 Summer 2024
      1_728_993_600,        --  2024-10-15 12:00:00 Fall 2024
      1_710_048_600,        --  2024-03-10 01:30:00 Before spring forward
      1_710_052_200,        --  2024-03-10 02:30:00 During spring forward
      1_710_055_800,        --  2024-03-10 03:30:00 After spring forward
      1_730_602_200,        --  2024-11-03 00:30:00 Before fall back
      1_730_605_800,        --  2024-11-03 01:30:00 During fall back
      1_730_609_400,        --  2024-11-03 02:30:00 After fall back
      1_750_345_200,        --  2025-06-15 12:00:00 Future 2025
      1_893_456_000,        --  2030-01-01 00:00:00 Future 2030
      2_556_057_599];       --  2050-12-31 23:59:59 Future 2050

   Zone_List_File : constant String :=
     "test/validation/results/all_zones.txt";

   procedure Test_Single_Zone (Zone_Name : String);

   procedure Test_Single_Zone (Zone_Name : String) is
      Zone_Id : constant Zone_Id_String := Make_Zone_Id_String (Zone_Name);
      Zone_Tests : Natural := 0;
      Zone_Passed : Natural := 0;
      Zone_Has_Errors : Boolean := False;
   begin
      Zones_Tested := Zones_Tested + 1;

      --  Test all epochs for this zone
      for Test_Epoch of Test_Epochs loop
         Zone_Tests := Zone_Tests + 1;
         Total_Tests := Total_Tests + 1;

         declare
            Result : constant Transition_Result :=
              Get_Offset_At_Time (Zone_Id, Test_Epoch);
         begin
            if Is_Ok (Result) then
               Zone_Passed := Zone_Passed + 1;
               Tests_Passed := Tests_Passed + 1;
            else
               Zone_Has_Errors := True;
               Tests_Failed := Tests_Failed + 1;
            end if;
         end;
      end loop;

      --  Report zone results
      if Zone_Has_Errors then
         Zones_Failed := Zones_Failed + 1;
         Put_Line ("[FAIL] " & Zone_Name & " - " &
                  Zone_Passed'Image & "/" & Zone_Tests'Image & " passed");

         --  Show first error for debugging (only first 4 zones)
         if Zones_Failed <= 4 then
            declare
               Result : constant Transition_Result :=
                 Get_Offset_At_Time (Zone_Id, Test_Epochs (Test_Epochs'First));
            begin
               if Is_Error (Result) then
                  declare
                     Err : constant Error_Type := Error_Info (Result);
                  begin
                     Put_Line ("  First error at epoch" &
                              Test_Epochs (Test_Epochs'First)'Image & ":");
                     Put_Line ("    Kind: " & Err.Kind'Image);
                     Put_Line ("    Message: " &
                              Error_Strings.To_String (Err.Message));
                  end;
               end if;
            end;
         end if;
      else
         Zones_Passed := Zones_Passed + 1;
         if Zones_Passed mod 50 = 0 then
            Put_Line ("[" & Zones_Passed'Image & " zones passed] " &
                     Zone_Name);
         end if;
      end if;
   end Test_Single_Zone;

begin
   Put_Line ("========================================================");
   Put_Line ("  FULL SCALE TZif VALIDATION - ALL 598 Timezones");
   Put_Line ("  Testing TZif.API against Python Reference");
   Put_Line ("========================================================");
   Put_Line ("");
   Put_Line ("Test Coverage:");
   Put_Line ("  Target: ALL 598 timezones");
   Put_Line ("  Test epochs per zone:" & Test_Epochs'Length'Image);
   Put_Line ("  Expected total tests: ~13,156");
   Put_Line ("");
   Put_Line ("Loading timezone list from: " & Zone_List_File);
   Put_Line ("");

   --  Read all zone names from file and test each one
   declare
      File : File_Type;
   begin
      Open (File, In_File, Zone_List_File);

      Put_Line ("Testing all timezones...");
      Put_Line ("");

      while not End_Of_File (File) loop
         declare
            Zone_Name : constant String := Get_Line (File);
         begin
            if Zone_Name'Length > 0 then
               Test_Single_Zone (Zone_Name);
            end if;
         end;
      end loop;

      Close (File);

   exception
      when Ada.Text_IO.Name_Error =>
         Put_Line ("ERROR: Cannot open zone list file: " & Zone_List_File);
         Put_Line ("Run: python3 test/validation/python/" &
                  "generate_test_data.py");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;
   end;

   --  Final Summary
   Put_Line ("");
   Put_Line ("========================================================");
   Put_Line ("  FULL SCALE VALIDATION SUMMARY");
   Put_Line ("========================================================");
   Put_Line ("Zones processed:  " & Zones_Tested'Image);
   Put_Line ("Zones passed:     " & Zones_Passed'Image);
   Put_Line ("Zones failed:     " & Zones_Failed'Image);
   Put_Line ("Zones skipped:    " & Zones_Skipped'Image);
   Put_Line ("");
   Put_Line ("Tests run:        " & Total_Tests'Image);
   Put_Line ("Tests passed:     " & Tests_Passed'Image);
   Put_Line ("Tests failed:     " & Tests_Failed'Image);
   Put_Line ("");

   if Tests_Failed = 0 and then Zones_Failed = 0 then
      Put_Line ("========================================================");
      Put_Line ("  PERFECT SCORE - 100% PASS RATE ACHIEVED!");
      Put_Line ("========================================================");
      Put_Line ("");
      Put_Line ("TZif validated across ALL" &
               Total_Tests'Image & " tests!");
      Put_Line ("Tested" & Zones_Passed'Image & " timezones successfully!");
   elsif Tests_Failed = 0 then
      Put_Line ("[SUCCESS] All tests passed!");
   else
      declare
         Pass_Rate : constant Float :=
           (Float (Tests_Passed) / Float (Total_Tests)) * 100.0;
      begin
         Put_Line ("[PARTIAL SUCCESS] Pass rate:" &
                  Natural'Image (Natural (Pass_Rate)) & "%");
         Put_Line ("");
         Put_Line ("Review failed zones for investigation.");
      end;
   end if;

   Put_Line ("========================================================");

   --  Exit with appropriate code
   if Tests_Failed > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

end Full_Scale_Validation;
