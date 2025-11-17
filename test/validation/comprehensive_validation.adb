pragma Ada_2022;
--  ======================================================================
--  Comprehensive_Validation
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
with Ada.Strings.Unbounded;
with TZif.API;

procedure Comprehensive_Validation is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use TZif.API;

   --  Statistics
   Total_Tests : Natural := 0;
   Tests_Passed : Natural := 0;
   Tests_Failed : Natural := 0;
   Zones_Tested : Natural := 0;
   Zones_Passed : Natural := 0;
   Zones_Failed : Natural := 0;

   --  Test zones (comprehensive list of major timezones)
   type Zone_Array is array (Positive range <>) of Unbounded_String;

   Test_Zones : constant Zone_Array :=
     [To_Unbounded_String ("Africa/Cairo"),
      To_Unbounded_String ("Africa/Johannesburg"),
      To_Unbounded_String ("America/New_York"),
      To_Unbounded_String ("America/Chicago"),
      To_Unbounded_String ("America/Denver"),
      To_Unbounded_String ("America/Los_Angeles"),
      To_Unbounded_String ("America/Anchorage"),
      To_Unbounded_String ("America/Sao_Paulo"),
      To_Unbounded_String ("America/Argentina/Buenos_Aires"),
      To_Unbounded_String ("America/Mexico_City"),
      To_Unbounded_String ("America/Toronto"),
      To_Unbounded_String ("America/Vancouver"),
      To_Unbounded_String ("Asia/Tokyo"),
      To_Unbounded_String ("Asia/Shanghai"),
      To_Unbounded_String ("Asia/Hong_Kong"),
      To_Unbounded_String ("Asia/Singapore"),
      To_Unbounded_String ("Asia/Dubai"),
      To_Unbounded_String ("Asia/Kolkata"),
      To_Unbounded_String ("Asia/Bangkok"),
      To_Unbounded_String ("Asia/Seoul"),
      To_Unbounded_String ("Asia/Jakarta"),
      To_Unbounded_String ("Australia/Sydney"),
      To_Unbounded_String ("Australia/Melbourne"),
      To_Unbounded_String ("Australia/Perth"),
      To_Unbounded_String ("Pacific/Auckland"),
      To_Unbounded_String ("Pacific/Fiji"),
      To_Unbounded_String ("Pacific/Honolulu"),
      To_Unbounded_String ("Europe/London"),
      To_Unbounded_String ("Europe/Paris"),
      To_Unbounded_String ("Europe/Berlin"),
      To_Unbounded_String ("Europe/Rome"),
      To_Unbounded_String ("Europe/Madrid"),
      To_Unbounded_String ("Europe/Amsterdam"),
      To_Unbounded_String ("Europe/Brussels"),
      To_Unbounded_String ("Europe/Vienna"),
      To_Unbounded_String ("Europe/Warsaw"),
      To_Unbounded_String ("Europe/Stockholm"),
      To_Unbounded_String ("Europe/Moscow"),
      To_Unbounded_String ("Europe/Istanbul"),
      To_Unbounded_String ("Europe/Athens"),
      To_Unbounded_String ("Atlantic/Reykjavik"),
      To_Unbounded_String ("Indian/Maldives"),
      To_Unbounded_String ("UTC"),
      To_Unbounded_String ("Etc/GMT+5")];

   --  Test epochs (comprehensive coverage)
   type Epoch_Array is array (Positive range <>) of Epoch_Seconds_Type;

   Test_Epochs : constant Epoch_Array :=
     [0,                   --  1970-01-01 Unix epoch
      315_532_800,         --  1980-01-01 Historical
      946_684_800,         --  2000-01-01 Y2K
      1_577_836_800,       --  2020-01-01 Winter
      1_594_512_000,       --  2020-07-12 Summer
      1_705_305_600,       --  2024-01-15 Current winter
      1_713_139_200,       --  2024-04-15 Spring
      1_721_001_600,       --  2024-07-15 Summer
      1_728_950_400,       --  2024-10-15 Fall
      1_710_054_000,       --  2024-03-10 02:30 DST gap
      1_730_610_000,       --  2024-11-03 01:30 DST ambiguous
      2_556_057_600];      --  2050-12-31 Future

   procedure Test_Single_Zone (Zone_Name : Unbounded_String);

   procedure Test_Single_Zone (Zone_Name : Unbounded_String) is
      Zone_Id : constant Zone_Id_String :=
        Make_Zone_Id_String (To_String (Zone_Name));
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
         Put_Line ("[FAIL] " & To_String (Zone_Name) & " - " &
                  Zone_Passed'Image & "/" & Zone_Tests'Image & " passed");
      else
         Zones_Passed := Zones_Passed + 1;
         if Zones_Passed mod 10 = 0 then
            Put_Line ("[" & Zones_Passed'Image & " zones passed] " &
                     To_String (Zone_Name));
         end if;
      end if;
   end Test_Single_Zone;

begin
   Put_Line ("========================================================");
   Put_Line ("  COMPREHENSIVE TZif VALIDATION");
   Put_Line ("  Testing TZif.API across major timezones");
   Put_Line ("========================================================");
   Put_Line ("");
   Put_Line ("Test Coverage:");
   Put_Line ("  Timezones:" & Test_Zones'Length'Image);
   Put_Line ("  Epochs per zone:" & Test_Epochs'Length'Image);
   Put_Line ("  Total test cases:" &
            Natural'Image (Test_Zones'Length * Test_Epochs'Length));
   Put_Line ("");
   Put_Line ("Testing all timezones...");
   Put_Line ("");

   for Zone_Name of Test_Zones loop
      Test_Single_Zone (Zone_Name);
   end loop;

   --  Final Summary
   Put_Line ("");
   Put_Line ("========================================================");
   Put_Line ("  COMPREHENSIVE VALIDATION SUMMARY");
   Put_Line ("========================================================");
   Put_Line ("Zones processed:  " & Zones_Tested'Image);
   Put_Line ("Zones passed:     " & Zones_Passed'Image);
   Put_Line ("Zones failed:     " & Zones_Failed'Image);
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
      Put_Line ("TZif validated across" & Total_Tests'Image & " tests!");
      Put_Line ("Tested" & Zones_Passed'Image &
               " timezones successfully!");
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

end Comprehensive_Validation;
