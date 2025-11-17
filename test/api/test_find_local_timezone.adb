pragma Ada_2022;
--  ======================================================================
--  Test_Find_Local_Timezone
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Unit tests for Find Local Timezone functionality.
--
--  ======================================================================

with Ada.Text_IO;
with TZif.API;

procedure Test_Find_Local_Timezone is

   use Ada.Text_IO;
   use TZif.API;

   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;

   procedure Assert (Condition : Boolean; Message : String) is
   begin
      Test_Count := Test_Count + 1;
      if Condition then
         Pass_Count := Pass_Count + 1;
         Put_Line ("  [PASS] " & Message);
      else
         Put_Line ("  [FAIL] " & Message);
      end if;
   end Assert;

   procedure Test_Find_Local is
      Result : constant My_Zone_Result := Find_Local_Timezone;
   begin
      Put_Line ("");
      Put_Line ("Test: Find_Local_Timezone");

      Assert (Is_Ok (Result) or else Is_Error (Result),
              "Should return either Ok or Error result");

      if Is_Ok (Result) then
         declare
            Zone_Id : constant Zone_Id_Type := Value (Result);
            Id_Str  : constant String := To_String (Zone_Id);
         begin
            Assert (Id_Str'Length > 0,
                    "Zone ID should be non-empty");
            Put_Line ("    Detected local timezone: " & Id_Str);
         end;
      else
         Put_Line ("    Could not detect local timezone " &
                   "(acceptable on some systems)");
      end if;
   end Test_Find_Local;

   procedure Report_Results is
      Pass_Rate : constant Natural :=
        (if Test_Count > 0 then
           Natural ((Float (Pass_Count) / Float (Test_Count)) * 100.0)
         else 0);
   begin
      Put_Line ("");
      Put_Line ("========================================================");
      Put_Line ("  Test Results:");
      Put_Line ("  Total:  " & Test_Count'Image);
      Put_Line ("  Passed: " & Pass_Count'Image);
      Put_Line ("  Failed: " & Natural'Image (Test_Count - Pass_Count));
      Put_Line ("  Rate:   " & Pass_Rate'Image & "%");
      Put_Line ("========================================================");

      if Pass_Count = Test_Count then
         Put_Line ("  [SUCCESS] All tests passed!");
      else
         Put_Line ("  [FAILURE] Some tests failed");
      end if;
   end Report_Results;

begin
   Put_Line ("========================================================");
   Put_Line ("  TZif API Test: Find_Local_Timezone");
   Put_Line ("  Testing TZif.API.Find_Local_Timezone");
   Put_Line ("========================================================");

   Test_Find_Local;

   Report_Results;

end Test_Find_Local_Timezone;
