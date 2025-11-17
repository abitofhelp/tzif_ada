pragma Ada_2022;
--  ======================================================================
--  Test_Get_Offset_At_Time
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Unit tests for Get Offset At Time functionality.
--
--  ======================================================================

with Ada.Text_IO;
with TZif.API;

procedure Test_Get_Offset_At_Time is

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

   procedure Test_UTC_At_Epoch_Zero is
      Zone_Id : constant Zone_Id_String := Make_Zone_Id_String ("UTC");
      Epoch   : constant Epoch_Seconds_Type := 0;
      Result  : constant Transition_Result :=
        Get_Offset_At_Time (Zone_Id, Epoch);
   begin
      Put_Line ("");
      Put_Line ("Test: Get_Offset_At_Time - UTC at epoch 0");

      Assert (Is_Ok (Result),
              "Should successfully get offset for UTC at epoch 0");
   end Test_UTC_At_Epoch_Zero;

   procedure Test_America_Los_Angeles_Summer is
      --  July 1, 2024 (summer - PDT, UTC-7)
      Zone_Id : constant Zone_Id_String :=
        Make_Zone_Id_String ("America/Los_Angeles");
      Summer_Epoch : constant Epoch_Seconds_Type := 1_719_792_000;
      Result : constant Transition_Result :=
        Get_Offset_At_Time (Zone_Id, Summer_Epoch);
   begin
      Put_Line ("");
      Put_Line ("Test: Get_Offset_At_Time - " &
                "Los Angeles summer (PDT)");

      Assert (Is_Ok (Result),
              "Should successfully get offset for LA in summer");
   end Test_America_Los_Angeles_Summer;

   procedure Test_America_New_York_Winter is
      --  January 15, 2024 (winter - EST, UTC-5)
      Zone_Id : constant Zone_Id_String :=
        Make_Zone_Id_String ("America/New_York");
      Winter_Epoch : constant Epoch_Seconds_Type := 1_705_305_600;
      Result : constant Transition_Result :=
        Get_Offset_At_Time (Zone_Id, Winter_Epoch);
   begin
      Put_Line ("");
      Put_Line ("Test: Get_Offset_At_Time - New York winter (EST)");

      Assert (Is_Ok (Result),
              "Should successfully get offset for NY in winter");
   end Test_America_New_York_Winter;

   procedure Test_Europe_London_DST is
      --  July 15, 2024 (summer - BST, UTC+1)
      Zone_Id : constant Zone_Id_String :=
        Make_Zone_Id_String ("Europe/London");
      Summer_Epoch : constant Epoch_Seconds_Type := 1_721_044_800;
      Result : constant Transition_Result :=
        Get_Offset_At_Time (Zone_Id, Summer_Epoch);
   begin
      Put_Line ("");
      Put_Line ("Test: Get_Offset_At_Time - London summer (BST)");

      Assert (Is_Ok (Result),
              "Should successfully get offset for London in summer");
   end Test_Europe_London_DST;

   procedure Test_Nonexistent_Zone is
      Zone_Id : constant Zone_Id_String :=
        Make_Zone_Id_String ("Invalid/Nonexistent");
      Epoch   : constant Epoch_Seconds_Type := 0;
      Result  : constant Transition_Result :=
        Get_Offset_At_Time (Zone_Id, Epoch);
   begin
      Put_Line ("");
      Put_Line ("Test: Get_Offset_At_Time - Nonexistent zone");

      Assert (Is_Error (Result),
              "Should return error for nonexistent zone");
   end Test_Nonexistent_Zone;

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
   Put_Line ("  TZif API Test: Get_Offset_At_Time");
   Put_Line ("  Testing TZif.API.Get_Offset_At_Time");
   Put_Line ("========================================================");

   Test_UTC_At_Epoch_Zero;
   Test_America_Los_Angeles_Summer;
   Test_America_New_York_Winter;
   Test_Europe_London_DST;
   Test_Nonexistent_Zone;

   Report_Results;

end Test_Get_Offset_At_Time;
