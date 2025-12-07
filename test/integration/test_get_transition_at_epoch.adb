pragma Ada_2022;
--  ======================================================================
--  Test_Get_Transition_At_Epoch
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Get Transition At Epoch functionality.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Test_Framework;
with TZif.Application.Port.Inbound.Get_Transition_At_Epoch;
with TZif.Application.Usecase.Get_Transition_At_Epoch;
with TZif.Infrastructure.Adapter.File_System.POSIX_Repository;
with TZif.Domain.Value_Object.Transition_Info;
with TZif.Domain.Value_Object.Epoch_Seconds;
procedure Test_Get_Transition_At_Epoch is
   use TZif.Application.Port.Inbound.Get_Transition_At_Epoch;
   use TZif.Domain.Value_Object.Transition_Info;
   use TZif.Domain.Value_Object.Epoch_Seconds;
   package UC is new
     TZif.Application.Usecase.Get_Transition_At_Epoch.Use_Case
       (Repository_Get_Transition_At_Epoch => TZif
            .Infrastructure
            .Adapter
            .File_System
            .POSIX_Repository
            .Get_Transition_At_Epoch);
   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;
   procedure Assert (Condition : Boolean; Test_Name : String) is
   begin
      Test_Count := Test_Count + 1;
      if Condition then
         Pass_Count := Pass_Count + 1;
         Put_Line ("  [PASS] " & Test_Name);
      else
         Put_Line ("  [FAIL] " & Test_Name);
      end if;
   end Assert;
begin
   Put_Line ("Test: Get Transition At Epoch - Valid Zones");
   --  Test UTC at epoch 0 (1970-01-01 00:00:00)
   declare
      Result : constant Get_Transition_Result :=
        UC.Execute (Zone_Id_Strings.To_Bounded_String ("UTC"), 0);
   begin
      Assert
        (Get_Transition_Result_Package.Is_Ok (Result),
         "Should return Ok for UTC at epoch 0");
      if Get_Transition_Result_Package.Is_Ok (Result) then
         declare
            Info : constant Transition_Info_Type :=
              Get_Transition_Result_Package.Value (Result);
         begin
            Assert
              (Get_UTC_Offset_Seconds (Info) = 0,
               "UTC offset should be 0 for UTC");
            Assert
              (not Is_Daylight_Saving (Info),
               "UTC should not be daylight saving");
            Assert
              (Get_Abbreviation (Info) = "UTC",
               "UTC abbreviation should be UTC");
         end;
      end if;
   end;
   --  Test America/New_York in winter (standard time)
   --  Jan 1, 2020 00:00:00 UTC = epoch 1577836800
   declare
      Result : constant Get_Transition_Result := UC.Execute
          (Zone_Id_Strings.To_Bounded_String ("America/New_York"),
           1_577_836_800);
   begin
      Assert
        (Get_Transition_Result_Package.Is_Ok (Result),
         "Should return Ok for America/New_York");
      if Get_Transition_Result_Package.Is_Ok (Result) then
         declare
            Info : constant Transition_Info_Type :=
              Get_Transition_Result_Package.Value (Result);
         begin
            Assert
              (Get_UTC_Offset_Seconds (Info) = -18_000,
               "EST offset should be -5 hours (-18000 seconds)");
            Assert
              (not Is_Daylight_Saving (Info),
               "Should be standard time in January");
            Assert
              (Get_Abbreviation (Info) = "EST",
               "Abbreviation should be EST in winter");
         end;
      end if;
   end;
   --  Test America/Los_Angeles in summer (daylight saving)
   --  July 1, 2020 00:00:00 UTC = epoch 1593561600
   declare
      Result : constant Get_Transition_Result := UC.Execute
          (Zone_Id_Strings.To_Bounded_String ("America/Los_Angeles"),
           1_593_561_600);
   begin
      Assert
        (Get_Transition_Result_Package.Is_Ok (Result),
         "Should return Ok for America/Los_Angeles");
      if Get_Transition_Result_Package.Is_Ok (Result) then
         declare
            Info : constant Transition_Info_Type :=
              Get_Transition_Result_Package.Value (Result);
         begin
            Assert
              (Get_UTC_Offset_Seconds (Info) = -25_200,
               "PDT offset should be -7 hours (-25200 seconds)");
            Assert
              (Is_Daylight_Saving (Info), "Should be daylight saving in July");
            Assert
              (Get_Abbreviation (Info) = "PDT",
               "Abbreviation should be PDT in summer");
         end;
      end if;
   end;
   Put_Line ("Test: Get Transition At Epoch - Invalid Inputs");
   --  Test invalid zone
   declare
      Result : constant Get_Transition_Result :=
        UC.Execute (Zone_Id_Strings.To_Bounded_String ("Invalid/Zone"), 0);
   begin
      Assert
        (Get_Transition_Result_Package.Is_Error (Result),
         "Should return error for invalid zone");
   end;
   --  Test empty zone ID
   declare
      Result : constant Get_Transition_Result :=
        UC.Execute (Zone_Id_Strings.To_Bounded_String (""), 0);
   begin
      Assert
        (Get_Transition_Result_Package.Is_Error (Result),
         "Should return error for empty zone ID");
   end;
   Put_Line ("Test: Get Transition At Epoch - Edge Cases");
   --  Test negative epoch (before 1970)
   declare
      Result : constant Get_Transition_Result :=
        UC.Execute (Zone_Id_Strings.To_Bounded_String ("UTC"), -86_400);
   begin
      Assert
        (Get_Transition_Result_Package.Is_Ok (Result)
         or else Get_Transition_Result_Package.Is_Error (Result),
         "Handles negative epoch (before 1970)");
   end;
   --  Test future epoch (year 2050)
   --  Jan 1, 2050 00:00:00 UTC = epoch 2524608000
   declare
      Result : constant Get_Transition_Result :=
        UC.Execute (Zone_Id_Strings.To_Bounded_String ("UTC"), 2_524_608_000);
   begin
      Assert
        (Get_Transition_Result_Package.Is_Ok (Result),
         "Should handle future epochs");
   end;
   --  Summary
   Put_Line ("====================================================");
   Put_Line
     ("  Results:" & Pass_Count'Image & " /" & Test_Count'Image & " passed");
   if Pass_Count = Test_Count then
      Put_Line ("  Status: ALL TESTS PASSED");
   else
      Put_Line ("  Status: FAILURES DETECTED");
   end if;
   Put_Line ("====================================================");
   Test_Framework.Register_Results (Test_Count, Pass_Count);
   if Pass_Count /= Test_Count then
      Ada.Command_Line.Set_Exit_Status (1);
   end if;
end Test_Get_Transition_At_Epoch;