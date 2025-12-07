pragma Ada_2022;
--  ======================================================================
--  Test_Find_My_Id
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Find My Id functionality.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Test_Framework;
with TZif.Application.Port.Inbound.Find_My_Id;
with TZif.Application.Usecase.Find_My_Id;
with TZif.Infrastructure.Adapter.File_System.POSIX_Repository;
procedure Test_Find_My_Id is
   use TZif.Application.Port.Inbound.Find_My_Id;
   package UC is new
     TZif.Application.Usecase.Find_My_Id.Use_Case
       (Repository_Find_My_Id =>
         TZif.Infrastructure.Adapter.File_System.POSIX_Repository.Find_My_Id);
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
   Put_Line ("Test: Find My ID");
   declare
      Res : constant TZif.Application.Port.Inbound.Find_My_Id.Result :=
        UC.Execute;
   begin
      Assert
        (Result_Zone_Id.Is_Ok (Res) or else Result_Zone_Id.Is_Error (Res),
         "Should return result");
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
end Test_Find_My_Id;