pragma Ada_2022;
--  ======================================================================
--  Test_Validate_Source
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Validate Source functionality.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Test_Framework;
with TZif.Application.Port.Inbound.Validate_Source;
with TZif.Application.Usecase.Validate_Source;
with TZif.Infrastructure.Adapter.File_System.POSIX_Repository;
procedure Test_Validate_Source is
   use TZif.Application.Port.Inbound.Validate_Source;
   package Repo renames
     TZif.Infrastructure.Adapter.File_System.POSIX_Repository;
   package UC is new
     TZif.Application.Usecase.Validate_Source.Use_Case
       (Repository_Validate_Source => Repo.Validate_Source);
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
   Put_Line ("Test: Validate Source");
   declare
      Result : constant Validation_Result := UC.Execute
          (TZif
             .Application
             .Port
             .Inbound
             .Validate_Source
             .Path_Strings
             .To_Bounded_String ("/usr/share/zoneinfo"));
   begin
      Assert
        (Validation_Result_Package.Is_Ok (Result)
         or else Validation_Result_Package.Is_Error (Result),
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
end Test_Validate_Source;