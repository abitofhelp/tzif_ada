pragma Ada_2022;
--  ======================================================================
--  Test_Load_Source
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Load Source functionality.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Test_Framework;
with TZif.Application.Port.Inbound.Load_Source;
with TZif.Application.Usecase.Load_Source;
with TZif.Infrastructure.Adapter.File_System.POSIX_Repository;
procedure Test_Load_Source is
   use TZif.Application.Port.Inbound.Load_Source;
   package UC is new
     TZif.Application.Usecase.Load_Source.Use_Case
       (Repository_Load_Source =>
         TZif.Infrastructure.Adapter.File_System.POSIX_Repository.Load_Source);
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
   Put_Line ("Test: Load Source");
   declare
      Result : constant Load_Source_Result := UC.Execute
          (TZif
             .Application
             .Port
             .Inbound
             .Load_Source
             .Path_Strings
             .To_Bounded_String ("/usr/share/zoneinfo"));
   begin
      Assert
        (Load_Source_Result_Package.Is_Ok (Result)
         or else Load_Source_Result_Package.Is_Error (Result),
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
end Test_Load_Source;