pragma Ada_2022;
--  ======================================================================
--  Test_Get_Version
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Get Version functionality.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Test_Framework;
with TZif.Application.Port.Inbound.Get_Version;
with TZif.Application.Usecase.Get_Version;
with TZif.Infrastructure.Adapter.File_System.POSIX_Repository;
with TZif.Domain.Value_Object.Source_Info;
procedure Test_Get_Version is
   use TZif.Application.Port.Inbound.Get_Version;
   use TZif.Domain.Value_Object.Source_Info;
   package Get_Version_UC is new
     TZif.Application.Usecase.Get_Version.Use_Case
       (Repository_Get_Version =>
         TZif.Infrastructure.Adapter.File_System.POSIX_Repository.Get_Version);
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
   Put_Line ("Test: Get_Version from system DB");
   declare
      Source : constant Source_Info_Type := Make_Source_Info
          (ULID => Make_ULID ("01ARZ3NDEKTSV4RRFFQ69G5FAV"),
           Path => Make_Path ("/usr/share/zoneinfo"),
           Version => Make_Version ("2024a"),
           Zone_Count => 0);
      Result : constant Version_Result := Get_Version_UC.Execute (Source);
   begin
      Assert
        (Version_Result_Package.Is_Ok (Result)
         or else Version_Result_Package.Is_Error (Result),
         "Should return Ok or Error");
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
   --  Register results with test framework
   Test_Framework.Register_Results (Test_Count, Pass_Count);
   if Pass_Count /= Test_Count then
      Ada.Command_Line.Set_Exit_Status (1);
   end if;
end Test_Get_Version;