pragma Ada_2022;
--  ======================================================================
--  Test_Find_By_Region
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Find By Region functionality.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Test_Framework;
with Test_Spies.Find_By_Region_Spy;
with TZif.Application.Port.Inbound.Find_By_Region;
with TZif.Application.Usecase.Find_By_Region;
with TZif.Infrastructure.Adapter.File_System.POSIX_Repository;
procedure Test_Find_By_Region is
   use TZif.Application.Port.Inbound.Find_By_Region;
   package Repo renames
     TZif.Infrastructure.Adapter.File_System.POSIX_Repository;
   package UC is new
     TZif.Application.Usecase.Find_By_Region.Use_Case
       (Repository_Find_By_Region => Repo.Find_By_Region);
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
   Region     : constant Region_String :=
     Region_Strings.To_Bounded_String ("America");
   Result     : Find_By_Region_Result;
begin
   Put_Line ("Test: Find By Region");
   Test_Spies.Find_By_Region_Spy.Reset;
   Result := UC.Execute (Region, Test_Spies.Find_By_Region_Spy.Collect'Access);
   Assert
     (Find_By_Region_Result_Package.Is_Ok (Result),
      "Should return Ok for valid region");
   Assert
     (Test_Spies.Find_By_Region_Spy.Count > 0,
      "Should find at least one zone in region 'America'");
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
end Test_Find_By_Region;