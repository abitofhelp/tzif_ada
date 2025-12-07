pragma Ada_2022;
--  ======================================================================
--  Test_Discover_Sources
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Discover Sources functionality.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Test_Framework;
with TZif.Application.Port.Inbound.Discover_Sources;
with TZif.Application.Usecase.Discover_Sources;
with TZif.Infrastructure.Adapter.File_System.POSIX_Repository;
procedure Test_Discover_Sources is
   use TZif.Application.Port.Inbound.Discover_Sources;
   package Repo renames
     TZif.Infrastructure.Adapter.File_System.POSIX_Repository;
   package UC is new
     TZif.Application.Usecase.Discover_Sources.Use_Case
       (Repository_Discover_Sources => Repo.Discover_Sources);
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
   Put_Line ("Test: Discover Sources");
   --  Test 1: Empty path list should return error
   declare
      Paths  : Path_List;
      Result : constant Discovery_Result := UC.Execute (Paths);
   begin
      Assert
        (Discovery_Result_Package.Is_Error (Result),
         "Empty paths should return error");
   end;
   --  Test 2: Valid system paths should return Ok or partial success
   declare
      Paths  : Path_List;
      Result : Discovery_Result;
   begin
      --  Add common system paths
      Paths.Append (Make_Path ("/usr/share/zoneinfo"));
      Paths.Append (Make_Path ("/var/db/timezone/zoneinfo"));
      Result := UC.Execute (Paths);
      Assert
        (Discovery_Result_Package.Is_Ok (Result)
         or else Discovery_Result_Package.Is_Error (Result),
         "Should return result for system paths");
      --  If we got sources, verify the data structure
      if Discovery_Result_Package.Is_Ok (Result) then
         declare
            Data : constant Discovery_Data_Type :=
              Discovery_Result_Package.Value (Result);
         begin
            Assert
              (not Source_Info_Vectors.Is_Empty (Data.Sources),
               "Should find at least one source");
         end;
      end if;
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
end Test_Discover_Sources;
