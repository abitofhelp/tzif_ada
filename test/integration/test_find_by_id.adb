pragma Ada_2022;
--  ======================================================================
--  Test_Find_By_Id
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Find By Id functionality.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Test_Framework;
with TZif.Application.Port.Inbound.Find_By_Id;
with TZif.Application.Usecase.Find_By_Id;
with TZif.Infrastructure.Adapter.File_System.POSIX_Repository;
with TZif.Domain.Value_Object.Zone_Id;
procedure Test_Find_By_Id is
   use TZif.Application.Port.Inbound.Find_By_Id;
   use TZif.Domain.Value_Object.Zone_Id;
   package Find_By_Id_UC is new
     TZif.Application.Usecase.Find_By_Id.Use_Case
       (Repository_Find_By_Id =>
          TZif.Infrastructure.Adapter.File_System.POSIX_Repository
          .Find_By_Id);
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
   Put_Line ("Test: Find By ID - Valid Zones");
   --  Test UTC (most common zone)
   declare
      Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("UTC");
      Result  : constant Find_By_Id_Result_Type :=
        Find_By_Id_UC.Execute (Zone_Id);
   begin
      Assert (Find_By_Id_Result.Is_Ok (Result), "Should find UTC zone");
   end;
   --  Test common zone with region
   declare
      Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("America/New_York");
      Result  : constant Find_By_Id_Result_Type :=
        Find_By_Id_UC.Execute (Zone_Id);
   begin
      Assert
        (Find_By_Id_Result.Is_Ok (Result),
         "Should find America/New_York zone");
   end;
   --  Test European zone
   declare
      Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("Europe/London");
      Result  : constant Find_By_Id_Result_Type :=
        Find_By_Id_UC.Execute (Zone_Id);
   begin
      Assert
        (Find_By_Id_Result.Is_Ok (Result), "Should find Europe/London zone");
   end;
   Put_Line ("Test: Find By ID - Invalid Zones");
   --  Test completely invalid zone name
   declare
      Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("Invalid/Zone");
      Result  : constant Find_By_Id_Result_Type :=
        Find_By_Id_UC.Execute (Zone_Id);
   begin
      Assert
        (Find_By_Id_Result.Is_Error (Result),
         "Should return error for non-existent zone");
   end;
   --  Test zone with typo
   declare
      Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("America/New_Yrok");
      Result  : constant Find_By_Id_Result_Type :=
        Find_By_Id_UC.Execute (Zone_Id);
   begin
      Assert
        (Find_By_Id_Result.Is_Error (Result),
         "Should return error for misspelled zone");
   end;
   Put_Line ("Test: Find By ID - Edge Cases");
   --  Test empty zone ID
   declare
      Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("");
      Result  : constant Find_By_Id_Result_Type :=
        Find_By_Id_UC.Execute (Zone_Id);
   begin
      Assert
        (Find_By_Id_Result.Is_Error (Result),
         "Should return error for empty zone ID");
   end;
   --  Test zone with only region (no city)
   declare
      Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("America");
      Result  : constant Find_By_Id_Result_Type :=
        Find_By_Id_UC.Execute (Zone_Id);
   begin
      Assert
        (Find_By_Id_Result.Is_Error (Result),
         "Should return error for region without city");
   end;
   --  Test case sensitivity
   declare
      Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("utc");
      Result  : constant Find_By_Id_Result_Type :=
        Find_By_Id_UC.Execute (Zone_Id);
   begin
      --  Case sensitivity behavior depends on implementation
      --  This documents the actual behavior
      if Find_By_Id_Result.Is_Error (Result) then
         Put_Line ("  [INFO] Zone IDs are case-sensitive");
      else
         Put_Line ("  [INFO] Zone IDs are case-insensitive");
      end if;
      --  Always pass - this is just documenting behavior
      Assert (True, "Case sensitivity test completed");
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
end Test_Find_By_Id;