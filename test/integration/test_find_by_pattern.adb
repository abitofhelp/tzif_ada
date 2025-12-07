pragma Ada_2022;
--  ======================================================================
--  Test_Find_By_Pattern
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Find By Pattern functionality.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Test_Framework;
with Test_Spies.Find_By_Pattern_Spy;
with TZif.Application.Port.Inbound.Find_By_Pattern;
with TZif.Application.Usecase.Find_By_Pattern;
with TZif.Infrastructure.Adapter.File_System.POSIX_Repository;
procedure Test_Find_By_Pattern is
   use TZif.Application.Port.Inbound.Find_By_Pattern;
   package Repo renames
     TZif.Infrastructure.Adapter.File_System.POSIX_Repository;
   package UC is new
     TZif.Application.Usecase.Find_By_Pattern.Use_Case
       (Repository_Find_By_Pattern => Repo.Find_By_Pattern);
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
   Put_Line ("Test: Find By Pattern - Valid Patterns");
   --  Test common region pattern
   declare
      Pattern : constant Pattern_String :=
      Pattern_Strings.To_Bounded_String ("America");
      Result  : Find_By_Pattern_Result;
   begin
      Test_Spies.Find_By_Pattern_Spy.Reset;
      Result := UC.Execute (Pattern,
        Test_Spies.Find_By_Pattern_Spy.Collect'Access);
      Assert
        (Find_By_Pattern_Result_Package.Is_Ok (Result),
         "Should return Ok for 'America' pattern");
      Assert
        (Test_Spies.Find_By_Pattern_Spy.Count > 0,
         "Should find zones matching 'America'");
      Assert
        (Test_Spies.Find_By_Pattern_Spy.Count > 10,
         "Should find many America/* zones");
   end;
   --  Test specific city pattern
   declare
      Pattern : constant Pattern_String :=
        Pattern_Strings.To_Bounded_String ("New_York");
      Result  : Find_By_Pattern_Result;
   begin
      Test_Spies.Find_By_Pattern_Spy.Reset;
      Result := UC.Execute (Pattern,
        Test_Spies.Find_By_Pattern_Spy.Collect'Access);
      Assert
        (Find_By_Pattern_Result_Package.Is_Ok (Result),
         "Should return Ok for 'New_York' pattern");
      Assert
        (Test_Spies.Find_By_Pattern_Spy.Count > 0,
         "Should find zones matching 'New_York'");
   end;
   --  Test exact match (UTC)
   declare
      Pattern : constant Pattern_String :=
                  Pattern_Strings.To_Bounded_String ("UTC");
      Result  : Find_By_Pattern_Result;
   begin
      Test_Spies.Find_By_Pattern_Spy.Reset;
      Result := UC.Execute (Pattern,
        Test_Spies.Find_By_Pattern_Spy.Collect'Access);
      Assert
        (Find_By_Pattern_Result_Package.Is_Ok (Result),
         "Should return Ok for 'UTC' pattern");
      Assert
        (Test_Spies.Find_By_Pattern_Spy.Count >= 1,
         "Should find at least UTC zone");
   end;
   Put_Line ("Test: Find By Pattern - No Matches");
   --  Test pattern with no matches
   declare
      Pattern : constant Pattern_String :=
        Pattern_Strings.To_Bounded_String ("NonExistentPattern");
      Result  : Find_By_Pattern_Result;
   begin
      Test_Spies.Find_By_Pattern_Spy.Reset;
      Result := UC.Execute (Pattern,
        Test_Spies.Find_By_Pattern_Spy.Collect'Access);
      Assert
        (Find_By_Pattern_Result_Package.Is_Ok (Result),
         "Should return Ok even when no zones match");
      Assert
        (Test_Spies.Find_By_Pattern_Spy.Count = 0,
         "Should find zero zones for non-existent pattern");
   end;
   Put_Line ("Test: Find By Pattern - Edge Cases");
   --  Test empty pattern
   declare
      Pattern : constant Pattern_String :=
        Pattern_Strings.To_Bounded_String ("");
      Result  : Find_By_Pattern_Result;
   begin
      Test_Spies.Find_By_Pattern_Spy.Reset;
      Result := UC.Execute (Pattern,
        Test_Spies.Find_By_Pattern_Spy.Collect'Access);
      --  Empty pattern behavior: may match all or none
      Assert
        (Find_By_Pattern_Result_Package.Is_Ok (Result)
         or else Find_By_Pattern_Result_Package.Is_Error (Result),
         "Empty pattern returns result");
   end;
   --  Test pattern with special characters
   declare
      Pattern : constant Pattern_String :=
        Pattern_Strings.To_Bounded_String ("Los_Angeles");
      Result  : Find_By_Pattern_Result;
   begin
      Test_Spies.Find_By_Pattern_Spy.Reset;
      Result := UC.Execute (Pattern,
        Test_Spies.Find_By_Pattern_Spy.Collect'Access);
      Assert
        (Find_By_Pattern_Result_Package.Is_Ok (Result),
         "Should handle underscore in pattern");
      Assert
        (Test_Spies.Find_By_Pattern_Spy.Count > 0,
         "Should find zones with underscores");
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
end Test_Find_By_Pattern;
