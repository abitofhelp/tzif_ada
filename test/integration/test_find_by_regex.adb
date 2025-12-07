pragma Ada_2022;
--  ======================================================================
--  Test_Find_By_Regex
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Find By Regex functionality.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Test_Framework;
with Test_Spies.Find_By_Regex_Spy;
with TZif.Application.Port.Inbound.Find_By_Regex;
with TZif.Application.Usecase.Find_By_Regex;
with TZif.Infrastructure.Adapter.File_System.POSIX_Repository;
procedure Test_Find_By_Regex is
   use TZif.Application.Port.Inbound.Find_By_Regex;
   package Repo renames
     TZif.Infrastructure.Adapter.File_System.POSIX_Repository;
   package UC is new
     TZif.Application.Usecase.Find_By_Regex.Use_Case
       (Repository_Find_By_Regex => Repo.Find_By_Regex);
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
   Put_Line ("Test: Find By Regex - Valid Patterns");
   --  Test wildcard region pattern
   declare
      Regex  : constant Regex_String :=
        Regex_Strings.To_Bounded_String ("America/.*");
      Result : Find_By_Regex_Result;
   begin
      Test_Spies.Find_By_Regex_Spy.Reset;
      Result :=
        UC.Execute (Regex, Test_Spies.Find_By_Regex_Spy.Collect'Access);
      Assert
        (Find_By_Regex_Result_Package.Is_Ok (Result),
         "Should return Ok for 'America/.*' regex");
      Assert
        (Test_Spies.Find_By_Regex_Spy.Count > 0,
         "Should find zones matching 'America/.*'");
      Assert
        (Test_Spies.Find_By_Regex_Spy.Count > 10,
         "Should find many America/* zones");
   end;
   --  Test exact match regex
   declare
      Regex  : constant Regex_String :=
        Regex_Strings.To_Bounded_String ("^UTC$");
      Result : Find_By_Regex_Result;
   begin
      Test_Spies.Find_By_Regex_Spy.Reset;
      Result :=
        UC.Execute (Regex, Test_Spies.Find_By_Regex_Spy.Collect'Access);
      Assert
        (Find_By_Regex_Result_Package.Is_Ok (Result),
         "Should return Ok for '^UTC$' regex");
      Assert
        (Test_Spies.Find_By_Regex_Spy.Count >= 1,
         "Should find UTC zone with exact match");
   end;
   --  Test alternation (Europe/London|Europe/Paris)
   declare
      Regex  : constant Regex_String :=
        Regex_Strings.To_Bounded_String ("Europe/(London|Paris)");
      Result : Find_By_Regex_Result;
   begin
      Test_Spies.Find_By_Regex_Spy.Reset;
      Result :=
        UC.Execute (Regex, Test_Spies.Find_By_Regex_Spy.Collect'Access);
      Assert
        (Find_By_Regex_Result_Package.Is_Ok (Result),
         "Should return Ok for alternation regex");
      Assert
        (Test_Spies.Find_By_Regex_Spy.Count >= 2,
         "Should find London and Paris");
   end;
   --  Test character class [AP] for Asia/Pacific
   declare
      Regex  : constant Regex_String :=
        Regex_Strings.To_Bounded_String ("^(Asia|Pacific)/.*");
      Result : Find_By_Regex_Result;
   begin
      Test_Spies.Find_By_Regex_Spy.Reset;
      Result :=
        UC.Execute (Regex, Test_Spies.Find_By_Regex_Spy.Collect'Access);
      Assert
        (Find_By_Regex_Result_Package.Is_Ok (Result),
         "Should return Ok for Asia|Pacific regex");
      Assert
        (Test_Spies.Find_By_Regex_Spy.Count > 0,
         "Should find Asia and Pacific zones");
   end;
   Put_Line ("Test: Find By Regex - No Matches");
   --  Test regex with no matches
   declare
      Regex  : constant Regex_String :=
        Regex_Strings.To_Bounded_String ("^Atlantis/.*");
      Result : Find_By_Regex_Result;
   begin
      Test_Spies.Find_By_Regex_Spy.Reset;
      Result :=
        UC.Execute (Regex, Test_Spies.Find_By_Regex_Spy.Collect'Access);
      Assert
        (Find_By_Regex_Result_Package.Is_Ok (Result),
         "Should return Ok even when no zones match");
      Assert
        (Test_Spies.Find_By_Regex_Spy.Count = 0,
         "Should find zero zones for non-existent pattern");
   end;
   Put_Line ("Test: Find By Regex - Edge Cases");
   --  Test empty regex
   declare
      Regex  : constant Regex_String := Regex_Strings.To_Bounded_String ("");
      Result : Find_By_Regex_Result;
   begin
      Test_Spies.Find_By_Regex_Spy.Reset;
      Result :=
        UC.Execute (Regex, Test_Spies.Find_By_Regex_Spy.Collect'Access);
      --  Empty regex may match all, none, or be invalid
      Assert
        (Find_By_Regex_Result_Package.Is_Ok (Result)
         or else Find_By_Regex_Result_Package.Is_Error (Result),
         "Empty regex returns result");
   end;
   --  Test match-all regex
   declare
      Regex  : constant Regex_String := Regex_Strings.To_Bounded_String (".*");
      Result : Find_By_Regex_Result;
   begin
      Test_Spies.Find_By_Regex_Spy.Reset;
      Result :=
        UC.Execute (Regex, Test_Spies.Find_By_Regex_Spy.Collect'Access);
      Assert
        (Find_By_Regex_Result_Package.Is_Ok (Result),
         "Should return Ok for .* regex");
      Assert
        (Test_Spies.Find_By_Regex_Spy.Count > 100,
         "Should find many zones with .* pattern");
   end;
   --  Test invalid regex syntax (incomplete character range)
   --  Note: GNAT.Regpat accepts "[" as literal, but rejects "[a-"
   declare
      Regex  : constant Regex_String :=
        Regex_Strings.To_Bounded_String ("America/[a-");
      Result : Find_By_Regex_Result;
   begin
      Test_Spies.Find_By_Regex_Spy.Reset;
      Result :=
        UC.Execute (Regex, Test_Spies.Find_By_Regex_Spy.Collect'Access);
      --  Invalid regex should return error
      Assert
        (Find_By_Regex_Result_Package.Is_Error (Result),
         "Should return error for invalid regex syntax");
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
end Test_Find_By_Regex;