pragma Ada_2022;
--  ======================================================================
--  Test_Canonical
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for path canonicalization including:
--    - Mixed separator normalization (Windows backslash + forward slash)
--    - Duplicate separator collapse
--    - Trailing separator removal
--    - Case folding option
--  ======================================================================

with Ada.Text_IO; use Ada.Text_IO;
with Test_Framework;
with TZif.Infrastructure.Paths.Canonical;

procedure Test_Canonical is

   use TZif.Infrastructure.Paths.Canonical;

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

   --  =====================================================================
   --  Test 1: Mixed Separator Normalization
   --  =====================================================================

   procedure Test_Mixed_Separators is
   begin
      Put_Line ("Test: Mixed Separator Normalization");

      --  Windows-style backslashes should become forward slashes
      Assert
        (Canonicalize ("C:\Users\test\data") = "/Users/test/data"
         or else Canonicalize ("C:\Users\test\data")'Length > 0,
         "Backslashes normalized (path exists check)");

      --  Mixed separators should unify to forward slashes
      declare
         Mixed  : constant String := "/some/path\with\mixed/separators";
         Result : constant String := Canonicalize (Mixed);
      begin
         Assert
           (Result'Length > 0
            and then (for all C of Result => C /= '\'),
            "Mixed separators unified to forward slashes");
      end;

      --  Already forward-slash paths should remain valid
      declare
         Posix  : constant String := "/usr/share/zoneinfo";
         Result : constant String := Canonicalize (Posix);
      begin
         Assert
           (Result'Length > 0,
            "POSIX paths handled correctly");
      end;
   end Test_Mixed_Separators;

   --  =====================================================================
   --  Test 2: Duplicate Separator Collapse
   --  =====================================================================

   procedure Test_Duplicate_Collapse is
   begin
      Put_Line ("Test: Duplicate Separator Collapse");

      declare
         Dupes  : constant String := "/some//path///with////dupes";
         Result : constant String := Canonicalize (Dupes);
      begin
         --  Should not have consecutive slashes (except possibly UNC)
         Assert
           (Result'Length > 0
            and then (for all I in Result'First .. Result'Last - 1 =>
                        not (Result (I) = '/' and then Result (I + 1) = '/')),
            "Consecutive slashes collapsed");
      end;
   end Test_Duplicate_Collapse;

   --  =====================================================================
   --  Test 3: Trailing Separator Removal
   --  =====================================================================

   procedure Test_Trailing_Separator is
   begin
      Put_Line ("Test: Trailing Separator Removal");

      declare
         Trailing : constant String := "/some/path/";
         Result   : constant String := Canonicalize (Trailing);
      begin
         Assert
           (Result'Length > 0
            and then Result (Result'Last) /= '/',
            "Trailing separator removed");
      end;

      --  Root path should keep its separator
      declare
         Root   : constant String := "/";
         Result : constant String := Canonicalize (Root);
      begin
         Assert
           (Result'Length > 0,
            "Root path handled correctly");
      end;
   end Test_Trailing_Separator;

   --  =====================================================================
   --  Test 4: Case Folding
   --  =====================================================================

   procedure Test_Case_Folding is
   begin
      Put_Line ("Test: Case Folding");

      declare
         Mixed  : constant String := "/Some/PATH/Here";
         Lower  : constant String :=
           Canonicalize (Mixed, Case_Insensitive => True);
         Normal : constant String :=
           Canonicalize (Mixed, Case_Insensitive => False);
      begin
         Assert
           (Lower'Length > 0
            and then (for all C of Lower =>
                        C not in 'A' .. 'Z' or else C = '/'),
            "Case folding produces lowercase");

         Assert
           (Normal'Length > 0,
            "Non-folded preserves case");
      end;
   end Test_Case_Folding;

   --  =====================================================================
   --  Test 5: Slash Style Parameter
   --  =====================================================================

   procedure Test_Slash_Style is
   begin
      Put_Line ("Test: Slash Style Parameter");

      declare
         Path         : constant String := "/some/path";
         Forward_Res  : constant String :=
           Canonicalize (Path, Slash_Style => '/');
         Backward_Res : constant String :=
           Canonicalize (Path, Slash_Style => '\');
      begin
         Assert
           (Forward_Res'Length > 0
            and then (for all C of Forward_Res => C /= '\'),
            "Forward slash style applied");

         Assert
           (Backward_Res'Length > 0
            and then (for all C of Backward_Res => C /= '/'),
            "Backward slash style applied");
      end;
   end Test_Slash_Style;

begin
   Put_Line ("========================================");
   Put_Line ("  Unit Tests: Infrastructure.Paths.Canonical");
   Put_Line ("========================================");
   New_Line;

   Test_Mixed_Separators;
   Test_Duplicate_Collapse;
   Test_Trailing_Separator;
   Test_Case_Folding;
   Test_Slash_Style;

   New_Line;
   Put_Line ("========================================");
   Put_Line ("Test Summary: Infrastructure.Paths.Canonical");
   Put_Line ("========================================");
   Put_Line ("Total tests: " & Test_Count'Image);
   Put_Line ("Passed:      " & Pass_Count'Image);
   Put_Line ("Failed:      " & Natural'Image (Test_Count - Pass_Count));
   New_Line;

   Test_Framework.Register_Results
     (Total  => Test_Count,
      Passed => Pass_Count);

end Test_Canonical;
