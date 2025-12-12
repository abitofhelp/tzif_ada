pragma Ada_2022;
--  ======================================================================
--  Test_Version
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for TZif.Version package accessors.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Test_Framework;
with TZif.Version;

procedure Test_Version is

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
   Put_Line ("Test: TZif.Version accessors");

   --  Note: These are compile-time verifiable assertions.
   --  The compiler identifies them as always-true, which is expected.
   pragma Warnings (Off, "condition is always True");

   --  Test Major version
   Assert (TZif.Version.Major = 99, "Major version is 99");

   --  Test Minor version
   Assert (TZif.Version.Minor = 99, "Minor version is 99");

   --  Test Patch version
   Assert (TZif.Version.Patch = 99, "Patch version is 99");

   --  Test Version string
   Assert (TZif.Version.Version'Length > 0, "Version string is non-empty");
   Assert (TZif.Version.Version = "99.99.99", "Version string is 99.99.99");

   pragma Warnings (On, "condition is always True");

   --  Test Is_Stable function
   Assert
     (TZif.Version.Is_Stable = (not TZif.Version.Is_Prerelease),
      "Is_Stable is inverse of Is_Prerelease");

   --  For current 99.99.99 release, verify stable status
   Assert (TZif.Version.Is_Stable, "Version 99.99.99 is stable");
   Assert (not TZif.Version.Is_Prerelease, "Version 99.99.99 is not prerelease");
   Assert (not TZif.Version.Is_Development, "Version 99.99.99 is not dev");

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

end Test_Version;
