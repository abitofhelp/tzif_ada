pragma Ada_2022;
--  ======================================================================
--  Test_Iana_Releases
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Iana Releases functionality.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with TZif.Domain.Value_Object.IANA_Releases;
with TZif.Domain.Value_Object.Source_Info;
with Test_Framework;
procedure Test_IANA_Releases is
   use TZif.Domain.Value_Object.IANA_Releases;
   use TZif.Domain.Value_Object.Source_Info;
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
   --  ========================================================================
   --  Test: Is_Known_Version - Known Versions
   --  ========================================================================
   procedure Test_Is_Known_Version_Valid is
      V2024b : constant Version_String_Type := Make_Version ("2024b");
      V2023c : constant Version_String_Type := Make_Version ("2023c");
      V2015a : constant Version_String_Type := Make_Version ("2015a");
   begin
      Put_Line ("Test: Is_Known_Version with valid versions");
      Assert
        (Is_Known_Version (V2024b), "Should recognize 2024b as known version");
      Assert
        (Is_Known_Version (V2023c), "Should recognize 2023c as known version");
      Assert
        (Is_Known_Version (V2015a), "Should recognize 2015a as known version");
   end Test_Is_Known_Version_Valid;
   --  ========================================================================
   --  Test: Is_Known_Version - Unknown Versions
   --  ========================================================================
   procedure Test_Is_Known_Version_Invalid is
      V9999z : constant Version_String_Type := Make_Version ("9999z");
      VEmpty : constant Version_String_Type := Make_Version ("");
      VBad   : constant Version_String_Type := Make_Version ("notaversion");
   begin
      Put_Line ("Test: Is_Known_Version with unknown versions");
      Assert
        (not Is_Known_Version (V9999z),
         "Should NOT recognize 9999z as known version");
      Assert
        (not Is_Known_Version (VEmpty),
         "Should NOT recognize empty string as known version");
      Assert
        (not Is_Known_Version (VBad),
         "Should NOT recognize 'notaversion' as known version");
   end Test_Is_Known_Version_Invalid;
   --  ========================================================================
   --  Test: Get_Expected_Zone_Count - Known Versions
   --  ========================================================================
   procedure Test_Get_Expected_Zone_Count_Valid is
      V2024b      : constant Version_String_Type := Make_Version ("2024b");
      V2023c      : constant Version_String_Type := Make_Version ("2023c");
      Count_2024b : constant Natural := Get_Expected_Zone_Count (V2024b);
      Count_2023c : constant Natural := Get_Expected_Zone_Count (V2023c);
   begin
      Put_Line ("Test: Get_Expected_Zone_Count with known versions");
      --  2024b has 417 zones, 2023c has 418 zones (from IANA_Releases table)
      Assert
        (Count_2024b = 417,
         "Should return 417 zones for 2024b (got" & Count_2024b'Image & ")");
      Assert
        (Count_2023c = 418,
         "Should return 418 zones for 2023c (got" & Count_2023c'Image & ")");
   end Test_Get_Expected_Zone_Count_Valid;
   --  ========================================================================
   --  Test: Get_Expected_Zone_Count - Unknown Version
   --  ========================================================================
   procedure Test_Get_Expected_Zone_Count_Unknown is
      V9999z : constant Version_String_Type := Make_Version ("9999z");
      Count  : constant Natural := Get_Expected_Zone_Count (V9999z);
   begin
      Put_Line ("Test: Get_Expected_Zone_Count with unknown version");
      Assert
        (Count = 0,
         "Should return 0 for unknown version (got" & Count'Image & ")");
   end Test_Get_Expected_Zone_Count_Unknown;
   --  ========================================================================
   --  Test: Get_Expected_SHA256 - Known Versions
   --  ========================================================================
   procedure Test_Get_Expected_SHA256_Valid is
      V2024b       : constant Version_String_Type := Make_Version ("2024b");
      V2023c       : constant Version_String_Type := Make_Version ("2023c");
      SHA256_2024b : constant String := Get_Expected_SHA256 (V2024b);
      SHA256_2023c : constant String := Get_Expected_SHA256 (V2023c);
   begin
      Put_Line ("Test: Get_Expected_SHA256 with known versions");
      --  SHA256 hashes are 64 characters (hex)
      Assert
        (SHA256_2024b'Length = 64,
         "Should return 64-char SHA256 for 2024b (got"
         & SHA256_2024b'Length'Image
         & ")");
      Assert
        (SHA256_2023c'Length = 64,
         "Should return 64-char SHA256 for 2023c (got"
         & SHA256_2023c'Length'Image
         & ")");
      --  Verify first few characters of known hashes
      Assert
        (SHA256_2024b (SHA256_2024b'First .. SHA256_2024b'First + 7)
         = "70e754db",
         "Should return correct SHA256 prefix for 2024b");
      Assert
        (SHA256_2023c (SHA256_2023c'First .. SHA256_2023c'First + 7)
         = "3f510b5d",
         "Should return correct SHA256 prefix for 2023c");
   end Test_Get_Expected_SHA256_Valid;
   --  ========================================================================
   --  Test: Get_Expected_SHA256 - Unknown Version
   --  ========================================================================
   procedure Test_Get_Expected_SHA256_Unknown is
      V9999z : constant Version_String_Type := Make_Version ("9999z");
      SHA256 : constant String := Get_Expected_SHA256 (V9999z);
   begin
      Put_Line ("Test: Get_Expected_SHA256 with unknown version");
      Assert (SHA256 = "", "Should return empty string for unknown version");
   end Test_Get_Expected_SHA256_Unknown;
   --  ========================================================================
   --  Test: Multiple Lookups
   --  ========================================================================
   procedure Test_Multiple_Lookups is
      V2022g : constant Version_String_Type := Make_Version ("2022g");
      Known  : constant Boolean := Is_Known_Version (V2022g);
      Count  : constant Natural := Get_Expected_Zone_Count (V2022g);
      SHA256 : constant String := Get_Expected_SHA256 (V2022g);
   begin
      Put_Line ("Test: Multiple lookups for same version");
      Assert (Known, "Should recognize 2022g as known");
      Assert (Count = 419, "Should return 419 zones for 2022g");
      Assert (SHA256'Length = 64, "Should return valid SHA256 for 2022g");
   end Test_Multiple_Lookups;
begin
   Put_Line ("====================================================");
   Put_Line ("  Running: IANA Releases Metadata Lookup Tests");
   Put_Line ("====================================================");
   New_Line;
   Test_Is_Known_Version_Valid;
   New_Line;
   Test_Is_Known_Version_Invalid;
   New_Line;
   Test_Get_Expected_Zone_Count_Valid;
   New_Line;
   Test_Get_Expected_Zone_Count_Unknown;
   New_Line;
   Test_Get_Expected_SHA256_Valid;
   New_Line;
   Test_Get_Expected_SHA256_Unknown;
   New_Line;
   Test_Multiple_Lookups;
   New_Line;
   --  Summary
   Put_Line ("====================================================");
   Put_Line ("  Tests Passed: " & Pass_Count'Image & " /" & Test_Count'Image);
   Put_Line ("====================================================");

   Test_Framework.Register_Results (Test_Count, Pass_Count);

   if Pass_Count /= Test_Count then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Test_IANA_Releases;