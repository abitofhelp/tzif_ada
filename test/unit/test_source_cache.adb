pragma Ada_2022;
--  ======================================================================
--  Test_Source_Cache
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Source Cache functionality.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with TZif.Infrastructure.Cache.Source_Cache;
with TZif.Domain.Value_Object.Source_Info;
procedure Test_Source_Cache is
   use TZif.Infrastructure.Cache.Source_Cache;
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
   Cache      : Source_Cache_Type;
   --  ========================================================================
   --  Test: Initial State
   --  ========================================================================
   procedure Test_Initial_State is
   begin
      Put_Line ("Test: Initial cache state");
      Assert (Cache.Is_Empty, "New cache should be empty");
      Assert (Cache.Size = 0, "New cache should have size 0");
   end Test_Initial_State;
   --  ========================================================================
   --  Test: Insert and Retrieve
   --  ========================================================================
   procedure Test_Insert_Retrieve is
      Path1        : constant Path_String_Type :=
        Make_Path ("/tmp/tzdata/2024b");
      Canonical1   : constant Path_String_Type :=
        Make_Path ("/tmp/tzdata/2024b");
      ULID1        : constant ULID_Type :=
        Make_ULID ("01HQXY123456789ABCDEFGHIJK");
      Version1     : constant Version_String_Type :=
        Make_Version ("2024b");
      Source_Info1 : constant Source_Info_Type :=
        Make_Source_Info
          (ULID       => ULID1,
           Path       => Path1,
           Version    => Version1,
           Zone_Count => 599);
      Entry1       : constant Source_Entry :=
        Make_Source_Entry (Source_Info1, Canonical1);
   begin
      Put_Line ("Test: Insert and retrieve source");
      Cache.Insert (Path1, Entry1);
      Assert
        (not Cache.Is_Empty,
         "Cache should not be empty after insert");
      Assert (Cache.Size = 1, "Cache should have size 1");
      Assert
        (Cache.Contains (Path1),
         "Cache should contain inserted path");
      declare
         Result : constant Source_Option_Type := Cache.Get (Path1);
      begin
         Assert
           (Source_Option.Is_Some (Result),
            "Get should return Some for existing path");
      end;
   end Test_Insert_Retrieve;
   --  ========================================================================
   --  Test: Contains Non-Existent
   --  ========================================================================
   procedure Test_Contains_Nonexistent is
      Path_None : constant Path_String_Type := Make_Path ("/nonexistent/path");
   begin
      Put_Line ("Test: Contains non-existent path");
      Assert
        (not Cache.Contains (Path_None),
         "Should return False for non-existent path");
      declare
         Result : constant Source_Option_Type := Cache.Get (Path_None);
      begin
         Assert
           (Source_Option.Is_None (Result),
            "Get should return None for non-existent path");
      end;
   end Test_Contains_Nonexistent;
   --  ========================================================================
   --  Test: Insert Multiple
   --  ========================================================================
   procedure Test_Insert_Multiple is
      Path2        : constant Path_String_Type :=
        Make_Path ("/tmp/tzdata/2023c");
      Canonical2   : constant Path_String_Type :=
        Make_Path ("/tmp/tzdata/2023c");
      ULID2        : constant ULID_Type :=
        Make_ULID ("01HQXY987654321ZYXWVUTSRQP");
      Version2     : constant Version_String_Type :=
        Make_Version ("2023c");
      Source_Info2 : constant Source_Info_Type :=
        Make_Source_Info
          (ULID       => ULID2,
           Path       => Path2,
           Version    => Version2,
           Zone_Count => 596);
      Entry2       : constant Source_Entry :=
        Make_Source_Entry (Source_Info2, Canonical2);
      Path3        : constant Path_String_Type :=
        Make_Path ("/tmp/tzdata/2022a");
      Canonical3   : constant Path_String_Type :=
        Make_Path ("/tmp/tzdata/2022a");
      ULID3        : constant ULID_Type :=
        Make_ULID ("01HQXYABCDEFGHIJKLMNOPQRST");
      Version3     : constant Version_String_Type :=
        Make_Version ("2022a");
      Source_Info3 : constant Source_Info_Type :=
        Make_Source_Info
          (ULID       => ULID3,
           Path       => Path3,
           Version    => Version3,
           Zone_Count => 590);
      Entry3       : constant Source_Entry :=
        Make_Source_Entry (Source_Info3, Canonical3);
   begin
      Put_Line ("Test: Insert multiple sources");
      Cache.Insert (Path2, Entry2);
      Cache.Insert (Path3, Entry3);
      Assert
        (Cache.Size = 3,
         "Cache should have 3 sources (1 from previous test + 2 new)");
      Assert (Cache.Contains (Path2), "Should contain second path");
      Assert (Cache.Contains (Path3), "Should contain third path");
   end Test_Insert_Multiple;
   --  ========================================================================
   --  Test: Get All
   --  ========================================================================
   procedure Test_Get_All is
      All_Sources : constant Source_Map_Type := Cache.Get_All;
   begin
      Put_Line ("Test: Get all sources");
      Assert
        (Natural (All_Sources.Length) = 3,
         "Get_All should return all 3 sources");
   end Test_Get_All;
   --  ========================================================================
   --  Test: Remove
   --  ========================================================================
   procedure Test_Remove is
      Path2 : constant Path_String_Type := Make_Path ("/tmp/tzdata/2023c");
   begin
      Put_Line ("Test: Remove source");
      Cache.Remove (Path2);
      Assert (Cache.Size = 2, "Size should decrease after remove");
      Assert (not Cache.Contains (Path2), "Should not contain removed path");
   end Test_Remove;
   --  ========================================================================
   --  Test: Clear
   --  ========================================================================
   procedure Test_Clear is
   begin
      Put_Line ("Test: Clear cache");
      Cache.Clear;
      Assert (Cache.Is_Empty, "Cache should be empty after clear");
      Assert (Cache.Size = 0, "Size should be 0 after clear");
   end Test_Clear;
begin
   Put_Line ("===================================================");
   Put_Line ("  Running: Source Cache Tests");
   Put_Line ("===================================================");
   New_Line;
   Test_Initial_State;
   Test_Insert_Retrieve;
   Test_Contains_Nonexistent;
   Test_Insert_Multiple;
   Test_Get_All;
   Test_Remove;
   Test_Clear;
   New_Line;
   Put_Line ("Tests Passed: " & Pass_Count'Image & " /" & Test_Count'Image);
   if Pass_Count = Test_Count then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   else
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Test_Source_Cache;