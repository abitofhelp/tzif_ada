pragma Ada_2022;
--  ======================================================================
--  Test_Zone_Entity
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Zone Entity functionality.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with TZif.Domain.Entity.Zone;
with TZif.Domain.TZif_Data;
with TZif.Domain.Value_Object.Zone_Id;
with TZif.Domain.Value_Object.Transition;
with TZif.Domain.Value_Object.Timezone_Type;
with TZif.Domain.Value_Object.UTC_Offset;
procedure Test_Zone_Entity is
   use TZif.Domain.Entity.Zone;
   use TZif.Domain.TZif_Data;
   use TZif.Domain.Value_Object.Zone_Id;
   use TZif.Domain.Value_Object.Transition;
   use TZif.Domain.Value_Object.Timezone_Type;
   use TZif.Domain.Value_Object.UTC_Offset;

   --  Suppress test-specific warnings for TZif_Data with default components
   pragma Warnings (Off, "*aggregate*not fully initialized*");
   pragma Warnings (Off, "*could be declared constant*");

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
   --  Test: Make_Zone with Zone_Id_Type
   --  ========================================================================
   procedure Test_Make_Zone_With_Zone_Id is
      Zone_ID : constant Zone_Id_Type := Make_Zone_Id ("America/Los_Angeles");
      Data    : TZif_Data_Type        := (others => <>);
      Zone    : constant Zone_Type    := Make_Zone (Zone_ID, Data);
   begin
      Put_Line ("Test: Make_Zone with Zone_Id_Type");
      Assert (Get_Id (Zone) = Zone_ID, "Zone should have correct ID");
      Assert
        (Get_Id_String (Zone) = "America/Los_Angeles",
         "Zone should return correct ID string");
   end Test_Make_Zone_With_Zone_Id;
   --  ========================================================================
   --  Test: Make_Zone with String
   --  ========================================================================
   procedure Test_Make_Zone_With_String is
      Data : TZif_Data_Type     := (others => <>);
      Zone : constant Zone_Type := Make_Zone ("UTC", Data);
   begin
      Put_Line ("Test: Make_Zone with String ID");
      Assert
        (Get_Id_String (Zone) = "UTC",
         "Zone should have correct ID from string");
   end Test_Make_Zone_With_String;
   --  ========================================================================
   --  Test: Get_Data
   --  ========================================================================
   procedure Test_Get_Data is
      Data    : TZif_Data_Type := (others => <>);
      TZ_Type : Timezone_Type_Record;
      Zone    : Zone_Type;
   begin
      Put_Line ("Test: Get_Data accessor");
      --  Build TZif data with a timezone type
      TZ_Type := Make_Timezone_Type
          (UTC_Offset => 0, Is_DST => False, Abbreviation => "UTC");
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, TZ_Type);
      Zone := Make_Zone ("UTC", Data);
      declare
         Retrieved_Data : constant TZif_Data_Type := Get_Data (Zone);
      begin
         Assert
           (Timezone_Type_Vectors.Length (Retrieved_Data.Timezone_Types) = 1,
            "Retrieved data should have 1 timezone type");
      end;
   end Test_Get_Data;
   --  ========================================================================
   --  Test: Has_Transitions (Empty)
   --  ========================================================================
   procedure Test_Has_Transitions_Empty is
      Data : TZif_Data_Type := (others => <>);  --  Empty, no transitions
      Zone : constant Zone_Type := Make_Zone ("UTC", Data);
   begin
      Put_Line ("Test: Has_Transitions with no transitions");
      Assert
        (not Has_Transitions (Zone),
         "Zone with no transitions should return False");
   end Test_Has_Transitions_Empty;
   --  ========================================================================
   --  Test: Has_Transitions (With Data)
   --  ========================================================================
   procedure Test_Has_Transitions_With_Data is
      Data  : TZif_Data_Type := (others => <>);
      Trans : Transition_Type;
      Zone  : Zone_Type;
   begin
      Put_Line ("Test: Has_Transitions with transitions");
      --  Add a transition
      Trans := (Time => 0, Type_Index => 0);
      Transition_Vectors.Unchecked_Append (Data.Transitions, Trans);
      Zone := Make_Zone ("America/New_York", Data);
      Assert
        (Has_Transitions (Zone), "Zone with transitions should return True");
   end Test_Has_Transitions_With_Data;
   --  ========================================================================
   --  Test: Transition_Count
   --  ========================================================================
   procedure Test_Transition_Count is
      Data : TZif_Data_Type := (others => <>);
      Zone : Zone_Type;
   begin
      Put_Line ("Test: Transition_Count");
      --  Add 3 transitions
      Transition_Vectors.Unchecked_Append
        (Data.Transitions, Transition_Type'(Time => 0, Type_Index => 0));
      Transition_Vectors.Unchecked_Append
        (Data.Transitions, Transition_Type'(Time => 1000, Type_Index => 1));
      Transition_Vectors.Unchecked_Append
        (Data.Transitions, Transition_Type'(Time => 2000, Type_Index => 0));
      Zone := Make_Zone ("America/New_York", Data);
      Assert (Transition_Count (Zone) = 3, "Zone should have 3 transitions");
   end Test_Transition_Count;
   --  ========================================================================
   --  Test: Entity Equality (Same ID)
   --  ========================================================================
   procedure Test_Entity_Equality_Same_Id is
      Data1        : TZif_Data_Type := (others => <>);
      Data2        : TZif_Data_Type := (others => <>);
      Zone1, Zone2 : Zone_Type;
   begin
      Put_Line ("Test: Entity equality with same ID");
      --  Create two zones with same ID but different data
      --  Data2 has transition
      Transition_Vectors.Unchecked_Append
        (Data2.Transitions, Transition_Type'(Time => 0, Type_Index => 0));
      Zone1 := Make_Zone ("UTC", Data1);
      Zone2 := Make_Zone ("UTC", Data2);
      Assert
        (Zone1 = Zone2,
         "Zones with same ID should be equal (entity identity)");
   end Test_Entity_Equality_Same_Id;
   --  ========================================================================
   --  Test: Entity Equality (Different ID)
   --  ========================================================================
   procedure Test_Entity_Equality_Different_Id is
      Data         : TZif_Data_Type := (others => <>);
      Zone1, Zone2 : Zone_Type;
   begin
      Put_Line ("Test: Entity equality with different ID");
      Zone1 := Make_Zone ("UTC", Data);
      Zone2 := Make_Zone ("America/Los_Angeles", Data);
      Assert (Zone1 /= Zone2, "Zones with different IDs should NOT be equal");
   end Test_Entity_Equality_Different_Id;
   --  ========================================================================
   --  Test: Has_Id with Zone_Id_Type
   --  ========================================================================
   procedure Test_Has_Id_With_Zone_Id is
      Data : TZif_Data_Type     := (others => <>);
      Zone : constant Zone_Type := Make_Zone ("America/New_York", Data);
      ID   : constant Zone_Id_Type := Make_Zone_Id ("America/New_York");
   begin
      Put_Line ("Test: Has_Id with Zone_Id_Type");
      Assert (Has_Id (Zone, ID), "Zone should match its own ID");
   end Test_Has_Id_With_Zone_Id;
   --  ========================================================================
   --  Test: Has_Id with String
   --  ========================================================================
   procedure Test_Has_Id_With_String is
      Data : TZif_Data_Type     := (others => <>);
      Zone : constant Zone_Type := Make_Zone ("America/New_York", Data);
   begin
      Put_Line ("Test: Has_Id with String");
      Assert
        (Has_Id (Zone, "America/New_York"),
         "Zone should match its own ID string");
      Assert
        (not Has_Id (Zone, "UTC"),
         "Zone should NOT match different ID string");
   end Test_Has_Id_With_String;
begin
   Put_Line ("====================================================");
   Put_Line ("  Running: Zone Entity Tests");
   Put_Line ("====================================================");
   New_Line;
   Test_Make_Zone_With_Zone_Id;
   New_Line;
   Test_Make_Zone_With_String;
   New_Line;
   Test_Get_Data;
   New_Line;
   Test_Has_Transitions_Empty;
   New_Line;
   Test_Has_Transitions_With_Data;
   New_Line;
   Test_Transition_Count;
   New_Line;
   Test_Entity_Equality_Same_Id;
   New_Line;
   Test_Entity_Equality_Different_Id;
   New_Line;
   Test_Has_Id_With_Zone_Id;
   New_Line;
   Test_Has_Id_With_String;
   New_Line;
   --  Summary
   Put_Line ("====================================================");
   Put_Line ("  Tests Passed: " & Pass_Count'Image & " /" & Test_Count'Image);
   Put_Line ("====================================================");
   if Pass_Count = Test_Count then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   else
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Test_Zone_Entity;