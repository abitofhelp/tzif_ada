pragma Ada_2022;
--  ======================================================================
--  Test_Timezone_Lookup
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Timezone Lookup functionality.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Test_Framework;
with TZif.Domain.Service.Timezone_Lookup;
with TZif.Domain.TZif_Data;
with TZif.Domain.Value_Object.UTC_Offset;
with TZif.Domain.Value_Object.Timezone_Type;
with TZif.Domain.Value_Object.Transition;
procedure Test_Timezone_Lookup is
   use TZif.Domain.TZif_Data;
   use TZif.Domain.Value_Object.UTC_Offset;
   use TZif.Domain.Value_Object.Timezone_Type;
   use TZif.Domain.Value_Object.Transition;

   package Tz_Lookup renames TZif.Domain.Service.Timezone_Lookup;

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

   --  Helper: Check if offset Option equals expected value
   function Offset_Equals
     (Opt : Tz_Lookup.UTC_Offset_Option; Expected : UTC_Offset_Type)
      return Boolean
   is
   begin
      return Tz_Lookup.UTC_Offset_Options.Is_Some (Opt)
        and then Tz_Lookup.UTC_Offset_Options.Value (Opt) = Expected;
   end Offset_Equals;

   --  Helper: Check if DST Option equals expected value
   function DST_Equals
     (Opt : Tz_Lookup.Boolean_Option; Expected : Boolean) return Boolean
   is
   begin
      return Tz_Lookup.Boolean_Options.Is_Some (Opt)
        and then Tz_Lookup.Boolean_Options.Value (Opt) = Expected;
   end DST_Equals;

   --  Helper: Check if abbreviation Option equals expected string
   function Abbrev_Equals
     (Opt : Tz_Lookup.Abbreviation_Option; Expected : String) return Boolean
   is
   begin
      return Tz_Lookup.Abbreviation_Options.Is_Some (Opt)
        and then Abbreviation_Strings.To_String
            (Tz_Lookup.Abbreviation_Options.Value (Opt)) = Expected;
   end Abbrev_Equals;

   --  =====================================================================
   --  Test: No Transitions
   --  =====================================================================
   procedure Test_No_Transitions is
      Data    : TZif_Data_Type;
      TZ_Type : Timezone_Type_Record;
   begin
      Put_Line ("Test: No transitions - use first type");
      TZ_Type := Make_Timezone_Type
          (UTC_Offset => 0, Is_DST => False, Abbreviation => "UTC");
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, TZ_Type);
      Assert
        (Offset_Equals (Tz_Lookup.Find_UTC_Offset_At_Time (Data, 0), 0),
         "No transitions: should return first type offset");
      Assert
        (Offset_Equals (Tz_Lookup.Find_UTC_Offset_At_Time (Data, 1000000), 0),
         "No transitions: should return first type offset for any time");
      Assert
        (DST_Equals (Tz_Lookup.Is_DST_At_Time (Data, 0), False),
         "No transitions: should return first type DST status");
   end Test_No_Transitions;

   --  =====================================================================
   --  Test: Before First Transition
   --  =====================================================================
   procedure Test_Before_First_Transition is
      Data         : TZif_Data_Type;
      Type1, Type2 : Timezone_Type_Record;
      Trans        : Transition_Type;
   begin
      Put_Line ("Test: Time before first transition");
      Type1 := Make_Timezone_Type
          (UTC_Offset => -18000, Is_DST => False, Abbreviation => "EST");
      Type2 := Make_Timezone_Type
          (UTC_Offset => -14400, Is_DST => True, Abbreviation => "EDT");
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, Type1);
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, Type2);
      Trans := (Time => 100, Type_Index => 1);
      Transition_Vectors.Unchecked_Append (Data.Transitions, Trans);
      Assert
        (Offset_Equals (Tz_Lookup.Find_UTC_Offset_At_Time (Data, 50), -14400),
         "Before first transition: should use first transition's type");
      Assert
        (DST_Equals (Tz_Lookup.Is_DST_At_Time (Data, 50), True),
         "Before first transition: should use first transition's DST");
   end Test_Before_First_Transition;

   --  =====================================================================
   --  Test: After Last Transition
   --  =====================================================================
   procedure Test_After_Last_Transition is
      Data           : TZif_Data_Type;
      Type1, Type2   : Timezone_Type_Record;
      Trans1, Trans2 : Transition_Type;
   begin
      Put_Line ("Test: Time after last transition");
      Type1 := Make_Timezone_Type
          (UTC_Offset => -18000, Is_DST => False, Abbreviation => "EST");
      Type2 := Make_Timezone_Type
          (UTC_Offset => -14400, Is_DST => True, Abbreviation => "EDT");
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, Type1);
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, Type2);
      Trans1 := (Time => 100, Type_Index => 1);
      Trans2 := (Time => 200, Type_Index => 0);
      Transition_Vectors.Unchecked_Append (Data.Transitions, Trans1);
      Transition_Vectors.Unchecked_Append (Data.Transitions, Trans2);
      Assert
        (Offset_Equals (Tz_Lookup.Find_UTC_Offset_At_Time (Data, 300), -18000),
         "After last transition: should use EST offset");
      Assert
        (DST_Equals (Tz_Lookup.Is_DST_At_Time (Data, 300), False),
         "After last transition: should not be DST");
   end Test_After_Last_Transition;

   --  =====================================================================
   --  Test: At Transition
   --  =====================================================================
   procedure Test_At_Transition is
      Data         : TZif_Data_Type;
      Type1, Type2 : Timezone_Type_Record;
      Trans        : Transition_Type;
   begin
      Put_Line ("Test: Time exactly at transition");
      Type1 := Make_Timezone_Type (-18000, False, "EST");
      Type2 := Make_Timezone_Type (-14400, True, "EDT");
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, Type1);
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, Type2);
      Trans := (Time => 100, Type_Index => 1);
      Transition_Vectors.Unchecked_Append (Data.Transitions, Trans);
      Assert
        (Offset_Equals (Tz_Lookup.Find_UTC_Offset_At_Time (Data, 100), -14400),
         "At transition: should use new type offset");
      Assert
        (DST_Equals (Tz_Lookup.Is_DST_At_Time (Data, 100), True),
         "At transition: should use new type DST");
   end Test_At_Transition;

   --  =====================================================================
   --  Test: Between Transitions
   --  =====================================================================
   procedure Test_Between_Transitions is
      Data           : TZif_Data_Type;
      Type1, Type2   : Timezone_Type_Record;
      Trans1, Trans2 : Transition_Type;
   begin
      Put_Line ("Test: Time between transitions");
      Type1 := Make_Timezone_Type (-18000, False, "EST");
      Type2 := Make_Timezone_Type (-14400, True, "EDT");
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, Type1);
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, Type2);
      Trans1 := (Time => 100, Type_Index => 1);
      Trans2 := (Time => 200, Type_Index => 0);
      Transition_Vectors.Unchecked_Append (Data.Transitions, Trans1);
      Transition_Vectors.Unchecked_Append (Data.Transitions, Trans2);
      Assert
        (Offset_Equals (Tz_Lookup.Find_UTC_Offset_At_Time (Data, 150), -14400),
         "Between transitions: should use EDT offset");
      Assert
        (DST_Equals (Tz_Lookup.Is_DST_At_Time (Data, 150), True),
         "Between transitions: should be DST");
   end Test_Between_Transitions;

   --  =====================================================================
   --  Test: DST Detection
   --  =====================================================================
   procedure Test_DST_Detection is
      Data         : TZif_Data_Type;
      Type1, Type2 : Timezone_Type_Record;
      Trans        : Transition_Type;
   begin
      Put_Line ("Test: DST detection");
      Type1 := Make_Timezone_Type (-18000, False, "EST");
      Type2 := Make_Timezone_Type (-14400, True, "EDT");
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, Type1);
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, Type2);
      Trans := (Time => 100, Type_Index => 1);
      Transition_Vectors.Unchecked_Append (Data.Transitions, Trans);
      Assert
        (DST_Equals (Tz_Lookup.Is_DST_At_Time (Data, 50), True),
         "Before transition: uses first transition's type (EDT/DST)");
      Assert
        (DST_Equals (Tz_Lookup.Is_DST_At_Time (Data, 150), True),
         "After transition: should be DST (EDT)");
   end Test_DST_Detection;

   --  =====================================================================
   --  Test: Abbreviation Lookup
   --  =====================================================================
   procedure Test_Abbreviation_Lookup is
      Data         : TZif_Data_Type;
      Type1, Type2 : Timezone_Type_Record;
      Trans        : Transition_Type;
   begin
      Put_Line ("Test: Abbreviation lookup");
      Type1 := Make_Timezone_Type (-18000, False, "EST");
      Type2 := Make_Timezone_Type (-14400, True, "EDT");
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, Type1);
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, Type2);
      Trans := (Time => 100, Type_Index => 1);
      Transition_Vectors.Unchecked_Append (Data.Transitions, Trans);
      Assert
        (Abbrev_Equals (Tz_Lookup.Get_Abbreviation_At_Time (Data, 150), "EDT"),
         "Should return correct abbreviation for time");
   end Test_Abbreviation_Lookup;

   --  =====================================================================
   --  Test: Timezone_Type Accessor Functions
   --  =====================================================================
   procedure Test_Timezone_Type_Accessors is
      Type_DST      : Timezone_Type_Record;
      Type_Standard : Timezone_Type_Record;
   begin
      Put_Line ("Test: Timezone_Type accessor functions");
      Type_DST := Make_Timezone_Type (-14400, True, "EDT");
      Type_Standard := Make_Timezone_Type (-18000, False, "EST");
      Assert
        (Is_Daylight_Saving (Type_DST),
         "DST type should report Is_Daylight_Saving");
      Assert
        (not Is_Daylight_Saving (Type_Standard),
         "Standard type should not report Is_Daylight_Saving");
      Assert
        (not Is_Standard_Time (Type_DST),
         "DST type should not report Is_Standard_Time");
      Assert
        (Is_Standard_Time (Type_Standard),
         "Standard type should report Is_Standard_Time");
      Assert
        (Has_Abbreviation (Type_DST, "EDT"), "Should match abbreviation EDT");
      Assert
        (not Has_Abbreviation (Type_DST, "EST"),
         "Should not match abbreviation EST");
      Assert
        (Has_Abbreviation (Type_Standard, "EST"),
         "Should match abbreviation EST");
   end Test_Timezone_Type_Accessors;

   --  =====================================================================
   --  Test: Error Paths - No Types Available
   --  =====================================================================
   procedure Test_Error_No_Types is
      Data  : TZif_Data_Type;
      Trans : Transition_Type;
   begin
      Put_Line ("Test: Error paths - no types available");
      --  Add a transition but no types (invalid state but test error handling)
      Trans := (Time => 100, Type_Index => 0);
      Transition_Vectors.Unchecked_Append (Data.Transitions, Trans);
      --  Should return None (no fallback values)
      Assert
        (Tz_Lookup.UTC_Offset_Options.Is_None
           (Tz_Lookup.Find_UTC_Offset_At_Time (Data, 150)),
         "No types: should return None for offset");
      Assert
        (Tz_Lookup.Boolean_Options.Is_None
           (Tz_Lookup.Is_DST_At_Time (Data, 150)),
         "No types: should return None for DST");
      Assert
        (Tz_Lookup.Abbreviation_Options.Is_None
           (Tz_Lookup.Get_Abbreviation_At_Time (Data, 150)),
         "No types: should return None for abbreviation");
   end Test_Error_No_Types;

   --  =====================================================================
   --  Test: Error Paths - Invalid Type Index
   --  =====================================================================
   procedure Test_Error_Invalid_Type_Index is
      Data    : TZif_Data_Type;
      TZ_Type : Timezone_Type_Record;
      Trans   : Transition_Type;
   begin
      Put_Line ("Test: Error paths - invalid type index");
      --  Add one type but transition points to invalid index
      TZ_Type := Make_Timezone_Type (-18000, False, "EST");
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, TZ_Type);
      Trans := (Time => 100, Type_Index => 99);  --  Invalid index
      Transition_Vectors.Unchecked_Append (Data.Transitions, Trans);
      --  Should return None (no silent fallback)
      Assert
        (Tz_Lookup.UTC_Offset_Options.Is_None
           (Tz_Lookup.Find_UTC_Offset_At_Time (Data, 150)),
         "Invalid type index: should return None for offset");
      Assert
        (Tz_Lookup.Boolean_Options.Is_None
           (Tz_Lookup.Is_DST_At_Time (Data, 150)),
         "Invalid type index: should return None for DST");
      Assert
        (Tz_Lookup.Abbreviation_Options.Is_None
           (Tz_Lookup.Get_Abbreviation_At_Time (Data, 150)),
         "Invalid type index: should return None for abbreviation");
   end Test_Error_Invalid_Type_Index;

   --  =====================================================================
   --  Test: Binary Search Edge Case - Mid Before First
   --  =====================================================================
   procedure Test_Binary_Search_Edge is
      Data   : TZif_Data_Type;
      Type1  : Timezone_Type_Record;
      Trans1 : Transition_Type;
   begin
      Put_Line ("Test: Binary search edge case");
      Type1 := Make_Timezone_Type (-18000, False, "EST");
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, Type1);
      Trans1 := (Time => 100, Type_Index => 0);
      Transition_Vectors.Unchecked_Append (Data.Transitions, Trans1);
      --  Query time way before first transition
      Assert
        (Offset_Equals (Tz_Lookup.Find_UTC_Offset_At_Time (Data, 1), -18000),
         "Before first transition: should use first transition's type");
   end Test_Binary_Search_Edge;

begin
   --  Run all tests
   Test_No_Transitions;
   Test_Before_First_Transition;
   Test_After_Last_Transition;
   Test_At_Transition;
   Test_Between_Transitions;
   Test_DST_Detection;
   Test_Abbreviation_Lookup;
   Test_Timezone_Type_Accessors;
   Test_Error_No_Types;
   Test_Error_Invalid_Type_Index;
   Test_Binary_Search_Edge;
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
   --  Register results with test framework
   Test_Framework.Register_Results (Test_Count, Pass_Count);
   if Pass_Count /= Test_Count then
      Ada.Command_Line.Set_Exit_Status (1);
   end if;
end Test_Timezone_Lookup;
