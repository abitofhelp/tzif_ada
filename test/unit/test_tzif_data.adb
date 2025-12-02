pragma Ada_2022;
--  ======================================================================
--  Test_Tzif_Data
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Tzif Data functionality.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Test_Framework;
with TZif.Domain.TZif_Data;
with TZif.Domain.Value_Object.UTC_Offset;
with TZif.Domain.Value_Object.Timezone_Type;
with TZif.Domain.Value_Object.Transition;
procedure Test_TZif_Data is
   use TZif.Domain.TZif_Data;
   use TZif.Domain.Value_Object.UTC_Offset;
   use TZif.Domain.Value_Object.Timezone_Type;
   use TZif.Domain.Value_Object.Transition;

   --  Suppress "aggregate not fully initialized" for test data with defaults
   pragma Warnings (Off, "*aggregate*not fully initialized*");

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
     (Opt : UTC_Offset_Option; Expected : UTC_Offset_Type) return Boolean
   is
   begin
      return UTC_Offset_Options.Is_Some (Opt)
        and then UTC_Offset_Options.Value (Opt) = Expected;
   end Offset_Equals;

   --  Helper: Check if DST Option equals expected value
   function DST_Equals
     (Opt : Boolean_Option; Expected : Boolean) return Boolean
   is
   begin
      return Boolean_Options.Is_Some (Opt)
        and then Boolean_Options.Value (Opt) = Expected;
   end DST_Equals;

   --  Helper: Check if abbreviation Option equals expected string
   function Abbrev_Equals
     (Opt : Abbreviation_Option; Expected : String) return Boolean
   is
   begin
      return Abbreviation_Options.Is_Some (Opt)
        and then Abbreviation_Strings.To_String
            (Abbreviation_Options.Value (Opt)) = Expected;
   end Abbrev_Equals;

   --  =====================================================================
   --  Test: No Transitions
   --  =====================================================================
   procedure Test_Find_Type_No_Transitions is
      Data       : TZif_Data_Type;
      TZ_Type    : Timezone_Type_Record;
      Type_Index : Natural;
   begin
      Put_Line ("Test: Find type with no transitions");
      TZ_Type := Make_Timezone_Type
          (UTC_Offset => 0, Is_DST => False, Abbreviation => "UTC");
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, TZ_Type);
      Type_Index := Find_Type_At_Time (Data, 0);
      Assert
        (Type_Index = 0, "No transitions: should return first type index");
      Type_Index := Find_Type_At_Time (Data, 1000000);
      Assert
        (Type_Index = 0,
         "No transitions: should return first type for any time");
   end Test_Find_Type_No_Transitions;

   --  =====================================================================
   --  Test: Before First Transition
   --  =====================================================================
   procedure Test_Find_Type_Before_First is
      Data         : TZif_Data_Type;
      Type1, Type2 : Timezone_Type_Record;
      Trans        : Transition_Type;
      Type_Index   : Natural;
   begin
      Put_Line ("Test: Find type before first transition");
      Type1 := Make_Timezone_Type
          (UTC_Offset => -18000, Is_DST => False, Abbreviation => "EST");
      Type2 := Make_Timezone_Type
          (UTC_Offset => -14400, Is_DST => True, Abbreviation => "EDT");
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, Type1);
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, Type2);
      Trans := (Time => 100, Type_Index => 1);
      Transition_Vectors.Unchecked_Append (Data.Transitions, Trans);
      Type_Index := Find_Type_At_Time (Data, 50);
      Assert
        (Type_Index = 1, "Before first transition: use first trans type");
   end Test_Find_Type_Before_First;

   --  =====================================================================
   --  Test: After Last Transition
   --  =====================================================================
   procedure Test_Find_Type_After_Last is
      Data           : TZif_Data_Type;
      Type1, Type2   : Timezone_Type_Record;
      Trans1, Trans2 : Transition_Type;
      Type_Index     : Natural;
   begin
      Put_Line ("Test: Find type after last transition");
      Type1 := Make_Timezone_Type (-18000, False, "EST");
      Type2 := Make_Timezone_Type (-14400, True, "EDT");
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, Type1);
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, Type2);
      Trans1 := (Time => 100, Type_Index => 1);
      Trans2 := (Time => 200, Type_Index => 0);
      Transition_Vectors.Unchecked_Append (Data.Transitions, Trans1);
      Transition_Vectors.Unchecked_Append (Data.Transitions, Trans2);
      Type_Index := Find_Type_At_Time (Data, 300);
      Assert
        (Type_Index = 0,
         "After last transition: should use last transition's type");
   end Test_Find_Type_After_Last;

   --  =====================================================================
   --  Test: Between Transitions
   --  =====================================================================
   procedure Test_Find_Type_Between is
      Data           : TZif_Data_Type;
      Type1, Type2   : Timezone_Type_Record;
      Trans1, Trans2 : Transition_Type;
      Type_Index     : Natural;
   begin
      Put_Line ("Test: Find type between transitions");
      Type1 := Make_Timezone_Type (-18000, False, "EST");
      Type2 := Make_Timezone_Type (-14400, True, "EDT");
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, Type1);
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, Type2);
      Trans1 := (Time => 100, Type_Index => 1);
      Trans2 := (Time => 200, Type_Index => 0);
      Transition_Vectors.Unchecked_Append (Data.Transitions, Trans1);
      Transition_Vectors.Unchecked_Append (Data.Transitions, Trans2);
      Type_Index := Find_Type_At_Time (Data, 150);
      Assert (Type_Index = 1, "Between: should use correct type");
   end Test_Find_Type_Between;

   --  =====================================================================
   --  Test: Find Offset At Time
   --  =====================================================================
   procedure Test_Find_Offset_At_Time is
      Data         : TZif_Data_Type;
      Type1, Type2 : Timezone_Type_Record;
      Trans        : Transition_Type;
   begin
      Put_Line ("Test: Find offset at specific time");
      Type1 := Make_Timezone_Type (-18000, False, "EST");
      Type2 := Make_Timezone_Type (-14400, True, "EDT");
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, Type1);
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, Type2);
      Trans := (Time => 100, Type_Index => 1);
      Transition_Vectors.Unchecked_Append (Data.Transitions, Trans);
      Assert
        (Offset_Equals (Find_Offset_At_Time (Data, 150), -14400),
         "Should return correct offset");
   end Test_Find_Offset_At_Time;

   --  =====================================================================
   --  Test: DST Status At Time
   --  =====================================================================
   procedure Test_Is_DST_At_Time is
      Data         : TZif_Data_Type;
      Type1, Type2 : Timezone_Type_Record;
      Trans        : Transition_Type;
   begin
      Put_Line ("Test: Check DST status at time");
      Type1 := Make_Timezone_Type (-18000, False, "EST");
      Type2 := Make_Timezone_Type (-14400, True, "EDT");
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, Type1);
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, Type2);
      Trans := (Time => 100, Type_Index => 1);
      Transition_Vectors.Unchecked_Append (Data.Transitions, Trans);
      --  Before first transition: uses first transition's type (EDT/DST)
      Assert
        (DST_Equals (Is_DST_At_Time (Data, 50), True),
         "Before transition: should be DST (uses first transition's type)");
      Assert
        (DST_Equals (Is_DST_At_Time (Data, 150), True),
         "After transition: should be DST");
   end Test_Is_DST_At_Time;

   --  =====================================================================
   --  Test: Has_Transitions Query
   --  =====================================================================
   procedure Test_Has_Transitions is
      Data  : TZif_Data_Type := (others => <>);
      Trans : Transition_Type;
   begin
      Put_Line ("Test: Has_Transitions query");
      Assert
        (not Has_Transitions (Data), "Empty data should have no transitions");
      Trans := (Time => 100, Type_Index => 0);
      Transition_Vectors.Unchecked_Append (Data.Transitions, Trans);
      Assert
        (Has_Transitions (Data), "Data with transition should report true");
   end Test_Has_Transitions;

   --  =====================================================================
   --  Test: Leap Seconds Queries
   --  =====================================================================
   procedure Test_Leap_Second_Queries is
      Data : TZif_Data_Type := (others => <>);
      Leap : Leap_Second_Type;
   begin
      Put_Line ("Test: Leap second queries");
      Assert
        (not Has_Leap_Seconds (Data),
         "Empty data should have no leap seconds");
      Assert
        (Leap_Second_Count (Data) = 0,
         "Empty data should have leap count of 0");
      Leap := (Occurrence_Time => 1000, Leap_Count => 1);
      Leap_Second_Vectors.Unchecked_Append (Data.Leap_Seconds, Leap);
      Assert
        (Has_Leap_Seconds (Data), "Data with leap second should report true");
      Assert
        (Leap_Second_Count (Data) = 1,
         "Should report correct leap second count");
   end Test_Leap_Second_Queries;

   --  =====================================================================
   --  Test: POSIX TZ String Queries
   --  =====================================================================
   procedure Test_POSIX_TZ_Queries is
      Data : TZif_Data_Type := (others => <>);
   begin
      Put_Line ("Test: POSIX TZ string queries");
      Assert
        (not Has_POSIX_TZ (Data), "Empty data should have no POSIX TZ string");
      Data.POSIX_TZ := POSIX_TZ_Strings.To_Bounded_String ("EST5EDT");
      Assert (Has_POSIX_TZ (Data), "Data with POSIX TZ should report true");
      Assert
        (Get_POSIX_TZ (Data) = "EST5EDT",
         "Should return correct POSIX TZ string");
   end Test_POSIX_TZ_Queries;

   --  =====================================================================
   --  Test: Abbreviation At Time
   --  =====================================================================
   procedure Test_Get_Abbreviation_At_Time is
      Data         : TZif_Data_Type;
      Type1, Type2 : Timezone_Type_Record;
      Trans        : Transition_Type;
   begin
      Put_Line ("Test: Get abbreviation at specific time");
      Type1 := Make_Timezone_Type (-18000, False, "EST");
      Type2 := Make_Timezone_Type (-14400, True, "EDT");
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, Type1);
      Timezone_Type_Vectors.Unchecked_Append (Data.Timezone_Types, Type2);
      Trans := (Time => 100, Type_Index => 1);
      Transition_Vectors.Unchecked_Append (Data.Transitions, Trans);
      --  Before first transition: uses first transition's type (EDT)
      Assert
        (Abbrev_Equals (Get_Abbreviation_At_Time (Data, 50), "EDT"),
         "Before transition: should return EDT (first transition's type)");
      Assert
        (Abbrev_Equals (Get_Abbreviation_At_Time (Data, 150), "EDT"),
         "After transition: should return EDT");
   end Test_Get_Abbreviation_At_Time;

begin
   --  Run all tests
   Test_Find_Type_No_Transitions;
   Test_Find_Type_Before_First;
   Test_Find_Type_After_Last;
   Test_Find_Type_Between;
   Test_Find_Offset_At_Time;
   Test_Is_DST_At_Time;
   Test_Has_Transitions;
   Test_Leap_Second_Queries;
   Test_POSIX_TZ_Queries;
   Test_Get_Abbreviation_At_Time;
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
end Test_TZif_Data;
