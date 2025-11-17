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
      Data.Timezone_Types.Append (TZ_Type);
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
      Data.Timezone_Types.Append (Type1);
      Data.Timezone_Types.Append (Type2);
      Trans := (Time => 100, Type_Index => 1);
      Data.Transitions.Append (Trans);
      Type_Index := Find_Type_At_Time (Data, 50);
      Assert
        (Type_Index = 0, "Before first transition: should use first type");
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
      Data.Timezone_Types.Append (Type1);
      Data.Timezone_Types.Append (Type2);
      Trans1 := (Time => 100, Type_Index => 1);
      Trans2 := (Time => 200, Type_Index => 0);
      Data.Transitions.Append (Trans1);
      Data.Transitions.Append (Trans2);
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
      Data.Timezone_Types.Append (Type1);
      Data.Timezone_Types.Append (Type2);
      Trans1 := (Time => 100, Type_Index => 1);
      Trans2 := (Time => 200, Type_Index => 0);
      Data.Transitions.Append (Trans1);
      Data.Transitions.Append (Trans2);
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
      Offset       : UTC_Offset_Type;
   begin
      Put_Line ("Test: Find offset at specific time");
      Type1 := Make_Timezone_Type (-18000, False, "EST");
      Type2 := Make_Timezone_Type (-14400, True, "EDT");
      Data.Timezone_Types.Append (Type1);
      Data.Timezone_Types.Append (Type2);
      Trans := (Time => 100, Type_Index => 1);
      Data.Transitions.Append (Trans);
      Offset := Find_Offset_At_Time (Data, 150);
      Assert (Offset = -14400, "Should return correct offset");
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
      Data.Timezone_Types.Append (Type1);
      Data.Timezone_Types.Append (Type2);
      Trans := (Time => 100, Type_Index => 1);
      Data.Transitions.Append (Trans);
      Assert
        (not Is_DST_At_Time (Data, 50),
         "Before transition: should not be DST (uses first type)");
      Assert (Is_DST_At_Time (Data, 150), "After transition: should be DST");
   end Test_Is_DST_At_Time;
   --  =====================================================================
   --  Test: Has_Transitions Query
   --  =====================================================================
   procedure Test_Has_Transitions is
      Data  : TZif_Data_Type;
      Trans : Transition_Type;
   begin
      Put_Line ("Test: Has_Transitions query");
      Assert
        (not Has_Transitions (Data), "Empty data should have no transitions");
      Trans := (Time => 100, Type_Index => 0);
      Data.Transitions.Append (Trans);
      Assert
        (Has_Transitions (Data), "Data with transition should report true");
   end Test_Has_Transitions;
   --  =====================================================================
   --  Test: Leap Seconds Queries
   --  =====================================================================
   procedure Test_Leap_Second_Queries is
      Data : TZif_Data_Type;
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
      Data.Leap_Seconds.Append (Leap);
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
      Data : TZif_Data_Type;
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
      Abbrev       : String (1 .. 3);
   begin
      Put_Line ("Test: Get abbreviation at specific time");
      Type1 := Make_Timezone_Type (-18000, False, "EST");
      Type2 := Make_Timezone_Type (-14400, True, "EDT");
      Data.Timezone_Types.Append (Type1);
      Data.Timezone_Types.Append (Type2);
      Trans := (Time => 100, Type_Index => 1);
      Data.Transitions.Append (Trans);
      Abbrev := Get_Abbreviation_At_Time (Data, 50);
      Assert (Abbrev = "EST", "Before transition: should return EST");
      Abbrev := Get_Abbreviation_At_Time (Data, 150);
      Assert (Abbrev = "EDT", "After transition: should return EDT");
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