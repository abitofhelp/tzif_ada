pragma Ada_2022;
--  ======================================================================
--  Test_Value_Object_Accessors
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Value Object Accessors functionality.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Test_Framework;
with TZif.Domain.Value_Object.Source_Info;
with TZif.Domain.Value_Object.TZif_Header;
with TZif.Domain.Value_Object.Transition_Info;
with TZif.Domain.Value_Object.Transition;
with TZif.Domain.Value_Object.UTC_Offset;
with TZif.Domain.Value_Object.Epoch_Seconds;
procedure Test_Value_Object_Accessors is
   use TZif.Domain.Value_Object.Source_Info;
   use TZif.Domain.Value_Object.TZif_Header;
   use TZif.Domain.Value_Object.Transition_Info;
   use TZif.Domain.Value_Object.UTC_Offset;
   use TZif.Domain.Value_Object.Epoch_Seconds;
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

   --  Helper to check value equality without triggering always-true warnings
   function Equals (A, B : Epoch_Seconds_Type) return Boolean is (A = B);

   --  =====================================================================
   --  Test: Source_Info Accessors
   --  =====================================================================
   procedure Test_Source_Info_Accessors is
      ULID_1    : constant ULID_Type :=
                    Make_ULID ("01HQZX3J7K9M2N4P5Q6R7S8T9V");
      ULID_2    : constant ULID_Type :=
                    Make_ULID ("01HQZX3J7K9M2N4P5Q6R7S8T9W");
      Path_1    : constant Path_String_Type :=
                    Make_Path ("/usr/share/zoneinfo");
      Version_1 : constant Version_String_Type := Make_Version ("2024b");
      Source_1  : constant Source_Info_Type :=
                    Make_Source_Info (ULID_1, Path_1, Version_1, 600);
      Source_2  : constant Source_Info_Type :=
                    Make_Source_Info (ULID_2, Path_1, Version_1, 600);
      Source_3  : constant Source_Info_Type :=
                    Make_Source_Info (ULID_1, Path_1, Version_1, 600);
   begin
      Put_Line ("Test: Source_Info accessor functions");
      --  Test accessors (using To_String for comparisons)
      Assert
        (To_String (Get_ULID (Source_1)) = To_String (ULID_1),
         "Get_ULID returns correct ULID");
      Assert
        (To_String (Get_Path (Source_1)) = To_String (Path_1),
         "Get_Path returns correct path");
      Assert
        (To_String (Get_Version (Source_1)) = To_String (Version_1),
         "Get_Version returns correct version");
      Assert
        (Get_Zone_Count (Source_1) = 600,
         "Get_Zone_Count returns correct count");
      --  Test ULID to_string
      Assert
        (To_String (ULID_1) = "01HQZX3J7K9M2N4P5Q6R7S8T9V",
         "ULID To_String works");
      --  Test Version to_string
      Assert (To_String (Version_1) = "2024b", "Version To_String");
      --  Test equality (by ULID)
      Assert (Source_1 = Source_3, "Equal ULIDs are equal");
      Assert (not (Source_1 = Source_2), "Different ULIDs are not equal");
      --  Test less-than comparison (by ULID)
      Assert (Source_1 < Source_2, "ULID_1 < ULID_2");
      Assert (not (Source_2 < Source_1), "Not ULID_2 < ULID_1");
   end Test_Source_Info_Accessors;

   --  =====================================================================
   --  Test: TZif_Header Accessors
   --  =====================================================================
   procedure Test_TZif_Header_Accessors is
      Header_V1             : TZif_Header_Type;
      Header_V2             : TZif_Header_Type;
      Header_V3             : TZif_Header_Type;
      Header_With_Leaps     : TZif_Header_Type;
      Header_No_Transitions : TZif_Header_Type;
   begin
      Put_Line ("Test: TZif_Header accessor functions");
      --  Setup version 1 header
      Header_V1.Version := Version_1;
      Header_V1.Leap_Count := 0;
      Header_V1.Transition_Count := 10;
      --  Setup version 2 header
      Header_V2.Version := Version_2;
      Header_V2.Leap_Count := 0;
      Header_V2.Transition_Count := 10;
      --  Setup version 3 header
      Header_V3.Version := Version_3;
      Header_V3.Leap_Count := 0;
      Header_V3.Transition_Count := 10;
      --  Setup header with leap seconds
      Header_With_Leaps.Version := Version_2;
      Header_With_Leaps.Leap_Count := 5;
      Header_With_Leaps.Transition_Count := 10;
      --  Setup header without transitions
      Header_No_Transitions.Version := Version_2;
      Header_No_Transitions.Leap_Count := 0;
      Header_No_Transitions.Transition_Count := 0;
      --  Test version queries
      Assert (Is_Version_1 (Header_V1), "Version 1 is detected");
      Assert (not Is_Version_1 (Header_V2), "Version 2 is not Version 1");
      Assert (Is_Version_2_Or_Later (Header_V2), "Version 2 is 2 or later");
      Assert (Is_Version_2_Or_Later (Header_V3), "Version 3 is 2 or later");
      Assert
        (not Is_Version_2_Or_Later (Header_V1), "Version 1 is not 2 or later");
      --  Test leap seconds query
      Assert
        (Has_Leap_Seconds (Header_With_Leaps),
         "Header with leap seconds detected");
      Assert
        (not Has_Leap_Seconds (Header_V1),
         "Header without leap seconds detected");
      --  Test transitions query
      Assert (Has_Transitions (Header_V1), "Header with transitions detected");
      Assert
        (not Has_Transitions (Header_No_Transitions),
         "Header without transitions detected");
   end Test_TZif_Header_Accessors;

   --  =====================================================================
   --  Test: Transition_Info Accessors
   --  =====================================================================
   procedure Test_Transition_Info_Accessors is
      Info_DST : constant Transition_Info_Type :=
                   Make_Transition_Info (1000, -14400, True, "EDT");
      Info_Std : constant Transition_Info_Type :=
                   Make_Transition_Info (2000, -18000, False, "EST");
   begin
      Put_Line ("Test: Transition_Info accessor functions");
      --  Test Is_Standard_Time
      Assert (Is_Standard_Time (Info_Std), "Standard time detected");
      Assert (not Is_Standard_Time (Info_DST), "DST time is not standard");
      --  Test Get_Epoch_Time
      Assert (Get_Epoch_Time (Info_DST) = 1000, "DST epoch time correct");
      Assert (Get_Epoch_Time (Info_Std) = 2000, "Standard epoch time correct");
   end Test_Transition_Info_Accessors;

   --  =====================================================================
   --  Test: UTC_Offset Accessors
   --  =====================================================================
   procedure Test_UTC_Offset_Accessors is
      Offset_Pos : constant UTC_Offset_Type := From_HMS (5, 30, 0);
      Offset_Neg : constant UTC_Offset_Type := From_HMS (-8, 0, 0);
      Offset_UTC : constant UTC_Offset_Type := UTC;
   begin
      Put_Line ("Test: UTC_Offset accessor functions");

      --  Test Is_Ahead_Of_UTC
      Assert (Is_Ahead_Of_UTC (Offset_Pos), "Positive offset is ahead");
      Assert (not Is_Ahead_Of_UTC (Offset_Neg), "Negative offset not ahead");
      Assert (not Is_Ahead_Of_UTC (Offset_UTC), "UTC is not ahead");

      --  Test Is_Behind_UTC
      Assert (Is_Behind_UTC (Offset_Neg), "Negative offset is behind");
      Assert (not Is_Behind_UTC (Offset_Pos), "Positive offset not behind");
      Assert (not Is_Behind_UTC (Offset_UTC), "UTC is not behind");

      --  Test Abs_Value
      Assert (Abs_Value (Offset_Neg) > 0, "Abs_Value of negative is positive");
      Assert
        (Abs_Value (Offset_Neg) = Abs_Value (-Offset_Neg),
         "Abs_Value symmetric");

      --  Test To_Hours
      Assert (To_Hours (Offset_Pos) = 5, "5h30m offset has 5 hours");
      Assert (To_Hours (Offset_Neg) = -8, "-8h offset has -8 hours");
      Assert (To_Hours (Offset_UTC) = 0, "UTC has 0 hours");

      --  Test To_Minutes_Part
      Assert (To_Minutes_Part (Offset_Pos) = 30, "5h30m has 30 min part");
      Assert (To_Minutes_Part (Offset_Neg) = 0, "-8h has 0 min part");

      --  Test To_Seconds_Part
      declare
         Offset_Secs : constant UTC_Offset_Type := From_HMS (1, 2, 45);
      begin
         Assert
           (To_Seconds_Part (Offset_Secs) = 45, "1h2m45s has 45 sec part");
      end;

      --  Test From_HMS round-trip
      Assert
        (From_HMS (0, 0, 0) = UTC,
         "From_HMS(0,0,0) equals UTC");
      Assert
        (From_HMS (1, 0, 0) = One_Hour,
         "From_HMS(1,0,0) equals One_Hour");
      Assert
        (From_HMS (-12, 0, 0) = Typical_Min_Offset,
         "From_HMS(-12,0,0) equals Typical_Min_Offset");
   end Test_UTC_Offset_Accessors;

   --  =====================================================================
   --  Test: Epoch_Seconds Accessors
   --  =====================================================================
   procedure Test_Epoch_Seconds_Accessors is
      Time_2000    : constant Epoch_Seconds_Type := 946_684_800;
      Time_32_Min  : constant Epoch_Seconds_Type := Epoch_Seconds_32_Min;
      Time_32_Max  : constant Epoch_Seconds_Type := Epoch_Seconds_32_Max;
      Time_64_Only : constant Epoch_Seconds_Type := Epoch_Seconds_32_Max + 1;
   begin
      Put_Line ("Test: Epoch_Seconds accessor functions");

      --  Test Epoch_Zero constant (using helper to avoid always-true warning)
      Assert (Equals (Epoch_Zero, 0), "Epoch_Zero is 0");

      --  Test Fits_In_32_Bit
      Assert (Fits_In_32_Bit (Time_2000), "Year 2000 fits in 32-bit");
      Assert (Fits_In_32_Bit (Time_32_Min), "32-bit min fits in 32-bit");
      Assert (Fits_In_32_Bit (Time_32_Max), "32-bit max fits in 32-bit");
      Assert (not Fits_In_32_Bit (Time_64_Only), "64-bit only does not fit");
      Assert (Fits_In_32_Bit (Epoch_Zero), "Epoch zero fits in 32-bit");

      --  Test To_Epoch_Seconds (32 to 64 bit conversion)
      declare
         Val_32 : constant Epoch_Seconds_32_Type := 1_000_000;
         Val_64 : constant Epoch_Seconds_Type := To_Epoch_Seconds (Val_32);
      begin
         Assert (Val_64 = 1_000_000, "32-to-64 conversion preserves value");
      end;

      --  Test To_Epoch_Seconds_32 (64 to 32 bit conversion)
      declare
         Val_64 : constant Epoch_Seconds_Type := 500_000;
         Val_32 : constant Epoch_Seconds_32_Type :=
           To_Epoch_Seconds_32 (Val_64);
      begin
         Assert (Val_32 = 500_000, "64-to-32 conversion preserves value");
      end;

      --  Test negative epoch times (before 1970)
      declare
         Before_1970 : constant Epoch_Seconds_Type := -86_400;
      begin
         Assert (Fits_In_32_Bit (Before_1970), "1 day before 1970 fits");
      end;

      --  Test boundary values using helper to avoid always-true warnings
      declare
         Min_32 : constant Epoch_Seconds_Type :=
           Epoch_Seconds_Type (Epoch_Seconds_32_Type'First);
         Max_32 : constant Epoch_Seconds_Type :=
           Epoch_Seconds_Type (Epoch_Seconds_32_Type'Last);
      begin
         Assert (Equals (Epoch_Seconds_32_Min, Min_32), "32-bit min correct");
         Assert (Equals (Epoch_Seconds_32_Max, Max_32), "32-bit max correct");
      end;
   end Test_Epoch_Seconds_Accessors;

   --  =====================================================================
   --  Test: Transition Type Accessors
   --  =====================================================================
   procedure Test_Transition_Accessors is
      T1 : constant Transition_Type := (Time => 1_000_000, Type_Index => 0);
      T2 : constant Transition_Type := (Time => 2_000_000, Type_Index => 1);
      T3 : constant Transition_Type := (Time => 1_000_000, Type_Index => 2);
      Check_Time : constant Epoch_Seconds_Type := 1_500_000;
   begin
      Put_Line ("Test: Transition accessor functions");

      --  Test comparison operators
      Assert (T1 < T2, "Earlier transition is less than later");
      Assert (T1 <= T2, "Earlier transition is <= later");
      Assert (T2 > T1, "Later transition is greater than earlier");
      Assert (T2 >= T1, "Later transition is >= earlier");
      Assert (T1 <= T3, "Same time transitions are <=");
      Assert (T1 >= T3, "Same time transitions are >=");

      --  Test Occurs_Before
      Assert (Occurs_Before (T1, Check_Time), "T1 occurs before 1.5M");
      Assert (not Occurs_Before (T2, Check_Time), "T2 does not occur before");

      --  Test Occurs_After
      Assert (Occurs_After (T2, Check_Time), "T2 occurs after 1.5M");
      Assert (not Occurs_After (T1, Check_Time), "T1 does not occur after");

      --  Test Occurs_At
      Assert (Occurs_At (T1, 1_000_000), "T1 occurs at 1M");
      Assert (not Occurs_At (T1, Check_Time), "T1 does not occur at 1.5M");
   end Test_Transition_Accessors;

begin
   --  Run all tests
   Test_Source_Info_Accessors;
   Test_TZif_Header_Accessors;
   Test_Transition_Info_Accessors;
   Test_UTC_Offset_Accessors;
   Test_Epoch_Seconds_Accessors;
   Test_Transition_Accessors;
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
end Test_Value_Object_Accessors;
