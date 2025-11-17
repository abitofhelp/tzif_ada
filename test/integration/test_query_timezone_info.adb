pragma Ada_2022;
--  ======================================================================
--  Test_Query_Timezone_Info
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Query Timezone Info functionality.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Test_Framework;
with TZif.Infrastructure.Query_Timezone_Info;
with TZif.Domain.Error;
with TZif.Domain.Value_Object.UTC_Offset;
procedure Test_Query_Timezone_Info is
   use TZif.Infrastructure.Query_Timezone_Info;
   use TZif.Domain.Error;
   use TZif.Domain.Value_Object.UTC_Offset;
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
   --  Test: Query Valid Timezone File
   --  =====================================================================
   procedure Test_Query_Valid_Timezone is
      --  Use UTC which should exist on the system
      UTC_Path : constant String := "/usr/share/zoneinfo/UTC";
      Result   : Query_Result_Type;
      Info     : Timezone_Info;
   begin
      Put_Line ("Test: Query timezone info from valid file (UTC)");
      Result := Query_Timezone_Info (UTC_Path, 0);
      if Info_Result.Is_Ok (Result) then
         Info := Info_Result.Value (Result);
         Assert (Info.UTC_Offset = 0, "UTC offset should be 0");
         Assert (not Info.Is_DST, "UTC should not have DST");
         Assert
           (Info.Abbr_Length > 0 and then Info.Abbr_Length <= 10,
            "Abbreviation length should be valid");
         --  Check that abbreviation is padded correctly
         declare
            Abbrev : constant String :=
              Info.Abbreviation (1 .. Info.Abbr_Length);
         begin
            Put_Line ("    UTC abbreviation: '" & Abbrev & "'");
            Assert (Abbrev'Length > 0, "Abbreviation should not be empty");
         end;
      else
         --  If UTC file doesn't exist, skip this test
         Put_Line ("  [SKIP] UTC file not found at " & UTC_Path);
      end if;
   end Test_Query_Valid_Timezone;
   --  =====================================================================
   --  Test: Query Nonexistent File
   --  =====================================================================
   procedure Test_Query_Nonexistent_File is
      Result : constant Query_Result_Type :=
        Query_Timezone_Info ("/nonexistent/timezone/file", 0);
   begin
      Put_Line ("Test: Query nonexistent timezone file");
      Assert
        (Info_Result.Is_Error (Result),
         "Should return Error for nonexistent file");
      if Info_Result.Is_Error (Result) then
         declare
            Err : constant Error_Type := Info_Result.Error_Info (Result);
         begin
            Assert
              (Err.Kind = Infrastructure_Error,
               "Error should be Infrastructure_Error");
         end;
      end if;
   end Test_Query_Nonexistent_File;
   --  =====================================================================
   --  Test: Query Invalid TZif File
   --  =====================================================================
   procedure Test_Query_Invalid_File is
      --  Use a directory instead of a file (should fail)
      Result : constant Query_Result_Type :=
        Query_Timezone_Info ("/usr/share/zoneinfo", 0);
   begin
      Put_Line ("Test: Query with directory path (not a file)");
      Assert
        (Info_Result.Is_Error (Result),
         "Should return Error for directory path");
      if Info_Result.Is_Error (Result) then
         declare
            Err : constant Error_Type := Info_Result.Error_Info (Result);
         begin
            --  Could be Infrastructure_Error or Validation_Error
            Assert
              (Err.Kind in Infrastructure_Error | Validation_Error,
               "Error should be Infrastructure or Validation error");
         end;
      end if;
   end Test_Query_Invalid_File;
   --  =====================================================================
   --  Test: Query with Different Epoch Times
   --  =====================================================================
   procedure Test_Query_Different_Times is
      UTC_Path : constant String := "/usr/share/zoneinfo/UTC";
      Result_1 : Query_Result_Type;
      Result_2 : Query_Result_Type;
   begin
      Put_Line ("Test: Query same file at different times");
      --  Query at epoch 0 (1970-01-01 00:00:00 UTC)
      Result_1 := Query_Timezone_Info (UTC_Path, 0);
      --  Query at a different time (2024-01-01 00:00:00 UTC = 1704067200)
      Result_2 := Query_Timezone_Info (UTC_Path, 1704067200);
      if Info_Result.Is_Ok (Result_1) and then Info_Result.Is_Ok (Result_2)
      then
         declare
            Info_1 : constant Timezone_Info := Info_Result.Value (Result_1);
            Info_2 : constant Timezone_Info := Info_Result.Value (Result_2);
         begin
            Assert
              (Info_1.UTC_Offset = Info_2.UTC_Offset,
               "UTC offset should be same at both times for UTC");
            Assert
              (Info_1.Is_DST = Info_2.Is_DST,
               "DST status should be same for UTC");
         end;
      elsif not Info_Result.Is_Ok (Result_1) then
         Put_Line ("  [SKIP] UTC file not available");
      end if;
   end Test_Query_Different_Times;
begin
   --  Run all tests
   Test_Query_Valid_Timezone;
   Test_Query_Nonexistent_File;
   Test_Query_Invalid_File;
   Test_Query_Different_Times;
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
end Test_Query_Timezone_Info;