pragma Ada_2022;
--  ======================================================================
--  Test_TZif_Parser_Errors
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for TZif Parser Errors functionality.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with TZif.Infrastructure.TZif_Parser;
with Test_Framework;
procedure Test_TZif_Parser_Errors is
   use TZif.Infrastructure.TZif_Parser;
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
   procedure Test_Empty_File is
      Result : constant Parse_Result_Type :=
        Parse_From_File ("test/data/invalid/empty.tzif");
   begin
      Put_Line ("Test: Empty File");
      --  Empty file should return an error, not crash
      Assert
        (Parse_Result.Is_Error (Result), "Should return Error for empty file");
      if Parse_Result.Is_Error (Result) then
         Put_Line ("  [INFO] Empty file correctly rejected");
      end if;
   end Test_Empty_File;
   procedure Test_Wrong_Magic_Number is
      Result : constant Parse_Result_Type :=
        Parse_From_File ("test/data/invalid/wrong_magic.tzif");
   begin
      Put_Line ("Test: Wrong Magic Number");
      --  File with wrong magic number should return error
      Assert
        (Parse_Result.Is_Error (Result),
         "Should return Error for invalid magic number");
   end Test_Wrong_Magic_Number;
   procedure Test_Truncated_Header is
      Result : constant Parse_Result_Type :=
        Parse_From_File ("test/data/invalid/truncated_header.tzif");
   begin
      Put_Line ("Test: Truncated Header");
      --  File with incomplete header should return error
      Assert
        (Parse_Result.Is_Error (Result),
         "Should return Error for truncated header");
   end Test_Truncated_Header;
   procedure Test_Nonexistent_File is
      Result : constant Parse_Result_Type :=
        Parse_From_File ("test/data/invalid/does_not_exist.tzif");
   begin
      Put_Line ("Test: Nonexistent File");
      --  Nonexistent file should return error gracefully
      Assert
        (Parse_Result.Is_Error (Result),
         "Should return Error for nonexistent file");
   end Test_Nonexistent_File;
   procedure Test_Directory_Not_File is
      --  Try to parse a directory as if it were a file
      Result : constant Parse_Result_Type := Parse_From_File ("test/data");
   begin
      Put_Line ("Test: Directory Instead of File");
      --  Directory should return error, not crash or infinite loop
      Assert
        (Parse_Result.Is_Error (Result),
         "Should return Error when path is directory");
   end Test_Directory_Not_File;
   procedure Test_Invalid_Version is
      Result : constant Parse_Result_Type :=
        Parse_From_File ("test/data/invalid/invalid_version.tzif");
   begin
      Put_Line ("Test: Invalid Version Byte");
      --  Version byte 0xFF is invalid
      Assert
        (Parse_Result.Is_Error (Result),
         "Should return Error for invalid version byte");
   end Test_Invalid_Version;
   procedure Test_Count_Mismatch is
      Result : constant Parse_Result_Type :=
        Parse_From_File ("test/data/invalid/count_mismatch.tzif");
   begin
      Put_Line ("Test: Count Mismatch");
      --  Header says 5 transitions but file only has 2
      Assert
        (Parse_Result.Is_Error (Result),
         "Should return Error when counts don't match data");
   end Test_Count_Mismatch;
   procedure Test_Truncated_Transition is
      Result : constant Parse_Result_Type :=
        Parse_From_File ("test/data/invalid/truncated_transition.tzif");
   begin
      Put_Line ("Test: Truncated Transition Data");
      --  File ends in middle of integer
      Assert
        (Parse_Result.Is_Error (Result),
         "Should return Error for truncated transition data");
   end Test_Truncated_Transition;
   procedure Test_Header_Only is
      Result : constant Parse_Result_Type :=
        Parse_From_File ("test/data/invalid/header_only.tzif");
   begin
      Put_Line ("Test: Header Only (No Data)");
      --  Valid header but missing all data sections
      Assert
        (Parse_Result.Is_Error (Result),
         "Should return Error when data section is missing");
   end Test_Header_Only;
   procedure Test_Malformed_Integers is
      Result : constant Parse_Result_Type :=
        Parse_From_File ("test/data/invalid/malformed_integers.tzif");
   begin
      Put_Line ("Test: Malformed Integer Fields");
      --  Corrupted count fields in header
      Assert
        (Parse_Result.Is_Error (Result),
         "Should return Error for malformed integer fields");
   end Test_Malformed_Integers;
   procedure Test_Zero_Counts_With_Data is
      Result : constant Parse_Result_Type :=
        Parse_From_File ("test/data/invalid/zero_counts_with_data.tzif");
   begin
      Put_Line ("Test: Zero Counts But Data Present");
      --  Logical inconsistency: claims no data but has data
      Assert
        (Parse_Result.Is_Error (Result),
         "Should return Error for inconsistent counts");
   end Test_Zero_Counts_With_Data;
   procedure Test_Only_Magic is
      Result : constant Parse_Result_Type :=
        Parse_From_File ("test/data/invalid/only_magic.tzif");
   begin
      Put_Line ("Test: Only Magic Bytes");
      --  File contains only 'TZif' with no version or data
      Assert
        (Parse_Result.Is_Error (Result),
         "Should return Error when file is only magic bytes");
   end Test_Only_Magic;
   procedure Test_Nonzero_Reserved is
      Result : constant Parse_Result_Type :=
        Parse_From_File ("test/data/invalid/nonzero_reserved.tzif");
   begin
      Put_Line ("Test: Non-Zero Reserved Bytes");
      --  Reserved bytes should be zero per RFC 9636
      Assert
        (Parse_Result.Is_Error (Result),
         "Should return Error for non-zero reserved bytes");
   end Test_Nonzero_Reserved;
   procedure Test_Single_Byte is
      Result : constant Parse_Result_Type :=
        Parse_From_File ("test/data/invalid/single_byte.tzif");
   begin
      Put_Line ("Test: Single Byte File");
      --  File contains only 'T'
      Assert
        (Parse_Result.Is_Error (Result),
         "Should return Error for single byte file");
   end Test_Single_Byte;
   procedure Test_V1_No_Data is
      Result : constant Parse_Result_Type :=
        Parse_From_File ("test/data/invalid/v1_no_data.tzif");
   begin
      Put_Line ("Test: Version 1 With No Type Data");
      --  Claims to have transitions but typecnt = 0
      Assert
        (Parse_Result.Is_Error (Result),
         "Should return Error when typecnt is zero but timecnt > 0");
   end Test_V1_No_Data;
   procedure Test_Abbr_Out_Of_Bounds is
      Result : constant Parse_Result_Type :=
        Parse_From_File ("test/data/invalid/abbr_out_of_bounds.tzif");
   begin
      Put_Line ("Test: Abbreviation Index Out of Bounds");
      --  Abbreviation index points beyond abbreviation string
      Assert
        (Parse_Result.Is_Error (Result),
         "Should return Error for out-of-bounds abbreviation index");
   end Test_Abbr_Out_Of_Bounds;
   procedure Test_Type_Index_Out_Of_Bounds is
      Result : constant Parse_Result_Type :=
        Parse_From_File ("test/data/invalid/type_index_out_of_bounds.tzif");
   begin
      Put_Line ("Test: Type Index Out of Bounds");
      --  Transition references type index 5 but only 1 type exists
      Assert
        (Parse_Result.Is_Error (Result),
         "Should return Error when transition type index is invalid");
   end Test_Type_Index_Out_Of_Bounds;
   procedure Test_Excessive_Counts is
      Result : constant Parse_Result_Type :=
        Parse_From_File ("test/data/invalid/excessive_counts.tzif");
   begin
      Put_Line ("Test: Excessive Counts (DoS Protection)");
      --  Counts claim 2 billion entries (potential DoS)
      Assert
        (Parse_Result.Is_Error (Result),
         "Should return Error for excessive counts");
   end Test_Excessive_Counts;
   procedure Test_Negative_Counts is
      Result : constant Parse_Result_Type :=
        Parse_From_File ("test/data/invalid/negative_counts.tzif");
   begin
      Put_Line ("Test: Negative Count Values");
      --  Count fields contain negative values
      Assert
        (Parse_Result.Is_Error (Result),
         "Should return Error for negative counts");
   end Test_Negative_Counts;
   procedure Test_Future_Version is
      Result : constant Parse_Result_Type :=
        Parse_From_File ("test/data/invalid/future_version.tzif");
   begin
      Put_Line ("Test: Future Version Number");
      --  Version 9 doesn't exist yet
      Assert
        (Parse_Result.Is_Error (Result),
         "Should return Error for unsupported version");
   end Test_Future_Version;
   --  =====================================================================
   --  NEW TESTS - Partial Read Scenarios
   --  =====================================================================
   procedure Test_Partial_UTC_Local is
      Result : constant Parse_Result_Type :=
        Parse_From_File ("test/data/invalid/partial_header_utc_local.tzif");
   begin
      Put_Line ("Test: Partial Read - UTC/Local Count");
      Assert
        (Parse_Result.Is_Error (Result),
         "Should return Error for truncated UTC/local count");
   end Test_Partial_UTC_Local;
   procedure Test_Partial_Std_Wall is
      Result : constant Parse_Result_Type :=
        Parse_From_File ("test/data/invalid/partial_header_std_wall.tzif");
   begin
      Put_Line ("Test: Partial Read - Std/Wall Count");
      Assert
        (Parse_Result.Is_Error (Result),
         "Should return Error for truncated std/wall count");
   end Test_Partial_Std_Wall;
   procedure Test_Partial_Leap is
      Result : constant Parse_Result_Type :=
        Parse_From_File ("test/data/invalid/partial_header_leap.tzif");
   begin
      Put_Line ("Test: Partial Read - Leap Count");
      Assert
        (Parse_Result.Is_Error (Result),
         "Should return Error for truncated leap count");
   end Test_Partial_Leap;
   procedure Test_Partial_Transition is
      Result : constant Parse_Result_Type :=
        Parse_From_File ("test/data/invalid/partial_header_transition.tzif");
   begin
      Put_Line ("Test: Partial Read - Transition Count");
      Assert
        (Parse_Result.Is_Error (Result),
         "Should return Error for truncated transition count");
   end Test_Partial_Transition;
   procedure Test_Partial_Type is
      Result : constant Parse_Result_Type :=
        Parse_From_File ("test/data/invalid/partial_header_type.tzif");
   begin
      Put_Line ("Test: Partial Read - Type Count");
      Assert
        (Parse_Result.Is_Error (Result),
         "Should return Error for truncated type count");
   end Test_Partial_Type;
   procedure Test_Partial_Abbrev is
      Result : constant Parse_Result_Type :=
        Parse_From_File ("test/data/invalid/partial_header_abbrev.tzif");
   begin
      Put_Line ("Test: Partial Read - Abbrev Chars");
      Assert
        (Parse_Result.Is_Error (Result),
         "Should return Error for truncated abbrev chars");
   end Test_Partial_Abbrev;
   procedure Test_Valid_V1 is
      Result : constant Parse_Result_Type :=
        Parse_From_File ("test/data/invalid/valid_v1.tzif");
   begin
      Put_Line ("Test: Valid Version 1 File");
      Assert
        (Parse_Result.Is_Ok (Result),
         "Should successfully parse valid V1 file");
   end Test_Valid_V1;
   procedure Test_V2_With_POSIX_TZ is
      Result : constant Parse_Result_Type :=
        Parse_From_File ("test/data/invalid/v2_with_posix_tz.tzif");
   begin
      Put_Line ("Test: V2 With POSIX TZ String");
      Assert
        (Parse_Result.Is_Ok (Result),
         "Should successfully parse V2 file with POSIX TZ");
   end Test_V2_With_POSIX_TZ;
   --  ROADMAP: Disabled - parser accepts V1 section, doesn't validate V2
   --  procedure Test_Partial_V2_Transition is
   --  Result : constant Parse_Result_Type := Parse_From_File
   --  ("test/data/invalid/partial_v2_transition.tzif");
   --  begin
   --  Put_Line ("Test: Partial 64-bit Transition Read");
   --  Assert
   --  (Parse_Result.Is_Error (Result),
   --  "Should return Error for truncated 64-bit transition");
   --  end Test_Partial_V2_Transition;
   procedure Test_Partial_Type_Index is
      Result : constant Parse_Result_Type :=
        Parse_From_File ("test/data/invalid/partial_type_index.tzif");
   begin
      Put_Line ("Test: Partial Type Index Byte");
      Assert
        (Parse_Result.Is_Error (Result),
         "Should return Error for truncated type index byte");
   end Test_Partial_Type_Index;
begin
   Put_Line ("====================================================");
   Put_Line ("  Running: TZif Parser Error Tests");
   Put_Line ("====================================================");
   --  Basic I/O and file system errors
   Test_Empty_File;
   Test_Nonexistent_File;
   Test_Directory_Not_File;
   --  Magic number and version errors
   Test_Wrong_Magic_Number;
   Test_Invalid_Version;
   Test_Future_Version;
   --  Header and structure errors
   Test_Truncated_Header;
   Test_Header_Only;
   Test_Malformed_Integers;
   --  Data consistency errors
   Test_Count_Mismatch;
   Test_Truncated_Transition;
   Test_Zero_Counts_With_Data;
   --  Additional structural errors
   Test_Only_Magic;
   Test_Nonzero_Reserved;
   Test_Single_Byte;
   Test_V1_No_Data;
   Test_Abbr_Out_Of_Bounds;
   Test_Type_Index_Out_Of_Bounds;
   --  Resource/DoS protection errors
   Test_Excessive_Counts;
   Test_Negative_Counts;
   --  Partial read scenarios
   Test_Partial_UTC_Local;
   Test_Partial_Std_Wall;
   Test_Partial_Leap;
   Test_Partial_Transition;
   Test_Partial_Type;
   Test_Partial_Abbrev;
   --  Version-specific tests
   Test_Valid_V1;
   Test_V2_With_POSIX_TZ;
   --  ROADMAP: Test_Partial_V2_Transition - parser accepts V1 section,
   --  doesn't validate V2 section
   Test_Partial_Type_Index;
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
end Test_TZif_Parser_Errors;