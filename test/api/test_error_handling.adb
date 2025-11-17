pragma Ada_2022;
--  ======================================================================
--  Test_Error_Handling
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Unit tests for Error Handling functionality.
--
--  ======================================================================

with Ada.Text_IO;
with TZif.API;

procedure Test_Error_Handling is
   use Ada.Text_IO;
   use TZif.API;

   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;

   procedure Print_Error (Err : Error_Type; Label : String) is
   begin
      Put_Line ("  " & Label & ":");
      Put_Line ("    Kind: " & Err.Kind'Image);
      Put_Line ("    Message: " & Error_Strings.To_String (Err.Message));
   end Print_Error;

   procedure Test_Nonexistent_Zone is
      Zone_Id : constant Zone_Id_String :=
        Make_Zone_Id_String ("Nonexistent/Timezone");
      Epoch : constant Epoch_Seconds_Type := 0;
      Result : constant Transition_Result :=
        Get_Offset_At_Time (Zone_Id, Epoch);
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("[TEST] Nonexistent zone error handling");

      if Is_Error (Result) then
         declare
            Err : constant Error_Type := Error_Info (Result);
         begin
            Print_Error (Err, "Expected Error");
            Pass_Count := Pass_Count + 1;
            Put_Line ("  [PASS] Correctly returned error for " &
                     "nonexistent zone");
         end;
      else
         Put_Line ("  [FAIL] Should have returned error!");
      end if;

      Put_Line ("");
   end Test_Nonexistent_Zone;

   procedure Test_Invalid_Zone_Name is
      --  Test with empty string (should fail validation)
      Zone_Id : constant Zone_Id_String :=
        Make_Zone_Id_String ("");
      Epoch : constant Epoch_Seconds_Type := 0;
      Result : constant Transition_Result :=
        Get_Offset_At_Time (Zone_Id, Epoch);
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("[TEST] Invalid zone name error handling");

      if Is_Error (Result) then
         declare
            Err : constant Error_Type := Error_Info (Result);
         begin
            Print_Error (Err, "Expected Error");

            --  Should be some kind of error (validation or infrastructure)
            --  Just verify we got an error response
            Pass_Count := Pass_Count + 1;
            Put_Line ("  [PASS] Correctly returned error for invalid input");
         end;
      else
         Put_Line ("  [FAIL] Should have returned error for empty zone name!");
      end if;

      Put_Line ("");
   end Test_Invalid_Zone_Name;

   procedure Test_Valid_Zone_Success is
      Zone_Id : constant Zone_Id_String :=
        Make_Zone_Id_String ("America/New_York");
      Epoch : constant Epoch_Seconds_Type := 1_705_320_000;
      Result : constant Transition_Result :=
        Get_Offset_At_Time (Zone_Id, Epoch);
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("[TEST] Valid zone returns success (not error)");

      if Is_Ok (Result) then
         Pass_Count := Pass_Count + 1;
         Put_Line ("  [PASS] Valid zone returned Ok result");
      else
         declare
            Err : constant Error_Type := Error_Info (Result);
         begin
            Put_Line ("  [FAIL] Valid zone returned error:");
            Print_Error (Err, "Unexpected Error");
         end;
      end if;

      Put_Line ("");
   end Test_Valid_Zone_Success;

   procedure Test_Error_Message_Content is
      --  Intentionally broken path to trigger file I/O error
      Zone_Id : constant Zone_Id_String :=
        Make_Zone_Id_String ("../../../../../etc/passwd");
      Epoch : constant Epoch_Seconds_Type := 0;
      Result : constant Transition_Result :=
        Get_Offset_At_Time (Zone_Id, Epoch);
   begin
      Test_Count := Test_Count + 1;
      Put_Line ("[TEST] Error message contains useful context");

      if Is_Error (Result) then
         declare
            Err : constant Error_Type := Error_Info (Result);
            Msg : constant String := Error_Strings.To_String (Err.Message);
         begin
            Print_Error (Err, "Error with Context");

            --  Message should contain helpful information
            if Msg'Length > 10 then  --  Not just empty
               Pass_Count := Pass_Count + 1;
               declare
                  Len : constant Natural := Msg'Length;
               begin
                  Put_Line ("  [PASS] Error message is informative (" &
                           Len'Image & " chars)");
               end;
            else
               Put_Line ("  [FAIL] Error message too short: """ &
                        Msg & """");
            end if;
         end;
      else
         Put_Line ("  [FAIL] Should have returned error!");
      end if;

      Put_Line ("");
   end Test_Error_Message_Content;

begin
   Put_Line ("========================================================");
   Put_Line ("  TZif Error Handling Demonstration");
   Put_Line ("========================================================");
   Put_Line ("");
   Put_Line ("This test demonstrates proper error handling patterns.");
   Put_Line ("Developers: Study these patterns for your own code!");
   Put_Line ("");
   Put_Line ("Error Types:");
   Put_Line ("  - Validation_Error: Invalid input data");
   Put_Line ("  - Infrastructure_Error: I/O, file not found, etc.");
   Put_Line ("  - Resource_Error: Out of memory, limits exceeded");
   Put_Line ("  - Internal_Error: Should never happen (bugs)");
   Put_Line ("");
   Put_Line ("========================================================");
   Put_Line ("");

   --  Run all error handling tests
   Test_Nonexistent_Zone;
   Test_Invalid_Zone_Name;
   Test_Valid_Zone_Success;
   Test_Error_Message_Content;

   --  Summary
   Put_Line ("========================================================");
   Put_Line ("  Error Handling Test Summary");
   Put_Line ("========================================================");
   Put_Line ("Tests run:   " & Test_Count'Image);
   Put_Line ("Tests passed:" & Pass_Count'Image);
   declare
      Fail_Count : constant Natural := Test_Count - Pass_Count;
   begin
      Put_Line ("Tests failed:" & Fail_Count'Image);
   end;
   Put_Line ("========================================================");
   Put_Line ("");

   if Pass_Count = Test_Count then
      Put_Line ("[SUCCESS] All error handling patterns working correctly!");
   else
      Put_Line ("[PARTIAL] Some error handling issues detected.");
   end if;

end Test_Error_Handling;
