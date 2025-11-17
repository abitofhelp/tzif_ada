pragma Ada_2022;
--  ======================================================================
--  Test_Platform_Stubs
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Platform Stubs functionality.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Test_Framework;
with TZif.Infrastructure.Platform;
with TZif.Infrastructure.Platform.Windows;
with TZif.Domain.Error;
procedure Test_Platform_Stubs is
   use TZif.Infrastructure.Platform;
   use TZif.Infrastructure.Platform.Windows;
   use TZif.Domain.Error;
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
   --  Test: Windows Read_Symbolic_Link Stub
   --  =====================================================================
   procedure Test_Windows_Stub is
      Result : constant Platform_String_Result :=
        Read_Symbolic_Link ("/some/path");
   begin
      Put_Line ("Test: Windows platform stub returns not-implemented error");
      Assert
        (String_Result.Is_Error (Result), "Windows stub should return Error");
      --  Check that it's an infrastructure error
      if String_Result.Is_Error (Result) then
         declare
            Err : constant Error_Type := String_Result.Error_Info (Result);
         begin
            Assert
              (Err.Kind = Infrastructure_Error,
               "Error should be Infrastructure_Error");
         end;
      end if;
   end Test_Windows_Stub;
begin
   --  Run all tests
   Test_Windows_Stub;
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
end Test_Platform_Stubs;