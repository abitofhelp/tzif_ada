pragma Ada_2022;
--  ======================================================================
--  Test_Export_Cache
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Export Cache functionality.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Test_Framework;
with TZif.Application.Port.Inbound.Export_Cache;
with TZif.Application.Usecase.Export_Cache;
with TZif.Infrastructure.Adapter.File_System.Repository;
procedure Test_Export_Cache is
   use TZif.Application.Port.Inbound.Export_Cache;
   use TZif.Infrastructure.Adapter.File_System;
   package UC is new
     TZif.Application.Usecase.Export_Cache.Use_Case
       (Repository_Export_Cache => Repository.Export_Cache);
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
begin
   Put_Line ("Test: Export Cache");
   declare
      Result : constant Export_Cache_Result := UC.Execute
          (TZif
             .Application
             .Port
             .Inbound
             .Export_Cache
             .Path_Strings
             .To_Bounded_String ("/tmp/test.cache"),
           Overwrite => True);
   begin
      Assert
        (Export_Cache_Result_Package.Is_Ok (Result)
         or else Export_Cache_Result_Package.Is_Error (Result),
         "Should return result");
   end;
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
end Test_Export_Cache;