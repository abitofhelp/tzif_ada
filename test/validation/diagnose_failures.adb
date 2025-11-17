pragma Ada_2022;
--  ======================================================================
--  Diagnose_Failures
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Test support utilities.
--
--  ======================================================================

with Ada.Text_IO;
with TZif.API;

procedure Diagnose_Failures is

   use Ada.Text_IO;
   use TZif.API;

   Test_Epoch : constant Epoch_Seconds_Type :=
     1_705_320_000;  -- 2024-01-15 12:00:00
   procedure Test_Zone (Zone_Name : String) is
      Zone_Id : constant Zone_Id_String := Make_Zone_Id_String (Zone_Name);
      Result : constant Transition_Result :=
        Get_Offset_At_Time (Zone_Id, Test_Epoch);
   begin
      Put ("Zone: " & Zone_Name);
      Put (" (length:" & Zone_Name'Length'Image & ")");
      Put (" => ");

      if Is_Ok (Result) then
         declare
            Transition : constant Transition_Info_Type := Value (Result);
         begin
            Put_Line (
              "SUCCESS - Offset: " & Transition.UTC_Offset_Seconds'Image & " seconds");

         end;
      else
         declare
            Err : constant Error_Type := Error_Info (Result);
         begin
            Put_Line ("FAILED");
            Put_Line ("  Kind: " & Err.Kind'Image);
            Put_Line ("  Message: " & To_String (Err.Message));
         end;
      end if;
   end Test_Zone;

begin
   Put_Line ("Diagnosing Failed Zones");
   Put_Line ("=======================");
   Put_Line ("");

   Test_Zone ("America/Juneau");
   Put_Line ("");

   Test_Zone ("America/Metlakatla");
   Put_Line ("");

   Test_Zone ("Asia/Manila");
   Put_Line ("");

   Test_Zone ("Pacific/Palau");
   Put_Line ("");

   Put_Line ("Diagnosis complete.");
end Diagnose_Failures;
