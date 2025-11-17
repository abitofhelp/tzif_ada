pragma Ada_2022;
--  ======================================================================
--  Simple_Zone_Test
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

procedure Simple_Zone_Test is
   use Ada.Text_IO;
   use TZif.API;

   Juneau_Zone : constant Zone_Id_String :=
     Make_Zone_Id_String ("America/Juneau");
   NY_Zone : constant Zone_Id_String :=
     Make_Zone_Id_String ("America/New_York");
   Epoch : constant Epoch_Seconds_Type := 1_705_320_000;
begin
   --  Test 1: Find_Zone
   Put ("Find_Zone America/Juneau: ");
   declare
      Find_Result : constant Zone_Result :=
        Find_Zone ("America/Juneau");
   begin
      if Is_Ok (Find_Result) then
         Put_Line ("FOUND");
      else
         Put_Line ("NOT FOUND");
      end if;
   end;

   Put ("Find_Zone America/New_York: ");
   declare
      Find_Result : constant Zone_Result :=
        Find_Zone ("America/New_York");
   begin
      if Is_Ok (Find_Result) then
         Put_Line ("FOUND");
      else
         Put_Line ("NOT FOUND");
      end if;
   end;

   --  Test 2: Get_Offset_At_Time
   Put_Line ("");
   Put ("Get_Offset_At_Time America/Juneau: ");
   declare
      Result : constant Transition_Result :=
        Get_Offset_At_Time (Juneau_Zone, Epoch);
   begin
      if Is_Ok (Result) then
         Put_Line ("SUCCESS");
      else
         Put_Line ("FAILED");
      end if;
   end;

   Put ("Get_Offset_At_Time America/New_York: ");
   declare
      Result : constant Transition_Result :=
        Get_Offset_At_Time (NY_Zone, Epoch);
   begin
      if Is_Ok (Result) then
         Put_Line ("SUCCESS");
      else
         Put_Line ("FAILED");
      end if;
   end;
end Simple_Zone_Test;
