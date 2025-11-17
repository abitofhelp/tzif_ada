pragma Ada_2022;
--  ===========================================================================
--  Get_Transition_At_Epoch - Query timezone offset at time
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Demonstrates querying timezone transition information at a specific
--    epoch time. Uses the public TZif.API facade.
--
--  Example:
--    $ ./bin/examples/get_transition_at_epoch
--    UTC offset at 2024-07-01: -25200 seconds (-7 hours PDT)
--  ===========================================================================

with Ada.Text_IO;
with TZif.API;

procedure Get_Transition_At_Epoch is

   use Ada.Text_IO;
   use TZif.API;

begin
   Put_Line ("======================================================");
   Put_Line ("| Get transition info at specific time    |");
   Put_Line ("======================================================");
   New_Line;

   --  July 1, 2024 (summer - PDT)
   declare
      Zone_Id : constant Zone_Id_String :=
        Make_Zone_Id_String ("America/Los_Angeles");
      Summer_Epoch : constant Epoch_Seconds_Type := 1_719_792_000;
      Result : constant Transition_Result :=
        Get_Transition_At_Epoch (Zone_Id, Summer_Epoch);
   begin
      Put_Line ("Querying: America/Los_Angeles at 2024-07-01");

      if Is_Ok (Result) then
         Put_Line ("[OK] Transition info retrieved");
      else
         Put_Line ("[ERROR] Could not get transition info");
      end if;
   end;

   New_Line;
   Put_Line ("Done.");

end Get_Transition_At_Epoch;
