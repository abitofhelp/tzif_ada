pragma Ada_2022;
--  ======================================================================
--  Check_Errors
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
with TZif.Domain.Error;
with TZif.Application.Port.Inbound.Get_Transition_At_Epoch;

procedure Check_Errors is
   use Ada.Text_IO;
   use TZif.API;
   use TZif.Domain.Error;

   package Get_Trans_Port renames
     TZif.Application.Port.Inbound.Get_Transition_At_Epoch;

   procedure Check_Zone (Zone_Name : String) is
      Zone_Id : constant Zone_Id_String :=
        Make_Zone_Id_String (Zone_Name);
      Epoch : constant Epoch_Seconds_Type := 1_705_320_000;
      Result : constant Transition_Result :=
        Get_Offset_At_Time (Zone_Id, Epoch);
   begin
      Put_Line ("=========================================");
      Put_Line ("Zone: " & Zone_Name);
      Put_Line ("=========================================");

      if Is_Error (Result) then
         declare
            Err : constant TZif.Domain.Error.Error_Type :=
              Get_Trans_Port.Get_Transition_Result_Package.Error_Info (Result);
         begin
            Put_Line ("STATUS: FAILED");
            Put_Line ("");
            Put_Line ("Error Kind: " & Err.Kind'Image);
            Put_Line ("Error Message: " &
                     TZif.Domain.Error.Error_Strings.To_String (Err.Message));
         end;
      else
         Put_Line ("STATUS: SUCCESS");
      end if;

      Put_Line ("");
   end Check_Zone;

begin
   Put_Line ("Checking Failed Zones for Error Messages");
   Put_Line ("");

   Check_Zone ("America/Juneau");
   Check_Zone ("America/Metlakatla");
   Check_Zone ("Asia/Manila");
   Check_Zone ("Pacific/Palau");
   Check_Zone ("America/New_York");  --  Control - should work

end Check_Errors;
