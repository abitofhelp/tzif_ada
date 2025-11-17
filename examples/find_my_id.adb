pragma Ada_2022;
--  ===========================================================================
--  Find_My_Id - Discover local system timezone
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Demonstrates discovering the local system's timezone.
--    Uses the public TZif.API facade.
--
--  Example:
--    $ ./bin/examples/find_my_id
--    [OK] Local timezone: America/Los_Angeles
--  ===========================================================================

with Ada.Text_IO;
with TZif.API;

procedure Find_My_Id is

   use Ada.Text_IO;
   use TZif.API;

begin
   Put_Line ("====================================================");
   Put_Line ("| Find_My_Id - Discover local timezone  |");
   Put_Line ("====================================================");
   New_Line;

   declare
      Result : constant My_Zone_Result := Find_My_Id;
   begin
      Put_Line ("Detecting local system timezone...");

      if Is_Ok (Result) then
         declare
            Zone_Id : constant Zone_Id_Type := Value (Result);
         begin
            Put_Line ("[OK] Local timezone: " & To_String (Zone_Id));
         end;
      else
         Put_Line ("[ERROR] Could not determine local timezone");
      end if;
   end;

   New_Line;
   Put_Line ("Done.");

end Find_My_Id;
