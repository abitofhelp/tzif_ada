pragma Ada_2022;
--  ===========================================================================
--  Find_By_Id - Find and parse timezone by ID
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Demonstrates finding and parsing a timezone by exact ID match.
--    Uses the public TZif.API facade.
--
--  Example:
--    $ ./bin/examples/find_by_id
--    Looking up timezone: America/Phoenix
--    [OK] Found zone: America/Phoenix
--    - Transitions: 5
--    - Timezone types: 2
--  ===========================================================================

with Ada.Text_IO;
with TZif.API;

procedure Find_By_Id is

   use Ada.Text_IO;
   use TZif.API;

begin
   Put_Line
     ("============================================"
      & "==========================");
   Put_Line
     ("| Find_By_Id - Find timezone by exact ID              |");
   Put_Line
     ("============================================"
      & "==========================");
   New_Line;

   --  Test case 1: America/Phoenix (no DST)
   declare
      Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("America/Phoenix");
      Result  : constant Zone_Result := Find_By_Id (Zone_Id);
   begin
      Put_Line ("Looking up: America/Phoenix");

      if Is_Ok (Result) then
         Put_Line ("[OK] Found zone: " & To_String (Zone_Id));
         Put_Line ("  - Success!");
      else
         Put_Line ("[ERROR] Failed to find zone");
      end if;
   end;

   New_Line;

   --  Test case 2: America/New_York (has DST)
   declare
      Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("America/New_York");
      Result  : constant Zone_Result := Find_By_Id (Zone_Id);
   begin
      Put_Line ("Looking up: America/New_York");

      if Is_Ok (Result) then
         Put_Line ("[OK] Found zone: " & To_String (Zone_Id));
      else
         Put_Line ("[ERROR] Failed to find zone");
      end if;
   end;

   New_Line;

   --  Test case 3: Invalid zone ID
   declare
      Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("Invalid/Nonexistent");
      Result  : constant Zone_Result := Find_By_Id (Zone_Id);
   begin
      Put_Line ("Looking up: Invalid/Nonexistent");

      if Is_Ok (Result) then
         Put_Line ("[UNEXPECTED] Found zone!");
      else
         Put_Line ("[EXPECTED] Zone not found - this is correct");
      end if;
   end;

   New_Line;
   Put_Line ("Done.");

end Find_By_Id;
