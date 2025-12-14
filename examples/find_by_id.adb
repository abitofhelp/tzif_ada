pragma Ada_2022;
--  ===========================================================================
--  Find_By_Id - Find and parse timezone by ID
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Demonstrates finding and parsing a timezone by exact ID match.
--    Uses the public TZif.API facade with smart constructor pattern.
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

   --  Qualified access to avoid ambiguity with overloaded Is_Ok/Value
   package API renames TZif.API;

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
      Id_Result : constant API.Zone_Id_Result :=
        API.Make_Zone_Id ("America/Phoenix");
   begin
      Put_Line ("Looking up: America/Phoenix");

      if API.Is_Ok (Id_Result) then
         declare
            Zone_Id : constant API.Zone_Id_Type :=
              API.Value (Id_Result);
            Find_Result : constant API.Zone_Result :=
              API.Find_By_Id (Zone_Id);
         begin
            if API.Is_Ok (Find_Result) then
               Put_Line ("[OK] Found zone: " & API.To_String (Zone_Id));
               Put_Line ("  - Success!");
            else
               Put_Line ("[ERROR] Failed to find zone");
            end if;
         end;
      else
         Put_Line ("[ERROR] Invalid zone ID");
      end if;
   end;

   New_Line;

   --  Test case 2: America/New_York (has DST)
   declare
      Id_Result : constant API.Zone_Id_Result :=
        API.Make_Zone_Id ("America/New_York");
   begin
      Put_Line ("Looking up: America/New_York");

      if API.Is_Ok (Id_Result) then
         declare
            Zone_Id : constant API.Zone_Id_Type :=
              API.Value (Id_Result);
            Find_Result : constant API.Zone_Result :=
              API.Find_By_Id (Zone_Id);
         begin
            if API.Is_Ok (Find_Result) then
               Put_Line ("[OK] Found zone: " & API.To_String (Zone_Id));
            else
               Put_Line ("[ERROR] Failed to find zone");
            end if;
         end;
      else
         Put_Line ("[ERROR] Invalid zone ID");
      end if;
   end;

   New_Line;

   --  Test case 3: Invalid zone ID (valid format, but zone doesn't exist)
   declare
      Id_Result : constant API.Zone_Id_Result :=
        API.Make_Zone_Id ("Invalid/Nonexistent");
   begin
      Put_Line ("Looking up: Invalid/Nonexistent");

      if API.Is_Ok (Id_Result) then
         declare
            Zone_Id : constant API.Zone_Id_Type :=
              API.Value (Id_Result);
            Find_Result : constant API.Zone_Result :=
              API.Find_By_Id (Zone_Id);
         begin
            if API.Is_Ok (Find_Result) then
               Put_Line ("[UNEXPECTED] Found zone!");
            else
               Put_Line ("[EXPECTED] Zone not found - this is correct");
            end if;
         end;
      else
         Put_Line ("[ERROR] Invalid zone ID format");
      end if;
   end;

   New_Line;
   Put_Line ("Done.");

end Find_By_Id;
