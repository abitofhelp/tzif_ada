pragma Ada_2022;
--  ===========================================================================
--  Find_By_Pattern - Search zones by substring
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Demonstrates finding timezones by substring pattern matching.
--    Uses callback-based iteration for results.
--
--  Example:
--    $ ./bin/examples/find_by_pattern
--    Searching for zones containing 'York'...
--    [OK] Found zones:
--      America/New_York
--  ===========================================================================

with Ada.Text_IO;
with TZif.API;

procedure Find_By_Pattern is

   use Ada.Text_IO;

   package API renames TZif.API;
   package Port renames API.Find_Pattern_Port;

   Zone_Count  : Natural := 0;
   Max_Display : constant := 10;

   --  Callback procedure invoked for each matching zone
   procedure Handle_Zone (Name : Port.Zone_Name_String) is
   begin
      Zone_Count := Zone_Count + 1;
      if Zone_Count <= Max_Display then
         Put_Line ("  " & Port.Zone_Name_Strings.To_String (Name));
      end if;
   end Handle_Zone;

begin
   Put_Line ("======================================================");
   Put_Line ("| Find_By_Pattern - Search zones by substring        |");
   Put_Line ("======================================================");
   New_Line;

   --  Search for zones containing 'York'
   declare
      Pattern : constant API.Pattern_String :=
        Port.Pattern_Strings.To_Bounded_String ("York");
      Result  : constant API.Pattern_Result :=
        API.Find_By_Pattern (Pattern, Handle_Zone'Unrestricted_Access);
   begin
      Put_Line
        ("Searching for zones containing '"
         & Port.Pattern_Strings.To_String (Pattern) & "'...");
      New_Line;

      if Port.Find_By_Pattern_Result_Package.Is_Ok (Result) then
         Put_Line ("[OK] Found" & Natural'Image (Zone_Count) & " zones:");
      else
         Put_Line ("[ERROR] Search failed");
         declare
            Err : constant API.Error_Type :=
              Port.Find_By_Pattern_Result_Package.Error_Info (Result);
         begin
            Put_Line ("  Error: " & API.Error_Strings.To_String (Err.Message));
         end;
      end if;
   end;

   New_Line;

   --  Search for zones containing 'Pacific'
   Zone_Count := 0;
   declare
      Pattern : constant API.Pattern_String :=
        Port.Pattern_Strings.To_Bounded_String ("Pacific");
      Result  : constant API.Pattern_Result :=
        API.Find_By_Pattern (Pattern, Handle_Zone'Unrestricted_Access);
   begin
      Put_Line
        ("Searching for zones containing '"
         & Port.Pattern_Strings.To_String (Pattern) & "'...");
      New_Line;

      if Port.Find_By_Pattern_Result_Package.Is_Ok (Result) then
         Put_Line ("[OK] Found" & Natural'Image (Zone_Count) & " zones");
         if Zone_Count > Max_Display then
            Put_Line
              ("  (showing first"
               & Natural'Image (Max_Display) & ")");
         end if;
      else
         Put_Line ("[ERROR] Search failed");
      end if;
   end;

   New_Line;
   Put_Line ("Done.");

end Find_By_Pattern;
