pragma Ada_2022;
--  ===========================================================================
--  Find_By_Regex - Search zones by regular expression
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Demonstrates finding timezones using regular expression matching.
--    Uses callback-based iteration for results.
--
--  Example:
--    $ ./bin/examples/find_by_regex
--    Searching for zones matching '^America/New_.*'...
--    [OK] Found zones:
--      America/New_York
--  ===========================================================================

with Ada.Text_IO;
with TZif.API;

procedure Find_By_Regex is

   use Ada.Text_IO;

   package API renames TZif.API;
   package Port renames API.Find_Regex_Port;

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
   Put_Line ("| Find_By_Regex - Search zones by regular expression |");
   Put_Line ("======================================================");
   New_Line;

   --  Search for zones starting with America/New_
   declare
      Regex  : constant API.Regex_String :=
        Port.Regex_Strings.To_Bounded_String ("^America/New_.*");
      Result : constant API.Regex_Result :=
        API.Find_By_Regex (Regex, Handle_Zone'Unrestricted_Access);
   begin
      Put_Line
        ("Searching for zones matching '"
         & Port.Regex_Strings.To_String (Regex) & "'...");
      New_Line;

      if Port.Find_By_Regex_Result_Package.Is_Ok (Result) then
         Put_Line ("[OK] Found" & Natural'Image (Zone_Count) & " zones:");
      else
         Put_Line ("[ERROR] Search failed");
         declare
            Err : constant API.Error_Type :=
              Port.Find_By_Regex_Result_Package.Error_Info (Result);
         begin
            Put_Line ("  Error: " & API.Error_Strings.To_String (Err.Message));
         end;
      end if;
   end;

   New_Line;

   --  Search for zones ending with 'Angeles' or 'Francisco'
   Zone_Count := 0;
   declare
      Regex  : constant API.Regex_String :=
        Port.Regex_Strings.To_Bounded_String (".*(Angeles|Francisco)$");
      Result : constant API.Regex_Result :=
        API.Find_By_Regex (Regex, Handle_Zone'Unrestricted_Access);
   begin
      Put_Line
        ("Searching for zones matching '"
         & Port.Regex_Strings.To_String (Regex) & "'...");
      New_Line;

      if Port.Find_By_Regex_Result_Package.Is_Ok (Result) then
         Put_Line ("[OK] Found" & Natural'Image (Zone_Count) & " zones:");
      else
         Put_Line ("[ERROR] Search failed");
      end if;
   end;

   New_Line;

   --  Example with invalid regex (to show error handling)
   Zone_Count := 0;
   declare
      Regex  : constant API.Regex_String :=
        Port.Regex_Strings.To_Bounded_String ("[invalid");
      Result : constant API.Regex_Result :=
        API.Find_By_Regex (Regex, Handle_Zone'Unrestricted_Access);
   begin
      Put_Line
        ("Searching with invalid regex '"
         & Port.Regex_Strings.To_String (Regex) & "'...");
      New_Line;

      if Port.Find_By_Regex_Result_Package.Is_Ok (Result) then
         Put_Line ("[UNEXPECTED] Invalid regex succeeded");
      else
         Put_Line ("[EXPECTED] Invalid regex correctly rejected");
      end if;
   end;

   New_Line;
   Put_Line ("Done.");

end Find_By_Regex;
