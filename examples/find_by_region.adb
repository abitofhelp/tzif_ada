pragma Ada_2022;
--  ===========================================================================
--  Find_By_Region - Search zones by geographic region
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Demonstrates finding timezones by geographic region.
--    Uses callback-based iteration for results.
--
--  Example:
--    $ ./bin/examples/find_by_region
--    Searching for zones in region 'America'...
--    [OK] Found zones:
--      America/Adak
--      America/Anchorage
--      ...
--  ===========================================================================

with Ada.Text_IO;
with TZif.API;

procedure Find_By_Region is

   use Ada.Text_IO;

   package API renames TZif.API;
   package Port renames API.Find_Region_Port;

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
   Put_Line ("| Find_By_Region - Search zones by geographic region |");
   Put_Line ("======================================================");
   New_Line;

   --  Search for zones in America region
   declare
      Region : constant API.Region_String :=
        Port.Region_Strings.To_Bounded_String ("America");
      Result : constant API.Region_Result :=
        API.Find_By_Region (Region, Handle_Zone'Unrestricted_Access);
   begin
      Put_Line
        ("Searching for zones in region '"
         & Port.Region_Strings.To_String (Region) & "'...");
      New_Line;

      if Port.Find_By_Region_Result_Package.Is_Ok (Result) then
         Put_Line ("[OK] Found" & Natural'Image (Zone_Count) & " zones");
         New_Line;
         Put_Line ("First" & Natural'Image (Max_Display) & " zones shown");
         if Zone_Count > Max_Display then
            Put_Line
              ("  ... and" & Natural'Image (Zone_Count - Max_Display)
               & " more");
         end if;
      else
         Put_Line ("[ERROR] Search failed");
         declare
            Err : constant API.Error_Type :=
              Port.Find_By_Region_Result_Package.Error_Info (Result);
         begin
            Put_Line ("  Error: " & API.Error_Strings.To_String (Err.Message));
         end;
      end if;
   end;

   New_Line;

   --  Search for zones in Europe region
   Zone_Count := 0;
   declare
      Region : constant API.Region_String :=
        Port.Region_Strings.To_Bounded_String ("Europe");
      Result : constant API.Region_Result :=
        API.Find_By_Region (Region, Handle_Zone'Unrestricted_Access);
   begin
      Put_Line
        ("Searching for zones in region '"
         & Port.Region_Strings.To_String (Region) & "'...");
      New_Line;

      if Port.Find_By_Region_Result_Package.Is_Ok (Result) then
         Put_Line ("[OK] Found" & Natural'Image (Zone_Count) & " zones");
      else
         Put_Line ("[ERROR] Search failed");
      end if;
   end;

   New_Line;
   Put_Line ("Done.");

end Find_By_Region;
