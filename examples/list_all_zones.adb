pragma Ada_2022;
--  ===========================================================================
--  List_All_Zones - Enumerate all available timezones
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Demonstrates listing all timezone zones from a source.
--    Shows zone enumeration capabilities.
--
--  Example:
--    $ ./bin/examples/list_all_zones
--    Loading source and listing zones...
--    [OK] Found 417 zones
--    First 10 zones:
--      Africa/Abidjan
--      Africa/Accra
--      ...
--  ===========================================================================

with Ada.Text_IO;
with TZif.API;
with TZif.Domain.Value_Object.Zone_Id;

procedure List_All_Zones is

   use Ada.Text_IO;

   package API renames TZif.API;
   package Load_Port renames API.Load_Port;
   package List_Port renames API.List_Zones_Port;
   package Zone_Id renames TZif.Domain.Value_Object.Zone_Id;

   Max_Display : constant := 10;  --  Limit output for readability

begin
   Put_Line ("======================================================");
   Put_Line ("| List_All_Zones - Enumerate all available timezones |");
   Put_Line ("======================================================");
   New_Line;

   --  First load a source, then list its zones
   declare
      Path        : constant API.Path_String :=
        Load_Port.Path_Strings.To_Bounded_String ("/usr/share/zoneinfo");
      Load_Result : constant API.Load_Source_Result := API.Load_Source (Path);
   begin
      Put_Line ("Loading source and listing zones...");
      New_Line;

      if Load_Port.Load_Source_Result_Package.Is_Ok (Load_Result) then
         declare
            Source      : constant API.Source_Info_Type :=
              Load_Port.Load_Source_Result_Package.Value (Load_Result);
            List_Result : constant API.Zone_List_Result :=
              API.List_All_Zones (Source, Descending => False);
         begin
            if List_Port.List_All_Zones_Result_Package.Is_Ok (List_Result) then
               declare
                  Zones      : constant List_Port.Zone_Id_List :=
                    List_Port.List_All_Zones_Result_Package.Value
                      (List_Result);
                  Zone_Count : constant Natural :=
                    Natural (List_Port.Zone_Id_Vectors.Length (Zones));
                  Count      : Natural := 0;
               begin
                  Put_Line ("[OK] Found" & Natural'Image (Zone_Count)
                            & " zones");
                  New_Line;
                  Put_Line ("First" & Natural'Image (Max_Display) & " zones:");

                  for I in 1 .. Zone_Count loop
                     Count := Count + 1;
                     exit when Count > Max_Display;
                     declare
                        Z : constant Zone_Id.Zone_Id_Type :=
                          List_Port.Zone_Id_Vectors.Unchecked_Element
                            (Zones, I);
                     begin
                        Put_Line ("  " & Zone_Id.To_String (Z));
                     end;
                  end loop;

                  if Zone_Count > Max_Display then
                     Put_Line ("  ... and"
                               & Natural'Image (Zone_Count - Max_Display)
                               & " more");
                  end if;
               end;
            else
               Put_Line ("[ERROR] Failed to list zones");
            end if;
         end;
      else
         Put_Line ("[ERROR] Failed to load source");
      end if;
   end;

   New_Line;
   Put_Line ("Done.");

end List_All_Zones;
