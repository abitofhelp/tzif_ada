pragma Ada_2022;
--  ===========================================================================
--  Load_Source - Load timezone data source
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Demonstrates loading a timezone data source from a directory.
--    Returns Source_Info_Type for use with other operations.
--
--  Example:
--    $ ./bin/examples/load_source
--    Loading timezone data from /usr/share/zoneinfo...
--    [OK] Source loaded: 2024b with 417 zones
--  ===========================================================================

with Ada.Text_IO;
with TZif.API;
with TZif.Domain.Value_Object.Source_Info;

procedure Load_Source is

   use Ada.Text_IO;

   package API renames TZif.API;
   package Port renames API.Load_Port;
   package Source_Info renames TZif.Domain.Value_Object.Source_Info;

begin
   Put_Line ("======================================================");
   Put_Line ("| Load_Source - Load timezone data source            |");
   Put_Line ("======================================================");
   New_Line;

   --  Try to load the system timezone data
   declare
      Path   : constant API.Path_String :=
        Port.Path_Strings.To_Bounded_String ("/usr/share/zoneinfo");
      Result : constant API.Load_Source_Result := API.Load_Source (Path);
   begin
      Put_Line
        ("Loading timezone data from "
         & Port.Path_Strings.To_String (Path) & "...");
      New_Line;

      if Port.Load_Source_Result_Package.Is_Ok (Result) then
         declare
            Source : constant API.Source_Info_Type :=
              Port.Load_Source_Result_Package.Value (Result);
         begin
            Put_Line ("[OK] Source loaded successfully");
            Put_Line
              ("  - Version: "
               & Source_Info.To_String (Source_Info.Get_Version (Source)));
            Put_Line
              ("  - Zone count:"
               & Natural'Image (Source_Info.Get_Zone_Count (Source)));
            Put_Line
              ("  - Path: "
               & Source_Info.To_String (Source_Info.Get_Path (Source)));
         end;
      else
         Put_Line ("[ERROR] Failed to load source");
         declare
            Err : constant API.Error_Type :=
              Port.Load_Source_Result_Package.Error_Info (Result);
         begin
            Put_Line ("  Error: " & API.Error_Strings.To_String (Err.Message));
         end;
      end if;
   end;

   New_Line;
   Put_Line ("Done.");

end Load_Source;
