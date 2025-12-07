pragma Ada_2022;
--  ===========================================================================
--  Discover_Sources - Scan filesystem for timezone sources
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Demonstrates discovering timezone data sources on the filesystem.
--    Scans standard locations for tzdata files.
--
--  Example:
--    $ ./bin/examples/discover_sources
--    Scanning for timezone data sources...
--    [OK] Found 1 source(s)
--    - /usr/share/zoneinfo (2024b, 417 zones)
--  ===========================================================================

with Ada.Text_IO;
with TZif.API;
with TZif.Domain.Value_Object.Source_Info;
with TZif.Domain.Error;

procedure Discover_Sources is

   use Ada.Text_IO;

   package API renames TZif.API;
   package Port renames API.Discover_Port;
   package Source_Info renames TZif.Domain.Value_Object.Source_Info;

begin
   Put_Line ("======================================================");
   Put_Line ("| Discover_Sources - Find timezone data sources      |");
   Put_Line ("======================================================");
   New_Line;

   Put_Line ("Scanning for timezone data sources...");
   New_Line;

   --  Build list of paths to search
   declare
      Paths  : API.Path_List;
      Result : API.Discovery_Result;
   begin
      --  Add common system paths (platform-dependent)
      Paths.Append (Port.Make_Path ("/usr/share/zoneinfo"));
      Paths.Append (Port.Make_Path ("/var/db/timezone/zoneinfo"));

      Result := API.Discover_Sources (Paths);

      if Port.Discovery_Result_Package.Is_Ok (Result) then
         declare
            Data : constant Port.Discovery_Data_Type :=
              Port.Discovery_Result_Package.Value (Result);
         begin
            Put_Line
              ("[OK] Found" & Natural'Image (Natural (Data.Sources.Length))
               & " source(s)");
            New_Line;

            --  Display each discovered source
            for Source of Data.Sources loop
               Put_Line
                 ("  - "
                  & Source_Info.To_String (Source_Info.Get_Path (Source))
                  & " ("
                  & Source_Info.To_String (Source_Info.Get_Version (Source))
                  & ","
                  & Natural'Image (Source_Info.Get_Zone_Count (Source))
                  & " zones)");
            end loop;

            --  Show any non-fatal errors encountered
            if not Data.Errors.Is_Empty then
               New_Line;
               Put_Line
                 ("Warnings:" & Natural'Image (Natural (Data.Errors.Length)));
               for Err of Data.Errors loop
                  Put_Line
                    ("  - "
                     & TZif.Domain.Error.Error_Strings.To_String (Err.Message));
               end loop;
            end if;
         end;
      else
         Put_Line ("[ERROR] Discovery failed");
         declare
            Err : constant API.Error_Type :=
              Port.Discovery_Result_Package.Error_Info (Result);
         begin
            Put_Line
              ("  Error: " & API.Error_Strings.To_String (Err.Message));
         end;
      end if;
   end;

   New_Line;
   Put_Line ("Done.");

end Discover_Sources;
