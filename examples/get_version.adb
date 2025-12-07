pragma Ada_2022;
--  ===========================================================================
--  Get_Version - Query timezone database version
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Demonstrates querying the version of a timezone data source.
--    Shows IANA tzdb version information.
--
--  Example:
--    $ ./bin/examples/get_version
--    Loading source and querying version...
--    [OK] IANA timezone database version: 2024b
--  ===========================================================================

with Ada.Text_IO;
with TZif.API;

procedure Get_Version is

   use Ada.Text_IO;

   package API renames TZif.API;
   package Load_Port renames API.Load_Port;
   package Version_Port renames API.Get_Version_Port;

begin
   Put_Line ("======================================================");
   Put_Line ("| Get_Version - Query timezone database version      |");
   Put_Line ("======================================================");
   New_Line;

   --  First load a source, then query its version
   declare
      Path        : constant API.Path_String :=
        Load_Port.Path_Strings.To_Bounded_String ("/usr/share/zoneinfo");
      Load_Result : constant API.Load_Source_Result := API.Load_Source (Path);
   begin
      Put_Line ("Loading source and querying version...");
      New_Line;

      if Load_Port.Load_Source_Result_Package.Is_Ok (Load_Result) then
         declare
            Source     : constant API.Source_Info_Type :=
              Load_Port.Load_Source_Result_Package.Value (Load_Result);
            Ver_Result : constant API.Version_Result :=
              API.Get_Version (Source);
         begin
            if Version_Port.Version_Result_Package.Is_Ok (Ver_Result) then
               declare
                  Ver : constant API.Version_String :=
                    Version_Port.Version_Result_Package.Value (Ver_Result);
               begin
                  Put_Line
                    ("[OK] IANA timezone database version: "
                     & Version_Port.Version_Strings.To_String (Ver));
               end;
            else
               Put_Line ("[ERROR] Failed to get version");
            end if;
         end;
      else
         Put_Line ("[ERROR] Failed to load source");
         Put_Line ("  Cannot query version without a loaded source");
      end if;
   end;

   New_Line;
   Put_Line ("Done.");

end Get_Version;
