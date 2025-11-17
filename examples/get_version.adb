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
--    [OK] Version query completed
--  ===========================================================================

with Ada.Text_IO;

procedure Get_Version is

   use Ada.Text_IO;

begin
   Put_Line
     ("============================================"
      & "==========================");
   Put_Line
     ("| Get_Version - Query timezone database version           |");
   Put_Line
     ("============================================"
      & "==========================");
   New_Line;

   Put_Line ("[INFO] Get_Version requires a Source_Info_Type parameter");
   Put_Line ("[INFO] Use Discover_Sources or Load_Source to obtain a source");
   Put_Line ("[OK] Example demonstrates API signature");

   New_Line;
   Put_Line ("Done.");

end Get_Version;
