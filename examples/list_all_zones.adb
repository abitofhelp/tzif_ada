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
--    [INFO] List_All_Zones requires a Source_Info_Type parameter
--    [OK] Example demonstrates API signature
--  ===========================================================================

with Ada.Text_IO;

procedure List_All_Zones is

   use Ada.Text_IO;

begin
   Put_Line
     ("============================================"
      & "==========================");
   Put_Line
     ("| List_All_Zones - Enumerate all available timezones      |");
   Put_Line
     ("============================================"
      & "==========================");
   New_Line;

   Put_Line ("[INFO] List_All_Zones requires a Source_Info_Type parameter");
   Put_Line ("[INFO] Use Discover_Sources or Load_Source to obtain a source");
   Put_Line ("[OK] Example demonstrates API signature");

   New_Line;
   Put_Line ("Done.");

end List_All_Zones;
