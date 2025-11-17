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
--  API Signature:
--    function Load_Source (Path : Path_String)
--      return Load_Source_Result;
--
--  Note:
--    This example demonstrates the API signature and purpose.
--    For detailed usage, see: test/integration/test_load_source.adb
--  ===========================================================================

with Ada.Text_IO;

procedure Load_Source is
   use Ada.Text_IO;
begin
   Put_Line
     ("============================================"
      & "==========================");
   Put_Line
     ("| Load_Source - Load timezone data source                 |");
   Put_Line
     ("============================================"
      & "==========================");
   New_Line;

   Put_Line ("API: function Load_Source (Path : Path_String)");
   Put_Line ("       return Load_Source_Result;");
   New_Line;
   Put_Line ("Purpose:");
   Put_Line ("  - Load timezone data from a specific directory");
   Put_Line ("  - Returns Source_Info_Type on success");
   Put_Line ("  - Source_Info used with Get_Version, List_All_Zones");
   New_Line;
   Put_Line ("Typical paths:");
   Put_Line ("  - /usr/share/zoneinfo");
   New_Line;
   Put_Line ("For detailed usage, see test/integration/test_load_source.adb");

   New_Line;
   Put_Line ("Done.");
end Load_Source;
