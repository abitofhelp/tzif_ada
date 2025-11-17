pragma Ada_2022;
--  ===========================================================================
--  Validate_Source - Validate timezone data source
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Demonstrates validating a timezone data source directory.
--    Checks for proper tzdata file structure.
--
--  API Signature:
--    function Validate_Source (Path : Validate_Path_String)
--      return Validation_Result;
--
--  Note:
--    This example demonstrates the API signature and purpose.
--    For detailed usage, see: test/integration/test_validate_source.adb
--  ===========================================================================

with Ada.Text_IO;

procedure Validate_Source is
   use Ada.Text_IO;
begin
   Put_Line
     ("============================================"
      & "==========================");
   Put_Line
     ("| Validate_Source - Validate timezone data source         |");
   Put_Line
     ("============================================"
      & "==========================");
   New_Line;

   Put_Line ("API: function Validate_Source (Path : Validate_Path_String)");
   Put_Line ("       return Validation_Result;");
   New_Line;
   Put_Line ("Purpose:");
   Put_Line ("  - Validate timezone data directory structure");
   Put_Line ("  - Check for required TZif files");
   Put_Line ("  - Returns validation errors if any");
   New_Line;
   Put_Line
     ("For detailed usage, see test/integration/test_validate_source.adb");

   New_Line;
   Put_Line ("Done.");
end Validate_Source;
