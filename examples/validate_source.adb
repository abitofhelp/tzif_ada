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
--  Example:
--    $ ./bin/examples/validate_source
--    Validating timezone source at /usr/share/zoneinfo...
--    [OK] Source is valid
--  ===========================================================================

with Ada.Text_IO;
with TZif.API;

procedure Validate_Source is

   use Ada.Text_IO;

   package API renames TZif.API;
   package Port renames API.Validate_Port;

begin
   Put_Line ("======================================================");
   Put_Line ("| Validate_Source - Validate timezone data source    |");
   Put_Line ("======================================================");
   New_Line;

   --  Validate a valid source
   declare
      Path   : constant API.Validate_Path_String :=
        Port.Path_Strings.To_Bounded_String ("/usr/share/zoneinfo");
      Result : constant API.Validation_Result := API.Validate_Source (Path);
   begin
      Put_Line
        ("Validating source at "
         & Port.Path_Strings.To_String (Path) & "...");
      New_Line;

      if Port.Validation_Result_Package.Is_Ok (Result) then
         Put_Line ("[OK] Source is valid");
      else
         Put_Line ("[ERROR] Source validation failed");
         declare
            Err : constant API.Error_Type :=
              Port.Validation_Result_Package.Error_Info (Result);
         begin
            Put_Line ("  Error: " & API.Error_Strings.To_String (Err.Message));
         end;
      end if;
   end;

   New_Line;

   --  Validate an invalid path
   declare
      Path   : constant API.Validate_Path_String :=
        Port.Path_Strings.To_Bounded_String ("/nonexistent/path");
      Result : constant API.Validation_Result := API.Validate_Source (Path);
   begin
      Put_Line
        ("Validating invalid path "
         & Port.Path_Strings.To_String (Path) & "...");
      New_Line;

      if Port.Validation_Result_Package.Is_Ok (Result) then
         --  Result is Ok, but check the Boolean value
         if Port.Validation_Result_Package.Value (Result) then
            Put_Line ("[UNEXPECTED] Invalid path validated as valid");
         else
            Put_Line ("[EXPECTED] Source correctly identified as invalid");
         end if;
      else
         Put_Line ("[EXPECTED] Validation returned error for invalid path");
      end if;
   end;

   New_Line;
   Put_Line ("Done.");

end Validate_Source;
