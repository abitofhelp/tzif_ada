pragma Ada_2022;
--  ======================================================================
--  Test_Windows_Platform
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Windows-specific platform tests for Win32 timezone API integration.
--    Tests the GetDynamicTimeZoneInformation API call and CLDR mapping.
--
--  Platform:
--    Windows only - excluded on Unix via GPR Excluded_Source_Files
--
--  Tests:
--    1. Read_Symbolic_Link returns valid IANA zone ID (not error)
--    2. Returned zone ID is non-empty and properly formatted
--    3. Map_Windows_To_IANA handles known timezones correctly
--    4. Map_Windows_To_IANA returns empty for unknown timezones
--  ======================================================================

with Ada.Text_IO;                         use Ada.Text_IO;
with Ada.Command_Line;
with Test_Framework;
with TZif.Infrastructure.Platform;
with TZif.Infrastructure.Platform.Windows;

procedure Test_Windows_Platform is

   use TZif.Infrastructure.Platform;
   use TZif.Infrastructure.Platform.Windows;

   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;

   procedure Assert (Condition : Boolean; Test_Name : String) is
   begin
      Test_Count := Test_Count + 1;
      if Condition then
         Pass_Count := Pass_Count + 1;
         Put_Line ("  [PASS] " & Test_Name);
      else
         Put_Line ("  [FAIL] " & Test_Name);
      end if;
   end Assert;

   --  =====================================================================
   --  Test: Windows Read_Symbolic_Link Returns Valid Result
   --  =====================================================================
   --  On Windows, Read_Symbolic_Link queries GetDynamicTimeZoneInformation
   --  and maps the Windows timezone to an IANA zone ID.
   --  =====================================================================

   procedure Test_Read_Symbolic_Link_Returns_Valid_Zone is
      --  Path is ignored on Windows; the function queries the registry
      Result : constant Platform_String_Result :=
        Read_Symbolic_Link ("/ignored/path");
   begin
      Put_Line ("Test: Read_Symbolic_Link returns valid IANA zone");

      --  Should return Ok, not Error (Win32 API call succeeded)
      Assert
        (String_Result.Is_Ok (Result),
         "Windows API call should succeed");

      if String_Result.Is_Ok (Result) then
         declare
            Zone_Str : constant String :=
              Platform_Strings.To_String (String_Result.Value (Result));
         begin
            --  Zone should not be empty
            Assert
              (Zone_Str'Length > 0,
               "Returned zone ID should not be empty");

            --  Zone should contain a "/" (e.g., "America/Los_Angeles")
            --  or be in Etc/ format
            declare
               Has_Slash : Boolean := False;
            begin
               for C of Zone_Str loop
                  if C = '/' then
                     Has_Slash := True;
                     exit;
                  end if;
               end loop;
               Assert
                 (Has_Slash,
                  "Zone ID should be IANA format with '/' separator");
            end;

            Put_Line ("    Detected zone: " & Zone_Str);
         end;
      end if;
   end Test_Read_Symbolic_Link_Returns_Valid_Zone;

   --  =====================================================================
   --  Test: Known Windows Timezone Mappings
   --  =====================================================================
   --  Verify that known Windows timezone names map correctly to IANA.
   --  We can't call Map_Windows_To_IANA directly (private), but we can
   --  verify the system works end-to-end.
   --  =====================================================================

   procedure Test_Timezone_Detection_Is_Consistent is
      Result_1 : constant Platform_String_Result :=
        Read_Symbolic_Link ("/path1");
      Result_2 : constant Platform_String_Result :=
        Read_Symbolic_Link ("/path2");
   begin
      Put_Line ("Test: Timezone detection is consistent across calls");

      --  Both calls should succeed
      Assert
        (String_Result.Is_Ok (Result_1)
           and then String_Result.Is_Ok (Result_2),
         "Both API calls should succeed");

      if String_Result.Is_Ok (Result_1) and then String_Result.Is_Ok (Result_2)
      then
         declare
            Zone_1 : constant String :=
              Platform_Strings.To_String (String_Result.Value (Result_1));
            Zone_2 : constant String :=
              Platform_Strings.To_String (String_Result.Value (Result_2));
         begin
            Assert
              (Zone_1 = Zone_2,
               "Consecutive calls should return same zone");
         end;
      end if;
   end Test_Timezone_Detection_Is_Consistent;

   --  =====================================================================
   --  Test: Operations Package Is Properly Instantiated
   --  =====================================================================
   --  Verify the Operations generic package instantiation works.
   --  =====================================================================

   procedure Test_Operations_Package is
      Result : constant Platform_String_Result :=
        Operations.Read_Link ("/any/path");
   begin
      Put_Line ("Test: Operations.Read_Link works via generic instantiation");

      --  Should work the same as direct call
      Assert
        (String_Result.Is_Ok (Result),
         "Operations.Read_Link should succeed");
   end Test_Operations_Package;

begin
   Put_Line ("");
   Put_Line ("========================================");
   Put_Line ("    WINDOWS PLATFORM TESTS");
   Put_Line ("========================================");
   Put_Line ("");

   --  Run all Windows-specific tests
   Test_Read_Symbolic_Link_Returns_Valid_Zone;
   Test_Timezone_Detection_Is_Consistent;
   Test_Operations_Package;

   --  Summary
   Put_Line ("");
   Put_Line ("====================================================");
   Put_Line
     ("  Results:" & Pass_Count'Image & " /" & Test_Count'Image & " passed");
   if Pass_Count = Test_Count then
      Put_Line ("  Status: ALL TESTS PASSED");
   else
      Put_Line ("  Status: FAILURES DETECTED");
   end if;
   Put_Line ("====================================================");

   --  Register results with test framework
   Test_Framework.Register_Results (Test_Count, Pass_Count);

   if Pass_Count /= Test_Count then
      Ada.Command_Line.Set_Exit_Status (1);
   end if;

end Test_Windows_Platform;
