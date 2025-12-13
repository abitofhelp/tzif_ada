pragma Ada_2022;
--  ======================================================================
--  Test_Zone_Id
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Zone Id functionality.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Test_Framework;
with TZif.Domain.Value_Object.Zone_Id;
with TZif.Domain.Value_Object.Zone_Id.Result;
procedure Test_Zone_Id is
   use TZif.Domain.Value_Object.Zone_Id;
   --  Type alias for cleaner code
   package Zone_Result renames TZif.Domain.Value_Object.Zone_Id.Result;
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
begin
   Put_Line ("Test: Zone ID Creation");
   --  Test Make_Zone_Id
   declare
      Zone : constant Zone_Id_Type := Make_Zone_Id ("America/New_York");
   begin
      Assert
        (To_String (Zone) = "America/New_York",
         "Make_Zone_Id creates valid zone");
      Assert (Length (Zone) = 16, "Zone ID length is correct");
      Assert (not Is_Empty (Zone), "Zone ID is not empty");
   end;
   --  Test UTC special zone
   declare
      Zone : constant Zone_Id_Type := Make_Zone_Id ("UTC");
   begin
      Assert (To_String (Zone) = "UTC", "UTC zone created correctly");
      Assert (Matches (Zone, "UTC"), "Matches function works");
   end;
   Put_Line ("Test: Zone ID Validation");
   --  Test valid zone IDs
   declare
      Result : Zone_Result.Result;
   begin
      Result := Zone_Result.Make_Zone_Id ("America/Los_Angeles");
      Assert (Zone_Result.Is_Ok (Result),
             "Valid zone ID passes validation");
      if Zone_Result.Is_Ok (Result) then
         declare
            Zone : constant Zone_Id_Type := Zone_Result.Value (Result);
         begin
            Assert
              (To_String (Zone) = "America/Los_Angeles",
               "Validated zone has correct value");
         end;
      end if;
   end;
   --  Test empty zone ID (should fail validation)
   declare
      Result : constant Zone_Result.Result := Zone_Result.Make_Zone_Id ("");
   begin
      Assert (Zone_Result.Is_Error (Result),
             "Empty zone ID fails validation");
   end;
   --  Test very long zone ID (beyond max length)
   declare
      Long_Id : constant String (1 .. 100) := [others => 'A'];
      Result : constant Zone_Result.Result :=
        Zone_Result.Make_Zone_Id (Long_Id);
   begin
      Assert
        (Zone_Result.Is_Error (Result),
         "Overly long zone ID fails validation");
   end;
   Put_Line ("Test: Zone ID Comparison");
   --  Test equality
   declare
      Zone1 : constant Zone_Id_Type := Make_Zone_Id ("Europe/London");
      Zone2 : constant Zone_Id_Type := Make_Zone_Id ("Europe/London");
      Zone3 : constant Zone_Id_Type := Make_Zone_Id ("Asia/Tokyo");
   begin
      Assert (Zone1 = Zone2, "Equal zones compare equal");
      Assert (not (Zone1 = Zone3), "Different zones are not equal");
   end;
   --  Test less than (ordering)
   declare
      Zone_A : constant Zone_Id_Type := Make_Zone_Id ("America/New_York");
      Zone_B : constant Zone_Id_Type := Make_Zone_Id ("Europe/London");
   begin
      Assert (Zone_A < Zone_B, "America/New_York < Europe/London");
      Assert (not (Zone_B < Zone_A), "Europe/London not < America/New_York");
   end;
   Put_Line ("Test: Zone ID Edge Cases");
   --  Test empty zone ID
   declare
      Empty : constant Zone_Id_Type := Make_Zone_Id ("");
   begin
      Assert (Is_Empty (Empty), "Empty zone ID is recognized");
      Assert (Length (Empty) = 0, "Empty zone has zero length");
   end;
   --  Test truncation
   declare
      Long_Id   : constant String (1 .. 100) := [others => 'X'];
      Truncated : constant Zone_Id_Type := Make_Zone_Id_Truncate (Long_Id);
   begin
      Assert
        (Length (Truncated) <= 64, "Truncated zone ID respects max length");
   end;
   --  Summary
   Put_Line ("====================================================");
   Put_Line
     ("  Results:" & Pass_Count'Image & " /" & Test_Count'Image & " passed");
   if Pass_Count = Test_Count then
      Put_Line ("  Status: ALL TESTS PASSED");
   else
      Put_Line ("  Status: FAILURES DETECTED");
   end if;
   Put_Line ("====================================================");
   Test_Framework.Register_Results (Test_Count, Pass_Count);
   if Pass_Count /= Test_Count then
      Ada.Command_Line.Set_Exit_Status (1);
   end if;
end Test_Zone_Id;