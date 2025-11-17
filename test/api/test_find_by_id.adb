pragma Ada_2022;
--  ======================================================================
--  Test_Find_By_Id
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Unit tests for Find By Id functionality.
--
--  ======================================================================

with Ada.Text_IO;
with TZif.API;
with TZif.Domain.Entity.Zone;

procedure Test_Find_By_Id is

   use Ada.Text_IO;
   use TZif.API;

   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;

   procedure Assert (Condition : Boolean; Message : String) is
   begin
      Test_Count := Test_Count + 1;
      if Condition then
         Pass_Count := Pass_Count + 1;
         Put_Line ("  [PASS] " & Message);
      else
         Put_Line ("  [FAIL] " & Message);
      end if;
   end Assert;

   procedure Test_Find_Valid_Zone is
      Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("UTC");
      Result  : constant Zone_Result := Find_Zone (Zone_Id);
   begin
      Put_Line ("");
      Put_Line ("Test: Find_Zone with valid zone (UTC)");

      Assert (Is_Ok (Result),
              "Should successfully find UTC zone");

      if Is_Ok (Result) then
         declare
            Zone : constant TZif.API.Zone_Type := Value (Result);
            Retrieved_Id : constant String :=
              TZif.Domain.Entity.Zone.Get_Id_String (Zone);
         begin
            Assert (Retrieved_Id = "UTC",
                    "Zone ID should match requested ID");
         end;
      end if;
   end Test_Find_Valid_Zone;

   procedure Test_Find_America_New_York is
      Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("America/New_York");
      Result  : constant Zone_Result := Find_Zone (Zone_Id);
   begin
      Put_Line ("");
      Put_Line ("Test: Find_Zone with America/New_York");

      Assert (Is_Ok (Result),
              "Should successfully find America/New_York zone");

      if Is_Ok (Result) then
         declare
            Zone : constant TZif.API.Zone_Type := Value (Result);
            Retrieved_Id : constant String :=
              TZif.Domain.Entity.Zone.Get_Id_String (Zone);
         begin
            Assert (Retrieved_Id = "America/New_York",
                    "Zone ID should match requested ID");

            --  America/New_York has DST transitions
            Assert (TZif.Domain.Entity.Zone.Transition_Count (Zone) > 0,
                    "Should have transition data for DST zone");
         end;
      end if;
   end Test_Find_America_New_York;

   procedure Test_Find_Nonexistent_Zone is
      Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("Invalid/Nonexistent");
      Result  : constant Zone_Result := Find_Zone (Zone_Id);
   begin
      Put_Line ("");
      Put_Line ("Test: Find_Zone with nonexistent zone");

      Assert (Is_Error (Result),
              "Should return error for nonexistent zone");

      if Is_Error (Result) then
         Put_Line ("    Error returned as expected");
      end if;
   end Test_Find_Nonexistent_Zone;

   procedure Test_Find_Europe_London is
      Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("Europe/London");
      Result  : constant Zone_Result := Find_Zone (Zone_Id);
   begin
      Put_Line ("");
      Put_Line ("Test: Find_Zone with Europe/London");

      Assert (Is_Ok (Result),
              "Should successfully find Europe/London zone");

      if Is_Ok (Result) then
         declare
            Zone : constant TZif.API.Zone_Type := Value (Result);
         begin
            Assert (TZif.Domain.Entity.Zone.Get_Id_String (Zone) =
                      "Europe/London",
                    "Zone ID should match");
            Assert (TZif.Domain.Entity.Zone.Transition_Count (Zone) > 0,
                    "Should have transition data");
         end;
      end if;
   end Test_Find_Europe_London;

   procedure Report_Results is
      Pass_Rate : constant Natural :=
        (if Test_Count > 0 then
           Natural ((Float (Pass_Count) / Float (Test_Count)) * 100.0)
         else 0);
   begin
      Put_Line ("");
      Put_Line ("========================================================");
      Put_Line ("  Test Results:");
      Put_Line ("  Total:  " & Test_Count'Image);
      Put_Line ("  Passed: " & Pass_Count'Image);
      Put_Line ("  Failed: " & Natural'Image (Test_Count - Pass_Count));
      Put_Line ("  Rate:   " & Pass_Rate'Image & "%");
      Put_Line ("========================================================");

      if Pass_Count = Test_Count then
         Put_Line ("  [SUCCESS] All tests passed!");
      else
         Put_Line ("  [FAILURE] Some tests failed");
      end if;
   end Report_Results;

begin
   Put_Line ("========================================================");
   Put_Line ("  TZif API Test: Find_Zone");
   Put_Line ("  Testing TZif.API.Find_Zone");
   Put_Line ("========================================================");

   Test_Find_Valid_Zone;
   Test_Find_America_New_York;
   Test_Find_Europe_London;
   Test_Find_Nonexistent_Zone;

   Report_Results;

end Test_Find_By_Id;
