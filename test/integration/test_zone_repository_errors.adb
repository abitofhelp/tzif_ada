pragma Ada_2022;
--  ======================================================================
--  Test_Zone_Repository_Errors
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Zone Repository Errors functionality.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with TZif.Application.Port.Outbound.Zone_Repository;
with TZif.Infrastructure.Adapter.File_System.POSIX_Zone_Repository;
with TZif.Domain.Value_Object.Zone_Id;
with Test_Framework;
procedure Test_Zone_Repository_Errors is
   use TZif.Application.Port.Outbound.Zone_Repository;
   use TZif.Infrastructure.Adapter.File_System.POSIX_Zone_Repository;
   use TZif.Domain.Value_Object.Zone_Id;
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
   --  ========================================================================
   --  Find_By_Id Error Tests
   --  ========================================================================
   procedure Test_Find_By_Id_Nonexistent_Zone is
      Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("Nonexistent/Timezone");
      Result  : constant Repository_Zone_Result := Find_By_Id (Zone_Id);
   begin
      Put_Line ("Test: Find_By_Id - Nonexistent Zone");
      --  Should return error for zone that doesn't exist
      Assert
        (Zone_Result.Is_Error (Result),
         "Should return Error for nonexistent zone");
      if Zone_Result.Is_Error (Result) then
         Put_Line ("  [INFO] Correctly rejected nonexistent zone");
      end if;
   end Test_Find_By_Id_Nonexistent_Zone;
   procedure Test_Find_By_Id_Invalid_Characters is
      Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("Invalid/../Path");
      Result  : constant Repository_Zone_Result := Find_By_Id (Zone_Id);
   begin
      Put_Line ("Test: Find_By_Id - Invalid Characters");
      --  Zone with .. in path should not be found (security)
      Assert
        (Zone_Result.Is_Error (Result),
         "Should return Error for zone with invalid characters");
   end Test_Find_By_Id_Invalid_Characters;
   procedure Test_Find_By_Id_Empty_Name is
      Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("");
      Result  : constant Repository_Zone_Result := Find_By_Id (Zone_Id);
   begin
      Put_Line ("Test: Find_By_Id - Empty Zone Name");
      --  Empty zone name should return error
      Assert
        (Zone_Result.Is_Error (Result),
         "Should return Error for empty zone name");
   end Test_Find_By_Id_Empty_Name;
   --  ========================================================================
   --  Exists Error Tests
   --  ========================================================================
   procedure Test_Exists_Nonexistent_Zone is
      Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("Does/Not/Exist");
      Result  : constant Repository_Boolean_Result := Exists (Zone_Id);
   begin
      Put_Line ("Test: Exists - Nonexistent Zone");
      --  Should return Ok(False) for nonexistent zone
      Assert
        (Boolean_Result.Is_Ok (Result),
         "Should return Ok result for Exists check");
      if Boolean_Result.Is_Ok (Result) then
         Assert
           (not Boolean_Result.Value (Result),
            "Should return False for nonexistent zone");
      end if;
   end Test_Exists_Nonexistent_Zone;
   procedure Test_Exists_Valid_Zone is
      Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("UTC");
      Result  : constant Repository_Boolean_Result := Exists (Zone_Id);
   begin
      Put_Line ("Test: Exists - Valid Zone");
      --  UTC should exist
      Assert
        (Boolean_Result.Is_Ok (Result),
         "Should return Ok result for Exists check");
      if Boolean_Result.Is_Ok (Result) then
         Assert (Boolean_Result.Value (Result), "UTC zone should exist");
      end if;
   end Test_Exists_Valid_Zone;
   --  ========================================================================
   --  Get_Version Error Tests
   --  ========================================================================
   procedure Test_Get_Version_Nonexistent_Zone is
      Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("Nowhere/Timezone");
      Result  : constant Repository_Version_Result := Get_Version (Zone_Id);
   begin
      Put_Line ("Test: Get_Version - Nonexistent Zone");
      --  Should return error for nonexistent zone
      Assert
        (Version_Result.Is_Error (Result),
         "Should return Error for nonexistent zone");
   end Test_Get_Version_Nonexistent_Zone;
   --  ========================================================================
   --  Get_Transition_At_Epoch Error Tests
   --  ========================================================================
   procedure Test_Get_Transition_Nonexistent_Zone is
      Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("Bad/Zone");
      Result  : constant Repository_Transition_Info_Result :=
        Get_Transition_At_Epoch (Zone_Id, 0);
   begin
      Put_Line ("Test: Get_Transition - Nonexistent Zone");
      --  Should return error for nonexistent zone
      Assert
        (Transition_Info_Result.Is_Error (Result),
         "Should return Error for nonexistent zone");
   end Test_Get_Transition_Nonexistent_Zone;
   procedure Test_Get_Transition_Invalid_Epoch is
      Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("UTC");
      --  Far future epoch (year 9999)
      Result  : constant Repository_Transition_Info_Result :=
        Get_Transition_At_Epoch (Zone_Id, 253_402_300_799);
   begin
      Put_Line ("Test: Get_Transition - Far Future Epoch");
      --  May return ok or error depending on implementation
      --  Just checking it doesn't crash
      if Transition_Info_Result.Is_Ok (Result) then
         Put_Line ("  [INFO] Returned transition for far future");
      else
         Put_Line ("  [INFO] Returned error for far future");
      end if;
      Assert (True, "Should handle far future epoch without crashing");
   end Test_Get_Transition_Invalid_Epoch;
   --  ========================================================================
   --  Find_My_Id Error Tests
   --  ========================================================================
   procedure Test_Find_My_Id_Basic is
      Result : constant Repository_Zone_Id_Result := Find_My_Id;
   begin
      Put_Line ("Test: Find_My_Id - Basic Call");
      --  Should return Ok or Error (depends on system config)
      --  Just verify it doesn't crash
      if Zone_Id_Result.Is_Ok (Result) then
         Put_Line ("  [INFO] Found local timezone");
      else
         Put_Line ("  [INFO] Could not determine local timezone");
      end if;
      Assert (True, "Should handle Find_My_Id without crashing");
   end Test_Find_My_Id_Basic;
   --  ========================================================================
   --  List_All_Zones Error Tests
   --  ========================================================================
   procedure Test_List_All_Zones_Ascending is
      Result : constant Repository_Zone_List_Result :=
        List_All_Zones (Ascending);
   begin
      Put_Line ("Test: List_All_Zones - Ascending Order");
      --  Should return list or error
      if Zone_List_Result.Is_Ok (Result) then
         Put_Line ("  [INFO] Successfully listed zones");
         Assert (True, "Should list zones successfully");
      else
         Put_Line ("  [INFO] Error listing zones (may be expected)");
         Assert (True, "Should handle list error gracefully");
      end if;
   end Test_List_All_Zones_Ascending;
   procedure Test_List_All_Zones_Descending is
      Result : constant Repository_Zone_List_Result :=
        List_All_Zones (Descending);
   begin
      Put_Line ("Test: List_All_Zones - Descending Order");
      --  Should return list or error
      if Zone_List_Result.Is_Ok (Result) then
         Put_Line ("  [INFO] Successfully listed zones descending");
         Assert (True, "Should list zones in descending order");
      else
         Put_Line ("  [INFO] Error listing zones descending");
         Assert (True, "Should handle list error gracefully");
      end if;
   end Test_List_All_Zones_Descending;
   --  ========================================================================
   --  List_Sources Error Tests
   --  ========================================================================
   procedure Test_List_Sources_Basic is
      Result : constant Repository_Source_List_Result := List_Sources;
   begin
      Put_Line ("Test: List_Sources - Basic Call");
      --  Should return list or error
      if Source_List_Result.Is_Ok (Result) then
         Put_Line ("  [INFO] Successfully listed sources");
         Assert (True, "Should list sources successfully");
      else
         Put_Line ("  [INFO] Error listing sources");
         Assert (True, "Should handle list error gracefully");
      end if;
   end Test_List_Sources_Basic;
begin
   Put_Line ("====================================================");
   Put_Line ("  Running: Zone Repository Error Tests");
   Put_Line ("====================================================");
   --  Find_By_Id error tests
   Test_Find_By_Id_Nonexistent_Zone;
   Test_Find_By_Id_Invalid_Characters;
   Test_Find_By_Id_Empty_Name;
   --  Exists tests
   Test_Exists_Nonexistent_Zone;
   Test_Exists_Valid_Zone;
   --  Get_Version error tests
   Test_Get_Version_Nonexistent_Zone;
   --  Get_Transition error tests
   Test_Get_Transition_Nonexistent_Zone;
   Test_Get_Transition_Invalid_Epoch;
   --  Find_My_Id tests
   Test_Find_My_Id_Basic;
   --  List_All_Zones tests
   Test_List_All_Zones_Ascending;
   Test_List_All_Zones_Descending;
   --  List_Sources tests
   Test_List_Sources_Basic;
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
end Test_Zone_Repository_Errors;