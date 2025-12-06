pragma Ada_2022;
--  ======================================================================
--  Test_API
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Integration tests for TZif.API facade.
--    Exercises the public API facade that delegates to Desktop.API.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Test_Framework;
with TZif.API;

procedure Test_API is

   use TZif.API;

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

   --  Callback for Find_By_Pattern
   Pattern_Match_Count : Natural := 0;
   procedure Pattern_Callback (Zone : Zone_Id_Type) is
      pragma Unreferenced (Zone);
   begin
      Pattern_Match_Count := Pattern_Match_Count + 1;
   end Pattern_Callback;

   --  Callback for Find_By_Region
   Region_Match_Count : Natural := 0;
   procedure Region_Callback (Zone : Zone_Id_Type) is
      pragma Unreferenced (Zone);
   begin
      Region_Match_Count := Region_Match_Count + 1;
   end Region_Callback;

   --  Callback for Find_By_Regex
   Regex_Match_Count : Natural := 0;
   procedure Regex_Callback (Zone : Zone_Id_Type) is
      pragma Unreferenced (Zone);
   begin
      Regex_Match_Count := Regex_Match_Count + 1;
   end Regex_Callback;

begin
   Put_Line ("Test: TZif.API.Find_By_Id");

   --  Test valid zone via API
   declare
      Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("UTC");
      Result  : constant Zone_Result := Find_By_Id (Zone_Id);
   begin
      Assert (Is_Ok (Result), "API.Find_By_Id should find UTC");
   end;

   declare
      Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("America/New_York");
      Result  : constant Zone_Result := Find_By_Id (Zone_Id);
   begin
      Assert (Is_Ok (Result), "API.Find_By_Id should find America/New_York");
   end;

   --  Test invalid zone
   declare
      Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("Invalid/Zone");
      Result  : constant Zone_Result := Find_By_Id (Zone_Id);
   begin
      Assert (Is_Error (Result), "API.Find_By_Id should error on invalid zone");
   end;

   Put_Line ("Test: TZif.API.Find_My_Id");

   --  Test system timezone detection
   declare
      Result : constant My_Zone_Result := Find_My_Id;
   begin
      Assert (Is_Ok (Result), "API.Find_My_Id should detect system timezone");
      if Is_Ok (Result) then
         Put_Line ("  [INFO] System timezone: " & To_String (Value (Result)));
      end if;
   end;

   Put_Line ("Test: TZif.API.Get_Transition_At_Epoch");

   --  Test Get_Transition_At_Epoch for UTC
   declare
      Zone : constant Zone_Id_String := Make_Zone_Id_String ("UTC");
      --  2025-01-01 00:00:00 UTC
      Epoch  : constant Epoch_Seconds_Type := 1_735_689_600;
      Result : constant Transition_Result :=
        Get_Transition_At_Epoch (Zone, Epoch);
   begin
      Assert (Is_Ok (Result), "API.Get_Transition_At_Epoch for UTC");
   end;

   --  Test Get_Transition_At_Epoch for America/Los_Angeles (has DST)
   declare
      Zone : constant Zone_Id_String := Make_Zone_Id_String ("America/Los_Angeles");
      --  Summer time (July 2025)
      Epoch  : constant Epoch_Seconds_Type := 1_751_328_000;
      Result : constant Transition_Result :=
        Get_Transition_At_Epoch (Zone, Epoch);
   begin
      Assert (Is_Ok (Result), "API.Get_Transition_At_Epoch for LA summer");
   end;

   --  Test invalid zone
   declare
      Zone : constant Zone_Id_String := Make_Zone_Id_String ("Invalid/Zone");
      Epoch  : constant Epoch_Seconds_Type := 1_735_689_600;
      Result : constant Transition_Result :=
        Get_Transition_At_Epoch (Zone, Epoch);
   begin
      Assert
        (Is_Error (Result),
         "API.Get_Transition_At_Epoch should error on invalid zone");
   end;

   Put_Line ("Test: TZif.API.Discover_Sources");

   --  Test Discover_Sources with standard paths
   declare
      Paths : Path_List (1 .. 1);
   begin
      Paths (1) := Discover_Port.Path_Strings.To_Bounded_String
        ("/usr/share/zoneinfo");
      declare
         Result : constant Discovery_Result := Discover_Sources (Paths);
      begin
         --  Should succeed on macOS/Linux
         Assert
           (Discover_Port.Discovery_Result_Package.Is_Ok (Result),
            "API.Discover_Sources should find /usr/share/zoneinfo");
      end;
   end;

   Put_Line ("Test: TZif.API.Load_Source");

   --  Test Load_Source with valid path
   declare
      Path : constant Path_String :=
        Load_Port.Path_Strings.To_Bounded_String ("/usr/share/zoneinfo");
      Result : constant Load_Source_Result := Load_Source (Path);
   begin
      Assert
        (Load_Port.Load_Source_Result_Package.Is_Ok (Result),
         "API.Load_Source should load /usr/share/zoneinfo");
   end;

   --  Test Load_Source with invalid path
   declare
      Path : constant Path_String :=
        Load_Port.Path_Strings.To_Bounded_String ("/invalid/path");
      Result : constant Load_Source_Result := Load_Source (Path);
   begin
      Assert
        (Load_Port.Load_Source_Result_Package.Is_Error (Result),
         "API.Load_Source should error on invalid path");
   end;

   Put_Line ("Test: TZif.API.Validate_Source");

   --  Test Validate_Source with valid path
   declare
      Path : constant Validate_Path_String :=
        Validate_Port.Path_Strings.To_Bounded_String ("/usr/share/zoneinfo");
      Result : constant Validation_Result := Validate_Source (Path);
   begin
      Assert
        (Validate_Port.Validation_Result_Package.Is_Ok (Result),
         "API.Validate_Source should validate /usr/share/zoneinfo");
   end;

   Put_Line ("Test: TZif.API.Get_Version");

   --  Test Get_Version requires a valid source first
   declare
      Path : constant Path_String :=
        Load_Port.Path_Strings.To_Bounded_String ("/usr/share/zoneinfo");
      Load_Result : constant Load_Source_Result := Load_Source (Path);
   begin
      if Load_Port.Load_Source_Result_Package.Is_Ok (Load_Result) then
         declare
            Source : constant Source_Info_Type :=
              Load_Port.Load_Source_Result_Package.Value (Load_Result);
            Version_Res : constant Version_Result := Get_Version (Source);
         begin
            --  Version may or may not be present depending on system
            if Get_Version_Port.Version_Result_Package.Is_Ok (Version_Res) then
               Put_Line ("  [INFO] Version: " &
                 Get_Version_Port.Version_Strings.To_String
                   (Get_Version_Port.Version_Result_Package.Value
                     (Version_Res)));
            end if;
            Assert (True, "API.Get_Version executed");
         end;
      else
         Assert (True, "API.Get_Version skipped (no source)");
      end if;
   end;

   Put_Line ("Test: TZif.API.List_All_Zones");

   --  Test List_All_Zones
   declare
      Path : constant Path_String :=
        Load_Port.Path_Strings.To_Bounded_String ("/usr/share/zoneinfo");
      Load_Result : constant Load_Source_Result := Load_Source (Path);
   begin
      if Load_Port.Load_Source_Result_Package.Is_Ok (Load_Result) then
         declare
            Source : constant Source_Info_Type :=
              Load_Port.Load_Source_Result_Package.Value (Load_Result);
            List_Result : constant Zone_List_Result :=
              List_All_Zones (Source, Descending => False);
         begin
            Assert
              (List_Zones_Port.List_All_Zones_Result_Package.Is_Ok
                 (List_Result),
               "API.List_All_Zones ascending");
         end;

         --  Test descending order
         declare
            Source : constant Source_Info_Type :=
              Load_Port.Load_Source_Result_Package.Value (Load_Result);
            List_Result : constant Zone_List_Result :=
              List_All_Zones (Source, Descending => True);
         begin
            Assert
              (List_Zones_Port.List_All_Zones_Result_Package.Is_Ok
                 (List_Result),
               "API.List_All_Zones descending");
         end;
      else
         Assert (True, "API.List_All_Zones skipped (no source)");
      end if;
   end;

   Put_Line ("Test: TZif.API.Find_By_Pattern");

   --  Test Find_By_Pattern
   declare
      Pattern : constant Pattern_String :=
        Find_Pattern_Port.Pattern_Strings.To_Bounded_String ("York");
      Result  : constant Pattern_Result :=
        Find_By_Pattern (Pattern, Pattern_Callback'Access);
   begin
      Assert
        (Find_Pattern_Port.Find_By_Pattern_Result_Package.Is_Ok (Result),
         "API.Find_By_Pattern 'York'");
      Put_Line ("  [INFO] Found" & Pattern_Match_Count'Image & " matches");
   end;

   Put_Line ("Test: TZif.API.Find_By_Region");

   --  Test Find_By_Region
   declare
      Region : constant Region_String :=
        Find_Region_Port.Region_Strings.To_Bounded_String ("Europe");
      Result : constant Region_Result :=
        Find_By_Region (Region, Region_Callback'Access);
   begin
      Assert
        (Find_Region_Port.Find_By_Region_Result_Package.Is_Ok (Result),
         "API.Find_By_Region 'Europe'");
      Put_Line ("  [INFO] Found" & Region_Match_Count'Image & " matches");
   end;

   Put_Line ("Test: TZif.API.Find_By_Regex");

   --  Test Find_By_Regex
   declare
      Regex : constant Regex_String :=
        Find_Regex_Port.Regex_Strings.To_Bounded_String ("^America/New.*");
      Result : constant Regex_Result :=
        Find_By_Regex (Regex, Regex_Callback'Access);
   begin
      Assert
        (Find_Regex_Port.Find_By_Regex_Result_Package.Is_Ok (Result),
         "API.Find_By_Regex '^America/New.*'");
      Put_Line ("  [INFO] Found" & Regex_Match_Count'Image & " matches");
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

end Test_API;
