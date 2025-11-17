pragma Ada_2022;
--  ======================================================================
--  Test_Json_Serialization
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Json Serialization functionality.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with GNATCOLL.JSON;
with TZif.Infrastructure.Cache.JSON_Serialization;
with TZif.Domain.Value_Object.Source_Info;
with TZif.Domain.Value_Object.Timezone_Type;
with TZif.Domain.Value_Object.Transition;
with TZif.Domain.Value_Object.Epoch_Seconds;
with TZif.Domain.Value_Object.UTC_Offset;
with TZif.Domain.TZif_Data;
procedure Test_JSON_Serialization is
   use GNATCOLL.JSON;
   use TZif.Infrastructure.Cache.JSON_Serialization;
   use TZif.Domain.Value_Object.Source_Info;
   use TZif.Domain.Value_Object.Timezone_Type;
   use TZif.Domain.Value_Object.Transition;
   use TZif.Domain.Value_Object.Epoch_Seconds;
   use TZif.Domain.Value_Object.UTC_Offset;
   use TZif.Domain.TZif_Data;
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
   --  Test: Source_Info Round-Trip
   --  ========================================================================
   procedure Test_Source_Info_Round_Trip is
      ULID1 : constant ULID_Type :=
        Make_ULID ("01HQXY123456789ABCDEFGHIJK");
      Path1    : constant Path_String_Type := Make_Path ("/tmp/tzdata/2024b");
      Version1 : constant Version_String_Type := Make_Version ("2024b");
      Original : constant Source_Info_Type := Make_Source_Info
          (ULID => ULID1,
           Path => Path1,
           Version => Version1,
           Zone_Count => 599);
      JSON_Obj : constant JSON_Value := To_JSON (Original);
      Restored : constant Source_Info_Type := From_JSON (JSON_Obj);
   begin
      Put_Line ("Test: Source_Info round-trip");
      Assert
        (To_String (Get_ULID (Restored)) = To_String (Get_ULID (Original)),
         "ULID should be preserved");
      Assert
        (To_String (Get_Path (Restored)) = To_String (Get_Path (Original)),
         "Path should be preserved");
      Assert
        (To_String (Get_Version (Restored))
         = To_String (Get_Version (Original)),
         "Version should be preserved");
      Assert
        (Get_Zone_Count (Restored) = Get_Zone_Count (Original),
         "Zone count should be preserved");
   end Test_Source_Info_Round_Trip;
   --  ========================================================================
   --  Test: Transition Round-Trip
   --  ========================================================================
   procedure Test_Transition_Round_Trip is
      Original : constant Transition_Type :=
        (Time => 1609459200, Type_Index => 1);
      JSON_Obj : constant JSON_Value := To_JSON (Original);
      Restored : constant Transition_Type := From_JSON (JSON_Obj);
   begin
      Put_Line ("Test: Transition round-trip");
      Assert (Restored.Time = Original.Time, "Time should be preserved");
      Assert
        (Restored.Type_Index = Original.Type_Index,
         "Type_Index should be preserved");
   end Test_Transition_Round_Trip;
   --  ========================================================================
   --  Test: Timezone_Type Round-Trip
   --  ========================================================================
   procedure Test_Timezone_Type_Round_Trip is
      Original : constant Timezone_Type_Record := Make_Timezone_Type
          (UTC_Offset => -28800, Is_DST => False, Abbreviation => "PST");
      JSON_Obj : constant JSON_Value := To_JSON (Original);
      Restored : constant Timezone_Type_Record := From_JSON (JSON_Obj);
   begin
      Put_Line ("Test: Timezone_Type round-trip");
      Assert
        (Restored.UTC_Offset = Original.UTC_Offset,
         "UTC offset should be preserved");
      Assert
        (Restored.Is_DST = Original.Is_DST, "DST flag should be preserved");
      Assert
        (Get_Abbreviation (Restored) = Get_Abbreviation (Original),
         "Abbreviation should be preserved");
   end Test_Timezone_Type_Round_Trip;
   --  ========================================================================
   --  Test: TZif_Data Empty Round-Trip
   --  ========================================================================
   procedure Test_TZif_Data_Empty is
      Original : TZif_Data_Type;  --  Default initialization
      JSON_Obj : constant JSON_Value := To_JSON (Original);
      Restored : constant TZif_Data_Type := From_JSON (JSON_Obj);
   begin
      Put_Line ("Test: Empty TZif_Data round-trip");
      Assert
        (Restored.Transitions.Is_Empty,
         "Empty transitions should be preserved");
      Assert
        (Restored.Timezone_Types.Is_Empty, "Empty types should be preserved");
      Assert
        (Restored.Leap_Seconds.Is_Empty,
         "Empty leap seconds should be preserved");
   end Test_TZif_Data_Empty;
   --  ========================================================================
   --  Test: TZif_Data With Data Round-Trip
   --  ========================================================================
   procedure Test_TZif_Data_With_Data is
      Original : TZif_Data_Type;
      TZ_Type  : constant Timezone_Type_Record := Make_Timezone_Type
          (UTC_Offset => 0, Is_DST => False, Abbreviation => "UTC");
      Trans    : constant Transition_Type := (Time => 0, Type_Index => 0);
      JSON_Obj : JSON_Value;
      Restored : TZif_Data_Type;
   begin
      Put_Line ("Test: TZif_Data with data round-trip");
      --  Add test data
      Original.Timezone_Types.Append (TZ_Type);
      Original.Transitions.Append (Trans);
      JSON_Obj := To_JSON (Original);
      Restored := From_JSON (JSON_Obj);
      Assert
        (Natural (Restored.Timezone_Types.Length)
         = Natural (Original.Timezone_Types.Length),
         "Type count should be preserved");
      Assert
        (Natural (Restored.Transitions.Length)
         = Natural (Original.Transitions.Length),
         "Transition count should be preserved");
   end Test_TZif_Data_With_Data;
   --  ========================================================================
   --  Test: Cache_Header Round-Trip
   --  ========================================================================
   procedure Test_Cache_Header_Round_Trip is
      Original : constant Cache_Header := (Magic => "TZIF_CACHE",
         Version => 2,
         Platform => "darwin    ",
         Library_Version => "0.1.0     ",
         Expected_Sources => 10,
         Expected_Zones => 599,
         Actual_Sources => 10,
         Actual_Zones => 599);
      JSON_Obj : constant JSON_Value := To_JSON (Original);
      Restored : constant Cache_Header := From_JSON (JSON_Obj);
   begin
      Put_Line ("Test: Cache_Header round-trip");
      Assert (Restored.Magic = Original.Magic, "Magic should be preserved");
      Assert
        (Restored.Version = Original.Version, "Version should be preserved");
      Assert
        (Restored.Expected_Sources = Original.Expected_Sources,
         "Expected_Sources should be preserved");
      Assert
        (Restored.Expected_Zones = Original.Expected_Zones,
         "Expected_Zones should be preserved");
      Assert
        (Restored.Actual_Sources = Original.Actual_Sources,
         "Actual_Sources should be preserved");
      Assert
        (Restored.Actual_Zones = Original.Actual_Zones,
         "Actual_Zones should be preserved");
   end Test_Cache_Header_Round_Trip;
begin
   Put_Line ("===================================================");
   Put_Line ("  Running: JSON Serialization Tests");
   Put_Line ("===================================================");
   New_Line;
   Test_Source_Info_Round_Trip;
   Test_Transition_Round_Trip;
   Test_Timezone_Type_Round_Trip;
   Test_TZif_Data_Empty;
   Test_TZif_Data_With_Data;
   Test_Cache_Header_Round_Trip;
   New_Line;
   Put_Line ("Tests Passed: " & Pass_Count'Image & " /" & Test_Count'Image);
   if Pass_Count = Test_Count then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   else
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Test_JSON_Serialization;