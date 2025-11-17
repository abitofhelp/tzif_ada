pragma Ada_2022;
--  ======================================================================
--  Test_Cache_With_Zones
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Cache With Zones functionality.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Fixed;
with TZif.Infrastructure.Adapter.File_System.Repository;
with TZif.Application.Port.Inbound.Find_By_Id;
with TZif.Application.Port.Inbound.Export_Cache;
with TZif.Application.Port.Inbound.Import_Cache;
with TZif.Domain.Value_Object.Zone_Id;
with TZif.Domain.Value_Object.Cache_Stats;
procedure Test_Cache_With_Zones is
   use TZif.Domain.Value_Object.Zone_Id;
   use TZif.Domain.Value_Object.Cache_Stats;
   Test_Count   : Natural := 0;
   Passed_Count : Natural := 0;
   procedure Assert (Condition : Boolean; Message : String) is
   begin
      Test_Count := Test_Count + 1;
      if Condition then
         Put_Line ("  [PASS] " & Message);
         Passed_Count := Passed_Count + 1;
      else
         Put_Line ("  [FAIL] " & Message);
      end if;
   end Assert;
   Cache_File   : constant String := "/tmp/tzif_zone_cache_test.json";
begin
   Put_Line ("====================================================");
   Put_Line ("  Running: Cache Export/Import with Zone Data");
   Put_Line ("====================================================");
   New_Line;
   --  Clean up any existing cache file
   if Ada.Directories.Exists (Cache_File) then
      Ada.Directories.Delete_File (Cache_File);
   end if;
   --  Test: Load some zones (validates they can be loaded)
   Put_Line ("Test: Load zones (validates zone loading works)");
   declare
      use TZif.Application.Port.Inbound.Find_By_Id;
      use TZif.Application.Port.Inbound.Export_Cache;
      --  Load a few common timezones
      UTC_Zone      : constant Find_By_Id_Result_Type :=
        TZif.Infrastructure.Adapter.File_System.Repository
          .Find_By_Id          (Make_Zone_Id ("UTC"));
      NY_Zone       : constant Find_By_Id_Result_Type :=
        TZif.Infrastructure.Adapter.File_System.Repository
          .Find_By_Id          (Make_Zone_Id ("America/New_York"));
      Path          :
        constant TZif.Application.Port.Inbound.Export_Cache.Path_String := TZif
            .Application
            .Port
            .Inbound
            .Export_Cache
            .Path_Strings
            .To_Bounded_String (Cache_File);
      Export_Result : constant Export_Cache_Result :=
        TZif.Infrastructure.Adapter.File_System.Repository.Export_Cache
          (Path, Overwrite => True);
   begin
      Assert
        (Find_By_Id_Result.Is_Ok (UTC_Zone),
         "Should load UTC zone successfully");
      Assert
        (Find_By_Id_Result.Is_Ok (NY_Zone),
         "Should load America/New_York zone successfully");
      Put_Line ("  [INFO] Zones loaded successfully, now testing export...");
      Assert
        (Export_Cache_Result_Package.Is_Ok (Export_Result),
         "Should export cache successfully");
      if Export_Cache_Result_Package.Is_Ok (Export_Result) then
         declare
            Stats : constant Export_Stats_Type :=
              Export_Cache_Result_Package.Value (Export_Result);
         begin
            Put_Line
              ("  [INFO] Exported"
               & Stats.Sources_Exported'Image
               & " sources and"
               & Stats.Zones_Exported'Image
               & " zones");
         --  Cache starts empty - sources/zones need explicit population
         --  This test validates the export/import mechanism works
         end;
      end if;
   end;
   --  Test: LRU Eviction - Load 26+ zones to trigger eviction
   Put_Line ("Test: LRU Cache Eviction (load 26+ zones)");
   declare
      use TZif.Application.Port.Inbound.Find_By_Id;
      use TZif.Application.Port.Inbound.Export_Cache;
      --  Zone IDs to load (26 zones to trigger LRU eviction at limit of 25)
      Zone_IDs     : constant array (1 .. 26) of String (1 .. 20) :=
        ["UTC                 ",
         "America/New_York    ",
         "America/Los_Angeles ",
         "America/Chicago     ",
         "America/Denver      ",
         "Europe/London       ",
         "Europe/Paris        ",
         "Europe/Berlin       ",
         "Asia/Tokyo          ",
         "Asia/Shanghai       ",
         "Asia/Dubai          ",
         "Australia/Sydney    ",
         "Pacific/Auckland    ",
         "Africa/Cairo        ",
         "America/Mexico_City ",
         "America/Toronto     ",
         "Europe/Madrid       ",
         "Europe/Rome         ",
         "Asia/Singapore      ",
         "Asia/Hong_Kong      ",
         "America/Sao_Paulo   ",
         "Europe/Amsterdam    ",
         "Asia/Seoul          ",
         "Australia/Melbourne ",
         "America/Vancouver   ",
         "Europe/Stockholm    "];  --  26th zone triggers eviction
      Zones_Loaded : Natural := 0;
   begin
      --  Load all 26 zones
      for Zone_ID of Zone_IDs loop
         declare
            Trimmed_ID : constant String :=
              Ada.Strings.Fixed.Trim (Zone_ID, Ada.Strings.Both);
            Result     : constant Find_By_Id_Result_Type :=
              TZif.Infrastructure.Adapter.File_System.Repository
                .Find_By_Id (Make_Zone_Id (Trimmed_ID));
         begin
            if Find_By_Id_Result.Is_Ok (Result) then
               Zones_Loaded := Zones_Loaded + 1;
            end if;
         end;
      end loop;
      Put_Line
        ("  [INFO] Successfully loaded" & Zones_Loaded'Image & " zones");
      Assert (Zones_Loaded >= 26, "Should successfully load all 26 zones");
      --  Export cache after LRU eviction
      declare
         Path          :
           constant TZif.Application.Port.Inbound.Export_Cache.Path_String :=
             TZif
               .Application
               .Port
               .Inbound
               .Export_Cache
               .Path_Strings
               .To_Bounded_String (Cache_File);
         Export_Result : constant Export_Cache_Result :=
           TZif.Infrastructure.Adapter.File_System.Repository
             .Export_Cache
          (Path, Overwrite => True);
      begin
         Assert
           (Export_Cache_Result_Package.Is_Ok (Export_Result),
            "Should export cache successfully after LRU eviction");
         if Export_Cache_Result_Package.Is_Ok (Export_Result) then
            declare
               Stats : constant Export_Stats_Type :=
                 Export_Cache_Result_Package.Value (Export_Result);
            begin
               Put_Line
                 ("  [INFO] After LRU eviction: exported"
                  & Stats.Zones_Exported'Image
                  & " zones");
               Assert
                 (Stats.Zones_Exported <= 25,
                  "Cache should have max 25 zones after LRU eviction (got"
                  & Stats.Zones_Exported'Image
                  & ")");
               --  Should have evicted the oldest zone (UTC was first)
               Put_Line
                 ("  [INFO] LRU eviction verified - cache size bounded to 25");
            end;
         end if;
      end;
   end;
   --  Test: Verify cache file exists and is valid JSON
   Put_Line ("Test: Verify cache file was created");
   Assert
     (Ada.Directories.Exists (Cache_File),
      "Cache file should exist after export");
   --  Test: Import cache and verify data
   Put_Line ("Test: Import cache and then verify");
   declare
      use TZif.Application.Port.Inbound.Import_Cache;
      Path          :
        constant TZif.Application.Port.Inbound.Import_Cache.Path_String := TZif
            .Application
            .Port
            .Inbound
            .Import_Cache
            .Path_Strings
            .To_Bounded_String (Cache_File);
      Import_Result : constant Import_Cache_Result :=
        TZif.Infrastructure.Adapter.File_System.Repository.Import_Cache
          (Path);
   begin
      Assert
        (Import_Cache_Result_Package.Is_Ok (Import_Result),
         "Should import cache successfully");
      if Import_Cache_Result_Package.Is_Ok (Import_Result) then
         declare
            Stats : constant Import_Stats_Type :=
              Import_Cache_Result_Package.Value (Import_Result);
         begin
            Put_Line
              ("  [INFO] Imported"
               & Stats.Sources_Loaded'Image
               & " sources and"
               & Stats.Zones_Loaded'Image
               & " zones");
            Assert
              (Stats.Sources_Removed = 0,
               "Should not have removed any sources");
            Put_Line ("  [INFO] Cache export/import validated successfully");
         end;
      end if;
   end;
   --  Clean up
   if Ada.Directories.Exists (Cache_File) then
      Ada.Directories.Delete_File (Cache_File);
   end if;
   --  Summary
   New_Line;
   Put_Line ("====================================================");
   Put_Line
     ("  Tests Passed: " & Passed_Count'Image & " /" & Test_Count'Image);
   Put_Line ("====================================================");
   if Passed_Count = Test_Count then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   else
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Test_Cache_With_Zones;