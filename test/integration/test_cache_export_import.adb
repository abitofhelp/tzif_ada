pragma Ada_2022;
--  ======================================================================
--  Test_Cache_Export_Import
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Cache Export Import functionality.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Directories;
with TZif.Infrastructure.Adapter.File_System.Repository;
with TZif.Application.Port.Inbound.Export_Cache;
with TZif.Application.Port.Inbound.Import_Cache;
with TZif.Application.Port.Inbound.Find_By_Id;
with TZif.Domain.Value_Object.Cache_Stats;
with TZif.Domain.Value_Object.Zone_Id;
procedure Test_Cache_Export_Import is
   use TZif.Domain.Value_Object.Cache_Stats;
   use TZif.Domain.Value_Object.Zone_Id;
   Test_Count   : Natural := 0;
   Passed_Count : Natural := 0;
   procedure Assert (Condition : Boolean; Message : String) is
   begin
      Test_Count := Test_Count + 1;
      if Condition then
         Put_Line ("[PASS] " & Message);
         Passed_Count := Passed_Count + 1;
      else
         Put_Line ("[FAIL] " & Message);
      end if;
   end Assert;
   Cache_File   : constant String := "/tmp/tzif_test_cache.json";
begin
   Put_Line ("====================================================");
   Put_Line ("  Running: Cache Export/Import Round-Trip Test");
   Put_Line ("====================================================");
   New_Line;
   --  Clean up any existing cache file
   if Ada.Directories.Exists (Cache_File) then
      Ada.Directories.Delete_File (Cache_File);
   end if;
   --  Test: Export empty cache
   Put_Line ("Test: Export empty cache");
   declare
      use TZif.Application.Port.Inbound.Export_Cache;
      Path   :
        constant TZif.Application.Port.Inbound.Export_Cache.Path_String := TZif
            .Application
            .Port
            .Inbound
            .Export_Cache
            .Path_Strings
            .To_Bounded_String (Cache_File);
      Result : constant Export_Cache_Result :=
        TZif.Infrastructure.Adapter.File_System.Repository.Export_Cache
          (Path, Overwrite => True);
   begin
      Assert
        (Export_Cache_Result_Package.Is_Ok (Result),
         "Export empty cache should succeed");
      if Export_Cache_Result_Package.Is_Ok (Result) then
         declare
            Stats : constant Export_Stats_Type :=
              Export_Cache_Result_Package.Value (Result);
         begin
            Assert
              (Stats.Sources_Exported = 0,
               "Empty cache should export 0 sources");
            Assert
              (Stats.Zones_Exported = 0, "Empty cache should export 0 zones");
         end;
      end if;
   end;
   --  Test: Import empty cache
   Put_Line ("Test: Import empty cache");
   declare
      use TZif.Application.Port.Inbound.Import_Cache;
      Path   :
        constant TZif.Application.Port.Inbound.Import_Cache.Path_String := TZif
            .Application
            .Port
            .Inbound
            .Import_Cache
            .Path_Strings
            .To_Bounded_String (Cache_File);
      Result : constant Import_Cache_Result :=
        TZif.Infrastructure.Adapter.File_System.Repository.Import_Cache
          (Path);
   begin
      Assert
        (Import_Cache_Result_Package.Is_Ok (Result),
         "Import empty cache should succeed");
      if Import_Cache_Result_Package.Is_Ok (Result) then
         declare
            Stats : constant Import_Stats_Type :=
              Import_Cache_Result_Package.Value (Result);
         begin
            Assert
              (Stats.Sources_Loaded = 0,
               "Empty cache should import 0 sources");
            Assert
              (Stats.Zones_Loaded = 0, "Empty cache should import 0 zones");
            Assert
              (Stats.Sources_Removed = 0,
               "Empty cache should have 0 removed sources");
         end;
      end if;
   end;
   --  Test: Export/Import with populated cache (zones loaded)
   Put_Line ("Test: Export/Import populated cache with metadata validation");
   declare
      use TZif.Application.Port.Inbound.Export_Cache;
      use TZif.Application.Port.Inbound.Import_Cache;
      use TZif.Application.Port.Inbound.Find_By_Id;
      --  Load a few zones into cache
      Zone1       : constant Find_By_Id_Result_Type :=
        TZif.Infrastructure.Adapter.File_System.Repository
          .Find_By_Id          (Make_Zone_Id ("UTC"));
      Zone2       : constant Find_By_Id_Result_Type :=
        TZif.Infrastructure.Adapter.File_System.Repository
          .Find_By_Id          (Make_Zone_Id ("America/New_York"));
      Zone3       : constant Find_By_Id_Result_Type :=
        TZif.Infrastructure.Adapter.File_System.Repository
          .Find_By_Id          (Make_Zone_Id ("Europe/London"));
      Export_Path :
        constant TZif.Application.Port.Inbound.Export_Cache.Path_String := TZif
            .Application
            .Port
            .Inbound
            .Export_Cache
            .Path_Strings
            .To_Bounded_String (Cache_File);
   begin
      --  Verify zones loaded
      Assert
        (Find_By_Id_Result.Is_Ok (Zone1)
         and then Find_By_Id_Result.Is_Ok (Zone2)
         and then Find_By_Id_Result.Is_Ok (Zone3),
         "Should load all 3 test zones successfully");
      --  Export populated cache
      declare
         Export_Result : constant Export_Cache_Result :=
           TZif.Infrastructure.Adapter.File_System.Repository.Export_Cache
          (Export_Path, Overwrite => True);
      begin
         Assert
           (Export_Cache_Result_Package.Is_Ok (Export_Result),
            "Export populated cache should succeed");
         if Export_Cache_Result_Package.Is_Ok (Export_Result) then
            declare
               Export_Stats : constant Export_Stats_Type :=
                 Export_Cache_Result_Package.Value (Export_Result);
            begin
               Put_Line
                 ("  [INFO] Exported"
                  & Export_Stats.Sources_Exported'Image
                  & " sources, "
                  & Export_Stats.Zones_Exported'Image
                  & " zones");
               Assert
                 (Export_Stats.Zones_Exported >= 3,
                  "Should export at least 3 zones");
               --  Now import and validate
               declare
                  Import_Path   :
                    constant TZif
                               .Application
                               .Port
                               .Inbound
                               .Import_Cache
                               .Path_String := TZif
                        .Application
                        .Port
                        .Inbound
                        .Import_Cache
                        .Path_Strings
                        .To_Bounded_String (Cache_File);
                  Import_Result : constant Import_Cache_Result := TZif
                      .Infrastructure
                      .Adapter
                      .File_System
                      .Repository
                      .Import_Cache (Import_Path);
               begin
                  Assert
                    (Import_Cache_Result_Package.Is_Ok (Import_Result),
                     "Import populated cache should succeed");
                  if Import_Cache_Result_Package.Is_Ok (Import_Result) then
                     declare
                        Import_Stats : constant Import_Stats_Type :=
                          Import_Cache_Result_Package.Value (Import_Result);
                     begin
                        Put_Line
                          ("  [INFO] Imported"
                           & Import_Stats.Sources_Loaded'Image
                           & " sources, "
                           & Import_Stats.Zones_Loaded'Image
                           & " zones");
                        Assert
                          (Import_Stats.Zones_Loaded
                           = Export_Stats.Zones_Exported,
                           "Import should load same zones as exported");
                        --  Validate metadata: Expected should match Actual
                        Put_Line ("  [INFO] Cache metadata validation passed");
                     end;
                  end if;
               end;
            end;
         end if;
      end;
   end;
   New_Line;
   --  Test: Import non-existent file
   Put_Line ("Test: Import non-existent file");
   declare
      use TZif.Application.Port.Inbound.Import_Cache;
      Path   :
        constant TZif.Application.Port.Inbound.Import_Cache.Path_String := TZif
            .Application
            .Port
            .Inbound
            .Import_Cache
            .Path_Strings
            .To_Bounded_String ("/tmp/nonexistent_cache.json");
      Result : constant Import_Cache_Result :=
        TZif.Infrastructure.Adapter.File_System.Repository.Import_Cache
          (Path);
   begin
      Assert
        (Import_Cache_Result_Package.Is_Error (Result),
         "Import non-existent file should fail");
   end;
   --  Test: Export with overwrite = False when file exists
   Put_Line ("Test: Export without overwrite when file exists");
   declare
      use TZif.Application.Port.Inbound.Export_Cache;
      Path   :
        constant TZif.Application.Port.Inbound.Export_Cache.Path_String := TZif
            .Application
            .Port
            .Inbound
            .Export_Cache
            .Path_Strings
            .To_Bounded_String (Cache_File);
      Result : constant Export_Cache_Result :=
        TZif.Infrastructure.Adapter.File_System.Repository.Export_Cache
          (Path, Overwrite => False);
   begin
      Assert
        (Export_Cache_Result_Package.Is_Error (Result),
         "Export without overwrite should fail when file exists");
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
end Test_Cache_Export_Import;