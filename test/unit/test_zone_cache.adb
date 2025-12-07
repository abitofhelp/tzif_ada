pragma Ada_2022;
--  ======================================================================
--  Test_Zone_Cache
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for Zone Cache operations.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Test_Framework;
with TZif.Infrastructure.Cache.Zone_Cache;
with TZif.Domain.Value_Object.Zone_Id;
with TZif.Domain.TZif_Data;
with TZif.Domain.Value_Object.TZif_Header;

procedure Test_Zone_Cache is

   use TZif.Infrastructure.Cache.Zone_Cache;
   use TZif.Domain.Value_Object.Zone_Id;
   use TZif.Domain.TZif_Data;
   use TZif.Domain.Value_Object.TZif_Header;

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

   --  Helper to create minimal TZif data for testing
   function Make_Test_Data
     (Version : TZif_Version_Type) return TZif_Data_Type is
      Data : TZif_Data_Type;
   begin
      Data.Header.Version := Version;
      Data.Header.Transition_Count := 0;
      Data.Header.Type_Count := 0;
      Data.Header.Leap_Count := 0;
      return Data;
   end Make_Test_Data;

   --  Test Cache
   Cache : Zone_Cache_Type;

begin
   Put_Line ("Test: Zone Cache operations");

   --  =====================================================================
   --  Test: Initial state
   --  =====================================================================
   Put_Line ("  Subtest: Initial state");

   Assert (Cache.Is_Empty, "New cache is empty");
   Assert (Cache.Size = 0, "New cache has size 0");
   Assert
     (not Cache.Contains (Make_Zone_Id ("UTC")),
      "New cache does not contain UTC");

   --  =====================================================================
   --  Test: Insert and retrieve
   --  =====================================================================
   Put_Line ("  Subtest: Insert and retrieve");

   declare
      UTC_Data  : constant TZif_Data_Type := Make_Test_Data (Version_2);
      Zone_ID   : constant Zone_Id_Type := Make_Zone_Id ("UTC");
   begin
      Cache.Insert (Zone_ID, UTC_Data);
      Assert (Cache.Contains (Zone_ID), "Cache contains inserted zone");
      Assert (Cache.Size = 1, "Cache size is 1 after insert");
      Assert (not Cache.Is_Empty, "Cache is not empty after insert");

      --  Test Get returns the data
      declare
         Result : constant Zone_Data_Option_Type := Cache.Get (Zone_ID);
      begin
         Assert
           (TZif_Data_Option.Is_Some (Result),
            "Get returns Some for existing zone");
      end;
   end;

   --  =====================================================================
   --  Test: Multiple inserts
   --  =====================================================================
   Put_Line ("  Subtest: Multiple inserts");

   declare
      NY_Data : constant TZif_Data_Type := Make_Test_Data (Version_2);
      LA_Data : constant TZif_Data_Type := Make_Test_Data (Version_3);
      NY_ID   : constant Zone_Id_Type := Make_Zone_Id ("America/New_York");
      LA_ID   : constant Zone_Id_Type := Make_Zone_Id ("America/Los_Angeles");
   begin
      Cache.Insert (NY_ID, NY_Data);
      Cache.Insert (LA_ID, LA_Data);
      Assert (Cache.Contains (NY_ID), "Cache contains New_York");
      Assert (Cache.Contains (LA_ID), "Cache contains Los_Angeles");
      Assert (Cache.Size = 3, "Cache size is 3 (UTC + 2 new)");
   end;

   --  =====================================================================
   --  Test: Get for non-existent zone
   --  =====================================================================
   Put_Line ("  Subtest: Get for non-existent zone");

   declare
      Fake_ID : constant Zone_Id_Type := Make_Zone_Id ("Fake/Zone");
      Result  : constant Zone_Data_Option_Type := Cache.Get (Fake_ID);
   begin
      Assert
        (TZif_Data_Option.Is_None (Result),
         "Get returns None for non-existent zone");
      Assert
        (not Cache.Contains (Fake_ID), "Cache does not contain fake zone");
   end;

   --  =====================================================================
   --  Test: Update existing zone
   --  =====================================================================
   Put_Line ("  Subtest: Update existing zone");

   declare
      UTC_V3   : constant TZif_Data_Type := Make_Test_Data (Version_3);
      Zone_ID  : constant Zone_Id_Type := Make_Zone_Id ("UTC");
      Old_Size : constant Natural := Cache.Size;
   begin
      Cache.Insert (Zone_ID, UTC_V3);
      Assert
        (Cache.Size = Old_Size,
         "Updating existing zone does not increase size");
      Assert (Cache.Contains (Zone_ID), "Cache still contains updated zone");

      --  Verify the data was updated
      declare
         Result : constant Zone_Data_Option_Type := Cache.Get (Zone_ID);
         Data   : TZif_Data_Type;
      begin
         if TZif_Data_Option.Is_Some (Result) then
            Data := TZif_Data_Option.Value (Result);
            Assert
              (Data.Header.Version = Version_3,
               "Updated zone has new version");
         else
            Assert (False, "Get should return Some for updated zone");
         end if;
      end;
   end;

   --  =====================================================================
   --  Test: Remove zone
   --  =====================================================================
   Put_Line ("  Subtest: Remove zone");

   declare
      NY_ID    : constant Zone_Id_Type := Make_Zone_Id ("America/New_York");
      Old_Size : constant Natural := Cache.Size;
   begin
      Cache.Remove (NY_ID);
      Assert (not Cache.Contains (NY_ID), "Removed zone is not in cache");
      Assert (Cache.Size = Old_Size - 1, "Size decreased after remove");
   end;

   --  Test remove of non-existent zone (should be no-op)
   declare
      Fake_ID  : constant Zone_Id_Type := Make_Zone_Id ("Fake/Zone");
      Old_Size : constant Natural := Cache.Size;
   begin
      Cache.Remove (Fake_ID);
      Assert (Cache.Size = Old_Size, "Remove of non-existent zone is no-op");
   end;

   --  =====================================================================
   --  Test: Clear cache
   --  =====================================================================
   Put_Line ("  Subtest: Clear cache");

   Cache.Clear;
   Assert (Cache.Is_Empty, "Cache is empty after clear");
   Assert (Cache.Size = 0, "Cache size is 0 after clear");
   Assert
     (not Cache.Contains (Make_Zone_Id ("UTC")),
      "UTC not in cache after clear");

   --  =====================================================================
   --  Test: Get_All
   --  =====================================================================
   Put_Line ("  Subtest: Get_All");

   declare
      UTC_Data : constant TZif_Data_Type := Make_Test_Data (Version_2);
      NY_Data  : constant TZif_Data_Type := Make_Test_Data (Version_2);
      All_Data : Zone_Data_Map_Type;
   begin
      Cache.Insert (Make_Zone_Id ("UTC"), UTC_Data);
      Cache.Insert (Make_Zone_Id ("America/New_York"), NY_Data);
      All_Data := Cache.Get_All;
      Assert
        (Natural (All_Data.Length) = 2,
         "Get_All returns all zones");
   end;

   --  =====================================================================
   --  Test: LRU eviction (cache size limit is 25)
   --  =====================================================================
   Put_Line ("  Subtest: LRU eviction");

   Cache.Clear;

   --  Fill cache to max size
   for I in 1 .. Max_Cache_Size loop
      declare
         Zone_ID : constant Zone_Id_Type :=
           Make_Zone_Id ("Zone_" & Natural'Image (I));
         Data    : constant TZif_Data_Type := Make_Test_Data (Version_2);
      begin
         Cache.Insert (Zone_ID, Data);
      end;
   end loop;

   Assert (Cache.Size = Max_Cache_Size, "Cache filled to max size");

   --  Insert one more - should evict oldest
   declare
      New_ID  : constant Zone_Id_Type := Make_Zone_Id ("Zone_New");
      Data    : constant TZif_Data_Type := Make_Test_Data (Version_2);
   begin
      Cache.Insert (New_ID, Data);
      Assert (Cache.Size = Max_Cache_Size, "Size stays at max after eviction");
      Assert (Cache.Contains (New_ID), "New zone is in cache");
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

end Test_Zone_Cache;
