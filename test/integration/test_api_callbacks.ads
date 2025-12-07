pragma Ada_2022;
--  ======================================================================
--  Test_API_Callbacks
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Library-level callbacks for Test_API integration tests.
--    Needed because 'Access requires library-level subprograms.
--  ======================================================================
with TZif.API;

package Test_API_Callbacks is

   use TZif.API;

   --  Match counters
   Pattern_Match_Count : Natural := 0;
   Region_Match_Count  : Natural := 0;
   Regex_Match_Count   : Natural := 0;

   --  Callback for Find_By_Pattern
   procedure On_Pattern_Match (Zone : Find_Pattern_Port.Zone_Name_String);

   --  Callback for Find_By_Region
   procedure On_Region_Match (Zone : Find_Region_Port.Zone_Name_String);

   --  Callback for Find_By_Regex
   procedure On_Regex_Match (Zone : Find_Regex_Port.Zone_Name_String);

   --  Reset counters between tests
   procedure Reset_Counters;

end Test_API_Callbacks;
