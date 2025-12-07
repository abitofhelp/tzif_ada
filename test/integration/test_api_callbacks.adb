pragma Ada_2022;
--  ======================================================================
--  Test_API_Callbacks - Body
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  ======================================================================

package body Test_API_Callbacks is

   procedure On_Pattern_Match (Zone : Find_Pattern_Port.Zone_Name_String) is
      pragma Unreferenced (Zone);
   begin
      Pattern_Match_Count := Pattern_Match_Count + 1;
   end On_Pattern_Match;

   procedure On_Region_Match (Zone : Find_Region_Port.Zone_Name_String) is
      pragma Unreferenced (Zone);
   begin
      Region_Match_Count := Region_Match_Count + 1;
   end On_Region_Match;

   procedure On_Regex_Match (Zone : Find_Regex_Port.Zone_Name_String) is
      pragma Unreferenced (Zone);
   begin
      Regex_Match_Count := Regex_Match_Count + 1;
   end On_Regex_Match;

   procedure Reset_Counters is
   begin
      Pattern_Match_Count := 0;
      Region_Match_Count := 0;
      Regex_Match_Count := 0;
   end Reset_Counters;

end Test_API_Callbacks;
