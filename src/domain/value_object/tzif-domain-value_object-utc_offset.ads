pragma Ada_2022;
--  ===========================================================================
--  TZif.Domain.Value_Object.Utc_Offset
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Utc Offset value object - immutable domain data.
--
--  Responsibilities:
--    - Define Utc Offset type and operations
--    - Provide constructors and accessors
--
--  Key Types:
--    UTC_Offset_Type
--
--  Dependencies:
--    Pure
--
--  ===========================================================================

package TZif.Domain.Value_Object.UTC_Offset with
  Pure
is

   --  ========================================================================
   --  UTC Offset Type
   --  ========================================================================

   --  Offset from UTC in seconds
   --  Range: ±26 hours to accommodate historical Local Mean Time (LMT) offsets
   --  Modern timezones typically range from -12 to +14 hours, but historical
   --  LMT offsets in IANA tzdata can exceed Â±15 hours (e.g., America/Juneau
   --  +15.04h, Asia/Manila -15.94h before standard time zones were adopted)
   type UTC_Offset_Type is range -26 * 3_600 .. 26 * 3_600;

   --  ========================================================================
   --  Constants
   --  ========================================================================

   --  Zero offset (UTC itself)
   UTC : constant UTC_Offset_Type := 0;

   --  Common offset constants (in hours)
   One_Hour    : constant := 3_600;
   Two_Hours   : constant := 2 * One_Hour;
   Three_Hours : constant := 3 * One_Hour;

   --  Typical min/max for most timezones
   Typical_Min_Offset : constant UTC_Offset_Type := -12 * One_Hour;  -- UTC-12
   Typical_Max_Offset : constant UTC_Offset_Type := 14 * One_Hour;   -- UTC+14

   --  ========================================================================
   --  Query Functions
   --  ========================================================================

   --  Check if offset is positive (ahead of UTC)
   function Is_Ahead_Of_UTC (Offset : UTC_Offset_Type) return Boolean is
     (Offset > 0);

   --  Check if offset is negative (behind UTC)
   function Is_Behind_UTC (Offset : UTC_Offset_Type) return Boolean is
     (Offset < 0);

   --  Get absolute value of offset
   function Abs_Value (Offset : UTC_Offset_Type) return UTC_Offset_Type is
     (abs Offset);

   --  ========================================================================
   --  Conversion Functions
   --  ========================================================================

   --  Convert offset to hours (rounded)
   function To_Hours (Offset : UTC_Offset_Type) return Integer is
     (Integer (Offset / One_Hour));

   --  Convert offset to minutes (remainder after hours)
   function To_Minutes_Part (Offset : UTC_Offset_Type) return Integer is
     (Integer ((abs Offset) mod One_Hour) / 60);

   --  Convert offset to seconds (remainder after hours and minutes)
   function To_Seconds_Part (Offset : UTC_Offset_Type) return Integer is
     (Integer ((abs Offset) mod 60));

   --  Create offset from hours, minutes, seconds
   function From_HMS
     (Hours : Integer; Minutes : Natural := 0; Seconds : Natural := 0)
      return UTC_Offset_Type is
     (UTC_Offset_Type (Hours * One_Hour + Minutes * 60 + Seconds));

end TZif.Domain.Value_Object.UTC_Offset;
