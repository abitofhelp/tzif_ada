pragma Ada_2022;
--  ===========================================================================
--  Tzif.Domain.Service.Timezone_Lookup
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Timezone Lookup - domain logic and operations.
--
--  Dependencies:
--    TZif.Domain.TZif_Data
--    TZif.Domain.Value_Object.Epoch_Seconds
--    TZif.Domain.Value_Object.UTC_Offset
--
--  ===========================================================================

with TZif.Domain.TZif_Data;
with TZif.Domain.Value_Object.Epoch_Seconds;
with TZif.Domain.Value_Object.UTC_Offset;

package TZif.Domain.Service.Timezone_Lookup with
  Preelaborate
is

   use TZif.Domain.TZif_Data;
   use TZif.Domain.Value_Object.Epoch_Seconds;
   use TZif.Domain.Value_Object.UTC_Offset;

   --  ========================================================================
   --  Find_UTC_Offset_At_Time
   --  ========================================================================
   --  Find the UTC offset applicable at the given epoch time.
   --
   --  Parameters:
   --    Data : TZif timezone data
   --    Time : Epoch timestamp to query
   --
   --  Returns:
   --    UTC offset in seconds (negative for west of UTC, positive for east)
   --
   --  Algorithm:
   --    1. Binary search transitions to find last transition <= Time
   --    2. Use that transition's type_index to lookup timezone type
   --    3. Return the UTC offset from that type
   --    4. If no transitions or Time < first transition, use first type
   --  ========================================================================

   function Find_UTC_Offset_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type) return UTC_Offset_Type;

   --  ========================================================================
   --  Is_DST_At_Time
   --  ========================================================================
   --  Determine if DST (Daylight Saving Time) is in effect at given time.
   --
   --  Parameters:
   --    Data : TZif timezone data
   --    Time : Epoch timestamp to query
   --
   --  Returns:
   --    True if DST is in effect, False otherwise
   --  ========================================================================

   function Is_DST_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type) return Boolean;

   --  ========================================================================
   --  Get_Abbreviation_At_Time
   --  ========================================================================
   --  Get the timezone abbreviation (e.g., "PST", "PDT") at given time.
   --
   --  Parameters:
   --    Data : TZif timezone data
   --    Time : Epoch timestamp to query
   --
   --  Returns:
   --    Timezone abbreviation string (e.g., "PST", "EDT", "UTC")
   --  ========================================================================

   function Get_Abbreviation_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type) return String;

end TZif.Domain.Service.Timezone_Lookup;
