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
--    TZif.Domain.Value_Object.Timezone_Type
--    Functional.Option
--
--  ===========================================================================

with TZif.Domain.TZif_Data;
with TZif.Domain.Value_Object.Epoch_Seconds;
with TZif.Domain.Value_Object.UTC_Offset;
with TZif.Domain.Value_Object.Timezone_Type;
with TZif.Domain.Option;

package TZif.Domain.Service.Timezone_Lookup with
  Preelaborate
is

   use TZif.Domain.TZif_Data;
   use TZif.Domain.Value_Object.Epoch_Seconds;
   use TZif.Domain.Value_Object.UTC_Offset;
   use TZif.Domain.Value_Object.Timezone_Type;

   --  ========================================================================
   --  Option Types for Lookup Results
   --  ========================================================================
   --  These Option types allow callers to explicitly handle cases where
   --  timezone data is unavailable (no types defined, invalid type index).
   --  ========================================================================

   package UTC_Offset_Options is new TZif.Domain.Option (UTC_Offset_Type);
   subtype UTC_Offset_Option is UTC_Offset_Options.Option;

   package Boolean_Options is new TZif.Domain.Option (Boolean);
   subtype Boolean_Option is Boolean_Options.Option;

   package Abbreviation_Options is new TZif.Domain.Option (Abbreviation_Type);
   subtype Abbreviation_Option is Abbreviation_Options.Option;

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
   --    Some(offset) - UTC offset in seconds (negative west, positive east)
   --    None         - No timezone types available in data
   --
   --  Algorithm:
   --    1. Binary search transitions to find last transition <= Time
   --    2. Use that transition's type_index to lookup timezone type
   --    3. Return the UTC offset from that type
   --    4. If no transitions or Time < first transition, use first type
   --  ========================================================================

   function Find_UTC_Offset_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type)
      return UTC_Offset_Option;

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
   --    Some(True)  - DST is in effect
   --    Some(False) - Standard time is in effect
   --    None        - No timezone types available in data
   --  ========================================================================

   function Is_DST_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type) return Boolean_Option;

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
   --    Some(abbreviation) - Timezone abbreviation (e.g., "PST", "EDT", "UTC")
   --    None               - No timezone types available in data
   --  ========================================================================

   function Get_Abbreviation_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type)
      return Abbreviation_Option;

end TZif.Domain.Service.Timezone_Lookup;
