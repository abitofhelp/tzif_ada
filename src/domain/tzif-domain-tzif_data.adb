pragma Ada_2022;
--  ===========================================================================
--  Tzif.Domain.Tzif_Data
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Tzif Data implementation.
--
--  ===========================================================================

with TZif.Domain.Service.Timezone_Lookup;

package body TZif.Domain.TZif_Data is

   package Tz_Lookup renames TZif.Domain.Service.Timezone_Lookup;

   --  ========================================================================
   --  Find_Type_At_Time
   --  ========================================================================
   --  Uses binary search to find the transition in effect at the given time.
   --  Transitions are sorted chronologically, so we find the last transition
   --  that occurs before or at the given time.
   --  ========================================================================

   function Find_Type_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type) return Natural
   is
      use Transition_Vectors;
   begin
      --  If no transitions, use first type (index 0)
      if Is_Empty (Data.Transitions) then
         return 0;
      end if;

      --  If time is before first transition, use first transition's type
      if Time < Unchecked_First (Data.Transitions).Time then
         return Unchecked_First (Data.Transitions).Type_Index;
      end if;

      --  Linear search backwards for the last transition <= Time
      --  We iterate backwards to find the most recent transition
      for I in reverse First_Index .. Last_Index (Data.Transitions) loop
         if Unchecked_Element (Data.Transitions, I).Time <= Time then
            return Unchecked_Element (Data.Transitions, I).Type_Index;
         end if;
      end loop;

      --  Fallback: use first type (shouldn't reach here)
      return 0;
   end Find_Type_At_Time;

   --  ========================================================================
   --  Find_Offset_At_Time
   --  ========================================================================
   --  Delegates to Domain Service for proper Option handling

   function Find_Offset_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type)
      return UTC_Offset_Option
   is
      Service_Result : constant Tz_Lookup.UTC_Offset_Option :=
        Tz_Lookup.Find_UTC_Offset_At_Time (Data, Time);
   begin
      --  Convert from service Option type to our Option type
      if Tz_Lookup.UTC_Offset_Options.Is_Some (Service_Result) then
         return
           UTC_Offset_Options.New_Some
             (Tz_Lookup.UTC_Offset_Options.Value (Service_Result));
      else
         return UTC_Offset_Options.None;
      end if;
   end Find_Offset_At_Time;

   --  ========================================================================
   --  Is_DST_At_Time
   --  ========================================================================
   --  Delegates to Domain Service for proper Option handling

   function Is_DST_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type) return Boolean_Option
   is
      Service_Result : constant Tz_Lookup.Boolean_Option :=
        Tz_Lookup.Is_DST_At_Time (Data, Time);
   begin
      --  Convert from service Option type to our Option type
      if Tz_Lookup.Boolean_Options.Is_Some (Service_Result) then
         return
           Boolean_Options.New_Some
             (Tz_Lookup.Boolean_Options.Value (Service_Result));
      else
         return Boolean_Options.None;
      end if;
   end Is_DST_At_Time;

   --  ========================================================================
   --  Get_Abbreviation_At_Time
   --  ========================================================================
   --  Delegates to Domain Service for proper Option handling

   function Get_Abbreviation_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type)
      return Abbreviation_Option
   is
      Service_Result : constant Tz_Lookup.Abbreviation_Option :=
        Tz_Lookup.Get_Abbreviation_At_Time (Data, Time);
   begin
      --  Convert from service Option type to our Option type
      if Tz_Lookup.Abbreviation_Options.Is_Some (Service_Result) then
         return
           Abbreviation_Options.New_Some
             (Tz_Lookup.Abbreviation_Options.Value (Service_Result));
      else
         return Abbreviation_Options.None;
      end if;
   end Get_Abbreviation_At_Time;

end TZif.Domain.TZif_Data;
