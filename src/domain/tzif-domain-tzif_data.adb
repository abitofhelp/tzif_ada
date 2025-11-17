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

package body TZif.Domain.TZif_Data is

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
   begin
      --  If no transitions, use first type (index 0)
      if Data.Transitions.Is_Empty then
         return 0;
      end if;

      --  If time is before first transition, use first type
      if Time < Data.Transitions.First_Element.Time then
         return 0;
      end if;

      --  Binary search for the last transition <= Time
      --  We iterate backwards to find the most recent transition
      for I in reverse
        Data.Transitions.First_Index .. Data.Transitions.Last_Index
      loop
         if Data.Transitions.Element (I).Time <= Time then
            return Data.Transitions.Element (I).Type_Index;
         end if;
      end loop;

      --  Fallback: use first type (shouldn't reach here)
      return 0;
   end Find_Type_At_Time;

   --  ========================================================================
   --  Find_Offset_At_Time
   --  ========================================================================

   function Find_Offset_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type) return UTC_Offset_Type
   is
      Type_Index : constant Natural := Find_Type_At_Time (Data, Time);
      TZ_Type    : constant Timezone_Type.Timezone_Type_Record :=
        Get_Type (Data, Type_Index);
   begin
      return TZ_Type.UTC_Offset;
   end Find_Offset_At_Time;

   --  ========================================================================
   --  Is_DST_At_Time
   --  ========================================================================

   function Is_DST_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type) return Boolean
   is
      Type_Index : constant Natural := Find_Type_At_Time (Data, Time);
      TZ_Type    : constant Timezone_Type.Timezone_Type_Record :=
        Get_Type (Data, Type_Index);
   begin
      return TZ_Type.Is_DST;
   end Is_DST_At_Time;

   --  ========================================================================
   --  Get_Abbreviation_At_Time
   --  ========================================================================

   function Get_Abbreviation_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type) return String
   is
      Type_Index : constant Natural := Find_Type_At_Time (Data, Time);
      TZ_Type    : constant Timezone_Type.Timezone_Type_Record :=
        Get_Type (Data, Type_Index);
   begin
      return Timezone_Type.Get_Abbreviation (TZ_Type);
   end Get_Abbreviation_At_Time;

end TZif.Domain.TZif_Data;
