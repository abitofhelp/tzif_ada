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
--  ===========================================================================

with TZif.Domain.Value_Object.Timezone_Type;

package body TZif.Domain.Service.Timezone_Lookup is

   use TZif.Domain.Value_Object.Timezone_Type;

   --  ========================================================================
   --  Find_Type_Index_At_Time (Helper)
   --  ========================================================================
   --  Find the timezone type index applicable at the given time.
   --  Returns 0 if no transitions, otherwise returns the type_index from
   --  the last transition that occurred before or at the given time.
   --  ========================================================================

   function Find_Type_Index_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type) return Natural
   is
   begin
      --  No transitions: use first type (index 0)
      if Transition_Count (Data) = 0 then
         return 0;
      end if;

      --  Time before first transition: use first transition's type
      if Time < Data.Transitions.First_Element.Time then
         return Data.Transitions.First_Element.Type_Index;
      end if;

      --  Binary search for last transition <= Time
      declare
         Low          : Natural := Data.Transitions.First_Index;
         High         : Natural := Data.Transitions.Last_Index;
         Mid          : Natural;
         Result_Index : Natural := Data.Transitions.First_Element.Type_Index;
      begin
         while Low <= High loop
            Mid := Low + (High - Low) / 2;

            if Data.Transitions.Element (Mid).Time <= Time then
               --  This transition applies, but there might be a later one
               Result_Index := Data.Transitions.Element (Mid).Type_Index;
               Low          := Mid + 1;
            else
               --  This transition is too late
               if Mid > Data.Transitions.First_Index then
                  High := Mid - 1;
               else
                  exit;
               end if;
            end if;
         end loop;

         return Result_Index;
      end;
   end Find_Type_Index_At_Time;

   --  ========================================================================
   --  Find_UTC_Offset_At_Time
   --  ========================================================================

   function Find_UTC_Offset_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type) return UTC_Offset_Type
   is
      Type_Index : constant Natural := Find_Type_Index_At_Time (Data, Time);
   begin
      --  Type indices from TZif file directly match vector indices
      --  (vector starts at index 0, same as TZif type indices)
      --  Bounds check: ensure type index is valid
      if Natural (Data.Timezone_Types.Length) = 0 then
         --  No types available - should never happen in valid TZif file
         return 0;
      elsif Type_Index in
          Data.Timezone_Types.First_Index .. Data.Timezone_Types.Last_Index
      then
         return Data.Timezone_Types.Element (Type_Index).UTC_Offset;
      else
         --  Invalid type index - use first type as fallback
         return Data.Timezone_Types.First_Element.UTC_Offset;
      end if;
   end Find_UTC_Offset_At_Time;

   --  ========================================================================
   --  Is_DST_At_Time
   --  ========================================================================

   function Is_DST_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type) return Boolean
   is
      Type_Index : constant Natural := Find_Type_Index_At_Time (Data, Time);
   begin
      --  Type indices from TZif file directly match vector indices
      if Natural (Data.Timezone_Types.Length) = 0 then
         return False;  -- No types - assume no DST
      elsif Type_Index in
          Data.Timezone_Types.First_Index .. Data.Timezone_Types.Last_Index
      then
         return Data.Timezone_Types.Element (Type_Index).Is_DST;
      else
         --  Invalid type index - use first type as fallback
         return Data.Timezone_Types.First_Element.Is_DST;
      end if;
   end Is_DST_At_Time;

   --  ========================================================================
   --  Get_Abbreviation_At_Time
   --  ========================================================================

   function Get_Abbreviation_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type) return String
   is
      Type_Index : constant Natural := Find_Type_Index_At_Time (Data, Time);
   begin
      --  Type indices from TZif file directly match vector indices
      if Natural (Data.Timezone_Types.Length) = 0 then
         return "";  -- No types available
      elsif Type_Index in
          Data.Timezone_Types.First_Index .. Data.Timezone_Types.Last_Index
      then
         return Get_Abbreviation (Data.Timezone_Types.Element (Type_Index));
      else
         --  Invalid type index - use first type as fallback
         return Get_Abbreviation (Data.Timezone_Types.First_Element);
      end if;
   end Get_Abbreviation_At_Time;

end TZif.Domain.Service.Timezone_Lookup;
