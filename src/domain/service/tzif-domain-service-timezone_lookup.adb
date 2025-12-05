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

package body TZif.Domain.Service.Timezone_Lookup is

   --  ========================================================================
   --  Find_Type_Index_At_Time (Helper)
   --  ========================================================================
   --  Find the timezone type index applicable at the given time.
   --  Returns None if no timezone types available, otherwise returns
   --  Some(type_index) from the last transition before or at the given time.
   --  ========================================================================

   function Find_Type_Index_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type) return Type_Index_Option
   is
      use Transition_Vectors;
      use Timezone_Type_Vectors;
   begin
      --  Return None if no timezone types available
      if Is_Empty (Data.Timezone_Types) then
         return Type_Index_Options.None;
      end if;

      --  No transitions: use first type (index 0)
      if Transition_Count (Data) = 0 then
         return Type_Index_Options.New_Some (0);
      end if;

      --  Time before first transition: use first transition's type
      if Time < Unchecked_First (Data.Transitions).Time then
         return Type_Index_Options.New_Some
           (Unchecked_First (Data.Transitions).Type_Index);
      end if;

      --  Binary search for last transition <= Time
      declare
         Low          : Natural := Transition_Vectors.First_Index;
         High         : Natural := Last_Index (Data.Transitions);
         Mid          : Natural;
         Result_Index : Natural :=
           Unchecked_First (Data.Transitions).Type_Index;
      begin
         while Low <= High loop
            Mid := Low + (High - Low) / 2;

            if Unchecked_Element (Data.Transitions, Mid).Time <= Time then
               --  This transition applies, but there might be a later one
               Result_Index :=
                 Unchecked_Element (Data.Transitions, Mid).Type_Index;
               Low          := Mid + 1;
            else
               --  This transition is too late
               if Mid > Transition_Vectors.First_Index then
                  High := Mid - 1;
               else
                  exit;
               end if;
            end if;
         end loop;

         return Type_Index_Options.New_Some (Result_Index);
      end;
   end Find_Type_Index_At_Time;

   --  ========================================================================
   --  Find_UTC_Offset_At_Time
   --  ========================================================================

   function Find_UTC_Offset_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type)
      return UTC_Offset_Option
   is
      use Timezone_Type_Vectors;
      Type_Index_Opt : constant Type_Index_Option :=
        Find_Type_Index_At_Time (Data, Time);
   begin
      if Type_Index_Options.Is_None (Type_Index_Opt) then
         return UTC_Offset_Options.None;
      end if;

      declare
         --  TZif uses 0-based type indices; our vector is 1-based
         Type_Index : constant Positive :=
           Type_Index_Options.Value (Type_Index_Opt) + 1;
      begin
         --  Validate index is within bounds of Timezone_Types vector
         if Type_Index > Length (Data.Timezone_Types) then
            return UTC_Offset_Options.None;
         end if;

         return
           UTC_Offset_Options.New_Some
             (Unchecked_Element (Data.Timezone_Types, Type_Index).UTC_Offset);
      end;
   end Find_UTC_Offset_At_Time;

   --  ========================================================================
   --  Is_DST_At_Time
   --  ========================================================================

   function Is_DST_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type) return Boolean_Option
   is
      use Timezone_Type_Vectors;
      Type_Index_Opt : constant Type_Index_Option :=
        Find_Type_Index_At_Time (Data, Time);
   begin
      if Type_Index_Options.Is_None (Type_Index_Opt) then
         return Boolean_Options.None;
      end if;

      declare
         --  TZif uses 0-based type indices; our vector is 1-based
         Type_Index : constant Positive :=
           Type_Index_Options.Value (Type_Index_Opt) + 1;
      begin
         --  Validate index is within bounds of Timezone_Types vector
         if Type_Index > Length (Data.Timezone_Types) then
            return Boolean_Options.None;
         end if;

         return
           Boolean_Options.New_Some
             (Unchecked_Element (Data.Timezone_Types, Type_Index).Is_DST);
      end;
   end Is_DST_At_Time;

   --  ========================================================================
   --  Get_Abbreviation_At_Time
   --  ========================================================================

   function Get_Abbreviation_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type)
      return Abbreviation_Option
   is
      use Timezone_Type_Vectors;
      Type_Index_Opt : constant Type_Index_Option :=
        Find_Type_Index_At_Time (Data, Time);
   begin
      if Type_Index_Options.Is_None (Type_Index_Opt) then
         return Abbreviation_Options.None;
      end if;

      declare
         --  TZif uses 0-based type indices; our vector is 1-based
         Type_Index : constant Positive :=
           Type_Index_Options.Value (Type_Index_Opt) + 1;
      begin
         --  Validate index is within bounds of Timezone_Types vector
         if Type_Index > Length (Data.Timezone_Types) then
            return Abbreviation_Options.None;
         end if;

         return
           Abbreviation_Options.New_Some
             (Unchecked_Element
                (Data.Timezone_Types, Type_Index).Abbreviation);
      end;
   end Get_Abbreviation_At_Time;

end TZif.Domain.Service.Timezone_Lookup;
