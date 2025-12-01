pragma Ada_2022;
--  ===========================================================================
--  Tzif.Domain.Tzif_Data
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Tzif Data interface and type definitions.
--
--  Key Types:
--    Transition
--    Timezone_Type
--    Transition_Vector
--    Timezone_Type_Vector
--    POSIX_TZ_String
--    ... and 4 more
--
--  Dependencies:
--    TZif_Config
--    TZif.Domain.Value_Object.TZif_Header
--    TZif.Domain.Value_Object.Transition
--
--  ===========================================================================

with Ada.Containers.Vectors;
with Ada.Strings.Bounded;
with TZif_Config;
with TZif.Domain.Value_Object.TZif_Header;
with TZif.Domain.Value_Object.Transition;
with TZif.Domain.Value_Object.Timezone_Type;
with TZif.Domain.Value_Object.Epoch_Seconds;
with TZif.Domain.Value_Object.UTC_Offset;
with TZif.Domain.Option;

package TZif.Domain.TZif_Data with
  Preelaborate
is

   use TZif.Domain.Value_Object;
   use TZif.Domain.Value_Object.Epoch_Seconds;
   use TZif.Domain.Value_Object.UTC_Offset;
   use TZif.Domain.Value_Object.Timezone_Type;

   --  Make comparison operators visible for vector instantiation
   use type Transition.Transition_Type;

   --  ========================================================================
   --  Bounded Containers for TZif Data
   --  ========================================================================

   --  Bounded vector for transitions
   package Transition_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Transition.Transition_Type);

   subtype Transition_Vector is Transition_Vectors.Vector;

   --  Bounded vector for timezone types
   package Timezone_Type_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Timezone_Type.Timezone_Type_Record);

   subtype Timezone_Type_Vector is Timezone_Type_Vectors.Vector;

   --  Bounded string for POSIX TZ string (version 2+)
   package POSIX_TZ_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max =>
        TZif_Config.Max_Zone_ID_Length * 2);  -- POSIX strings can be long

   subtype POSIX_TZ_String is POSIX_TZ_Strings.Bounded_String;

   --  ========================================================================
   --  Leap Second Record
   --  ========================================================================

   type Leap_Second_Type is record
      --  Time at which leap second occurs
      Occurrence_Time : Epoch_Seconds.Epoch_Seconds_Type;

      --  Total number of leap seconds after this occurrence
      Leap_Count : Integer;
   end record;

   --  Bounded vector for leap seconds
   package Leap_Second_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Leap_Second_Type);

   subtype Leap_Second_Vector is Leap_Second_Vectors.Vector;

   --  ========================================================================
   --  TZif Data Aggregate Root
   --  ========================================================================

   type TZif_Data_Type is record
      --  Header information
      Header : TZif_Header.TZif_Header_Type;

      --  Transition times and their corresponding type indices
      --  Sorted in ascending chronological order
      Transitions : Transition_Vector;

      --  Timezone types (offset, DST flag, abbreviation)
      --  Indexed by transition type_index values
      Timezone_Types : Timezone_Type_Vector;

      --  Leap second corrections (optional)
      Leap_Seconds : Leap_Second_Vector;

      --  POSIX TZ string for future calculations (version 2+)
      --  Empty for version 1 files
      POSIX_TZ : POSIX_TZ_String;
   end record;

   --  ========================================================================
   --  Query Functions
   --  ========================================================================

   --  Check if TZif data has transitions
   function Has_Transitions (Data : TZif_Data_Type) return Boolean is
     (not Data.Transitions.Is_Empty);

   --  Get number of transitions
   function Transition_Count (Data : TZif_Data_Type) return Natural is
     (Natural (Data.Transitions.Length));

   --  Check if TZif data has leap seconds
   function Has_Leap_Seconds (Data : TZif_Data_Type) return Boolean is
     (not Data.Leap_Seconds.Is_Empty);

   --  Get number of leap seconds
   function Leap_Second_Count (Data : TZif_Data_Type) return Natural is
     (Natural (Data.Leap_Seconds.Length));

   --  Check if TZif data has POSIX TZ string
   function Has_POSIX_TZ (Data : TZif_Data_Type) return Boolean is
     (POSIX_TZ_Strings.Length (Data.POSIX_TZ) > 0);

   --  Get POSIX TZ string
   function Get_POSIX_TZ (Data : TZif_Data_Type) return String is
     (POSIX_TZ_Strings.To_String (Data.POSIX_TZ));

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
   --  Lookup Functions
   --  ========================================================================

   --  Find the timezone type in effect at a given epoch time
   --  Returns the type index, or raises Constraint_Error if not found
   function Find_Type_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type) return Natural;

   --  Get timezone type by index
   function Get_Type
     (Data : TZif_Data_Type; Type_Index : Natural)
      return Timezone_Type.Timezone_Type_Record is
     (Data.Timezone_Types.Element (Type_Index)) with
     Pre => Natural (Data.Timezone_Types.Length) > Type_Index;

   --  Find the UTC offset in effect at a given epoch time
   --  Returns Some(offset) or None if no timezone types available
   function Find_Offset_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type)
      return UTC_Offset_Option;

   --  Check if DST is in effect at a given epoch time
   --  Returns Some(is_dst) or None if no timezone types available
   function Is_DST_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type) return Boolean_Option;

   --  Get timezone abbreviation in effect at a given epoch time
   --  Returns Some(abbreviation) or None if no timezone types available
   function Get_Abbreviation_At_Time
     (Data : TZif_Data_Type; Time : Epoch_Seconds_Type)
      return Abbreviation_Option;

end TZif.Domain.TZif_Data;
