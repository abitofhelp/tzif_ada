pragma Ada_2022;
--  ===========================================================================
--  TZif.Domain.Value_Object.Transition_Info
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Transition Info value object - immutable domain data.
--
--  Responsibilities:
--    - Define Transition Info type and operations
--    - Provide constructors and accessors
--
--  Key Types:
--    Abbreviation_Type
--    Transition_Info_Type
--
--  Dependencies:
--    TZif_Config
--    TZif.Domain.Value_Object.UTC_Offset
--    TZif.Domain.Value_Object.Epoch_Seconds
--
--  ===========================================================================

with Ada.Strings.Bounded;
with TZif_Config;
with TZif.Domain.Value_Object.UTC_Offset;
with TZif.Domain.Value_Object.Epoch_Seconds;

package TZif.Domain.Value_Object.Transition_Info with
  Preelaborate
is

   use TZif.Domain.Value_Object.UTC_Offset;
   use TZif.Domain.Value_Object.Epoch_Seconds;

   --  ========================================================================
   --  Bounded String for Abbreviations
   --  ========================================================================

   package Abbreviation_Strings is new Ada.Strings.Bounded
     .Generic_Bounded_Length
     (Max => TZif_Config.Max_Abbreviation_Length);

   subtype Abbreviation_Type is Abbreviation_Strings.Bounded_String;

   --  ========================================================================
   --  Transition Info Record
   --  ========================================================================

   type Transition_Info_Type is record
      --  Epoch time for which this info applies
      Epoch_Time : Epoch_Seconds_Type;

      --  Offset from UTC in seconds
      UTC_Offset : UTC_Offset_Type;

      --  Is Daylight Saving Time in effect?
      Is_DST : Boolean;

      --  Timezone abbreviation (e.g., "PST", "PDT", "EST")
      Abbreviation : Abbreviation_Type;
   end record;

   --  ========================================================================
   --  Constructor Functions
   --  ========================================================================

   --  Create transition info
   function Make_Transition_Info
     (Epoch_Time : Epoch_Seconds_Type; UTC_Offset : UTC_Offset_Type;
      Is_DST : Boolean; Abbreviation : String) return Transition_Info_Type is
     (Epoch_Time   => Epoch_Time, UTC_Offset => UTC_Offset, Is_DST => Is_DST,
      Abbreviation => Abbreviation_Strings.To_Bounded_String (Abbreviation));

   --  ========================================================================
   --  Query Functions
   --  ========================================================================

   --  Get abbreviation as string
   function Get_Abbreviation (Info : Transition_Info_Type) return String is
     (Abbreviation_Strings.To_String (Info.Abbreviation));

   --  Check if this is daylight saving time
   function Is_Daylight_Saving (Info : Transition_Info_Type) return Boolean is
     (Info.Is_DST);

   --  Check if this is standard time
   function Is_Standard_Time (Info : Transition_Info_Type) return Boolean is
     (not Info.Is_DST);

   --  Get UTC offset in seconds
   function Get_UTC_Offset_Seconds
     (Info : Transition_Info_Type) return Integer is
     (Integer (Info.UTC_Offset));

   --  Get epoch time
   function Get_Epoch_Time
     (Info : Transition_Info_Type) return Epoch_Seconds_Type is
     (Info.Epoch_Time);

end TZif.Domain.Value_Object.Transition_Info;
