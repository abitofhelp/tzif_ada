pragma Ada_2022;
--  ===========================================================================
--  TZif.Domain.Value_Object.Timezone_Type
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Timezone Type value object - immutable domain data.
--
--  Responsibilities:
--    - Define Timezone Type type and operations
--    - Provide constructors and accessors
--
--  Key Types:
--    Abbreviation_Type
--    Timezone_Type_Record
--    Daylight
--
--  Dependencies:
--    TZif_Config
--    TZif.Domain.Value_Object.UTC_Offset
--    Preelaborate
--
--  ===========================================================================

with Ada.Strings.Bounded;
with TZif_Config;
with TZif.Domain.Value_Object.UTC_Offset;

package TZif.Domain.Value_Object.Timezone_Type with
  Preelaborate
is

   use TZif.Domain.Value_Object.UTC_Offset;

   --  ========================================================================
   --  Bounded String for Abbreviations
   --  ========================================================================

   package Abbreviation_Strings is new Ada.Strings.Bounded
     .Generic_Bounded_Length
     (Max => TZif_Config.Max_Abbreviation_Length);

   subtype Abbreviation_Type is Abbreviation_Strings.Bounded_String;

   --  ========================================================================
   --  Timezone Type Record
   --  ========================================================================

   type Timezone_Type_Record is record
      --  Offset from UTC in seconds
      UTC_Offset : UTC_Offset_Type;

      --  Is this type Daylight Saving Time?
      Is_DST : Boolean;

      --  Timezone abbreviation (e.g., "PST", "PDT", "EST")
      Abbreviation : Abbreviation_Type;
   end record;

   --  ========================================================================
   --  Constructor Functions
   --  ========================================================================

   --  Create a timezone type
   function Make_Timezone_Type
     (UTC_Offset : UTC_Offset_Type; Is_DST : Boolean; Abbreviation : String)
      return Timezone_Type_Record is
     (UTC_Offset   => UTC_Offset, Is_DST => Is_DST,
      Abbreviation => Abbreviation_Strings.To_Bounded_String (Abbreviation));

   --  ========================================================================
   --  Query Functions
   --  ========================================================================

   --  Check if this is a DST type
   function Is_Daylight_Saving
     (TZ_Type : Timezone_Type_Record) return Boolean is
     (TZ_Type.Is_DST);

   --  Check if this is standard time (not DST)
   function Is_Standard_Time (TZ_Type : Timezone_Type_Record) return Boolean is
     (not TZ_Type.Is_DST);

   --  Get abbreviation as string
   function Get_Abbreviation (TZ_Type : Timezone_Type_Record) return String is
     (Abbreviation_Strings.To_String (TZ_Type.Abbreviation));

   --  Check if abbreviation matches given string
   function Has_Abbreviation
     (TZ_Type : Timezone_Type_Record; Abbrev : String) return Boolean is
     (Get_Abbreviation (TZ_Type) = Abbrev);

end TZif.Domain.Value_Object.Timezone_Type;
