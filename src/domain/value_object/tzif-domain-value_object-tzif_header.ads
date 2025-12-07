pragma Ada_2022;
--  ===========================================================================
--  TZif.Domain.Value_Object.TZif_Header
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    TZif Header value object - immutable domain data.
--
--  Responsibilities:
--    - Define TZif Header type and operations
--    - Provide constructors and accessors
--
--  Key Types:
--    TZif_Version_Type
--    TZif_Header_Type
--
--  Dependencies:
--    TZif_Config
--    Pure
--
--  ===========================================================================

with TZif_Config;

package TZif.Domain.Value_Object.TZif_Header with
  Pure
is

   --  ========================================================================
   --  TZif Version
   --  ========================================================================

   type TZif_Version_Type is (Version_1, Version_2, Version_3, Version_4);

   --  ========================================================================
   --  TZif Header Record
   --  ========================================================================

   type TZif_Header_Type is record
      --  File format version
      Version : TZif_Version_Type := Version_1;

      --  Number of UT/local indicators (should equal typecnt)
      UTC_Local_Count : Natural range 0 .. TZif_Config.Max_Types_Per_Zone := 0;

      --  Number of standard/wall indicators (should equal typecnt)
      Standard_Wall_Count : Natural range 0 ..
          TZif_Config.Max_Types_Per_Zone :=
        0;

      --  Number of leap second records
      Leap_Count : Natural range 0 .. TZif_Config.Max_Leap_Seconds := 0;

      --  Number of transition times
      Transition_Count : Natural range 0 ..
          TZif_Config.Max_Transitions_Per_Zone :=
        0;

      --  Number of local time type records
      Type_Count : Natural range 0 .. TZif_Config.Max_Types_Per_Zone := 0;

      --  Number of time zone abbreviation characters
      Abbrev_Chars : Natural range 0 ..
          TZif_Config.Max_Abbreviation_Length *
          TZif_Config.Max_Types_Per_Zone :=
        0;
   end record;

   --  ========================================================================
   --  Constants
   --  ========================================================================

   --  TZif magic number: "TZif" (0x545A6966)
   TZif_Magic        : constant String := "TZif";
   TZif_Magic_Length : constant        := 4;

   --  Header size in bytes
   TZif_Header_Size : constant := 44;

   --  ========================================================================
   --  Query Functions
   --  ========================================================================

   --  Check if header represents version 1 (32-bit timestamps)
   function Is_Version_1 (Header : TZif_Header_Type) return Boolean is
     (Header.Version = Version_1);

   --  Check if header represents version 2+ (64-bit timestamps)
   function Is_Version_2_Or_Later (Header : TZif_Header_Type) return Boolean is
     (Header.Version in Version_2 | Version_3 | Version_4);

   --  Check if header has leap second data
   function Has_Leap_Seconds (Header : TZif_Header_Type) return Boolean is
     (Header.Leap_Count > 0);

   --  Check if header has transition data
   function Has_Transitions (Header : TZif_Header_Type) return Boolean is
     (Header.Transition_Count > 0);

end TZif.Domain.Value_Object.TZif_Header;
