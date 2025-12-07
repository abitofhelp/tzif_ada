pragma Ada_2022;
--  ===========================================================================
--  TZif.Domain.Value_Object.Epoch_Seconds
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Epoch Seconds value object - immutable domain data.
--
--  Responsibilities:
--    - Define Epoch Seconds type and operations
--    - Provide constructors and accessors
--
--  Key Types:
--    Epoch_Seconds_Type
--    Epoch_Seconds_32_Type
--
--  Dependencies:
--    Pure
--    Pre => Fits_In_32_Bit (Value)
--
--  ===========================================================================

package TZif.Domain.Value_Object.Epoch_Seconds with
  Pure
is

   --  ========================================================================
   --  Epoch Time Types
   --  ========================================================================

   --  64-bit epoch timestamp (seconds since 1970-01-01 00:00:00 UTC)
   --  Range: -2^63 to 2^63-1
   type Epoch_Seconds_Type is range -2**63 .. 2**63 - 1;

   --  32-bit epoch timestamp (for TZif version 1 compatibility)
   --  Range: 1901-12-13 20:45:52 to 2038-01-19 03:14:07 UTC
   type Epoch_Seconds_32_Type is range -2**31 .. 2**31 - 1;

   --  ========================================================================
   --  Constants
   --  ========================================================================

   --  Epoch zero point: 1970-01-01 00:00:00 UTC
   Epoch_Zero : constant Epoch_Seconds_Type := 0;

   --  32-bit limits (Y2038 problem boundaries)
   Epoch_Seconds_32_Min : constant Epoch_Seconds_Type :=
     Epoch_Seconds_Type (Epoch_Seconds_32_Type'First);
   Epoch_Seconds_32_Max : constant Epoch_Seconds_Type :=
     Epoch_Seconds_Type (Epoch_Seconds_32_Type'Last);

   --  ========================================================================
   --  Conversion Functions
   --  ========================================================================

   --  Convert 32-bit to 64-bit epoch time
   function To_Epoch_Seconds
     (Value : Epoch_Seconds_32_Type) return Epoch_Seconds_Type is
     (Epoch_Seconds_Type (Value));

   --  Check if 64-bit time fits in 32-bit range
   function Fits_In_32_Bit (Value : Epoch_Seconds_Type) return Boolean is
     (Value in Epoch_Seconds_32_Min .. Epoch_Seconds_32_Max);

   --  Convert 64-bit to 32-bit (unsafe if out of range)
   function To_Epoch_Seconds_32
     (Value : Epoch_Seconds_Type) return Epoch_Seconds_32_Type is
     (Epoch_Seconds_32_Type (Value)) with
     Pre => Fits_In_32_Bit (Value);

end TZif.Domain.Value_Object.Epoch_Seconds;
