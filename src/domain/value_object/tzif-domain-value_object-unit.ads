pragma Ada_2022;
--  ===========================================================================
--  TZif.Domain.Value_Object.Unit
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Unit value object - immutable domain data.
--
--  Responsibilities:
--    - Define Unit type and operations
--
--  Key Types:
--    Unit_Type
--
--  Dependencies:
--    Pure
--
--  ===========================================================================

package TZif.Domain.Value_Object.Unit with
  Pure
is

   --  ========================================================================
   --  Unit Type (Zero-Size Type)
   --  ========================================================================

   --  Unit type representing "no value"
   --  Used for side-effect operations that don't return a meaningful value
   type Unit_Type is null record;

   --  Singleton unit value (like () in functional languages)
   Unit : constant Unit_Type := (null record);

end TZif.Domain.Value_Object.Unit;
