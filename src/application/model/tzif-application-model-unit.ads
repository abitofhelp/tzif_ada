pragma Ada_2022;
--  ===========================================================================
--  TZif.Application.Model.Unit
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Unit interface and type definitions.
--
--  Key Types:
--    Unit
--
--  Dependencies:
--    Preelaborate
--
--  ===========================================================================

package TZif.Application.Model.Unit with
  Preelaborate
is

   --  ========================================================================
   --  Unit Type: Represents "no meaningful value"
   --  ========================================================================

   --  Used when we need Result[T] but T carries no information
   type Unit is null record;

   --  Singleton instance for convenience
   Unit_Value : constant Unit := (null record);

end TZif.Application.Model.Unit;
