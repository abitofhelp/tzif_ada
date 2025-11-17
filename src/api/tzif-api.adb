pragma Ada_2022;
--  ===========================================================================
--  Tzif.Api
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Api implementation.
--
--  ===========================================================================

package body TZif.API is

   --  ========================================================================
   --  Helper Functions
   --  ========================================================================

   function Make_Zone_Id_String (Id : String) return Zone_Id_String is
   begin
      return Get_Transition_Port.Zone_Id_Strings.To_Bounded_String (Id);
   end Make_Zone_Id_String;

end TZif.API;
