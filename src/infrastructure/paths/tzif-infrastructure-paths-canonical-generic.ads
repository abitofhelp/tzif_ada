pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.Paths.Canonical.Generic
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Generic interface and type definitions.
--
--  Key Types:
--    Path_String_Type
--
--  Dependencies:
--    function To_String (P : Path_String_Type) return String is <>
--    function Of_String (S : String) return Path_String_Type
--    Preelaborate
--
--  ===========================================================================

generic
   type Path_String_Type is private;
   with function To_String (P : Path_String_Type) return String is <>;
   with function Of_String (S : String) return Path_String_Type;
package TZif.Infrastructure.Paths.Canonical.Generic
  with Preelaborate
is

   --  Canonicalize a path of type Path_String_Type
   --
   --  Parameters:
   --    P                - Input path
   --    Case_Insensitive - If True, converts to lowercase (default: False)
   --    Slash_Style      - Separator character to use (default: '/')
   --
   --  Returns: Canonicalized path
   function Canonicalize
     (P                : Path_String_Type;
      Case_Insensitive : Boolean  := False;
      Slash_Style      : Character := '/')
      return Path_String_Type;

end TZif.Infrastructure.Paths.Canonical.Generic;
