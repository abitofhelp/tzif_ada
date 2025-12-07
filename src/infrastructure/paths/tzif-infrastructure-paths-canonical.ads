pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.Paths.Canonical
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Canonical interface and type definitions.
--
--  ===========================================================================

package TZif.Infrastructure.Paths.Canonical is

   --  Canonicalize a filesystem path string
   --
   --  Parameters:
   --    S                - Input path string
   --    Case_Insensitive - If True, converts to lowercase (default: False)
   --    Slash_Style      - Separator character to use (default: '/')
   --
   --  Returns: Canonicalized path string
   function Canonicalize
     (S           : String; Case_Insensitive : Boolean := False;
      Slash_Style : Character := '/') return String;

end TZif.Infrastructure.Paths.Canonical;
