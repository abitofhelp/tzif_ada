pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.Paths.Canonical.Generic
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Generic implementation.
--
--  ===========================================================================

with TZif.Infrastructure.Paths.Canonical;

package body TZif.Infrastructure.Paths.Canonical.Generic is

   function Canonicalize
     (P                : Path_String_Type;
      Case_Insensitive : Boolean;
      Slash_Style      : Character)
      return Path_String_Type
   is
      S : constant String := Infrastructure.Paths.Canonical.Canonicalize
        (To_String (P), Case_Insensitive, Slash_Style);
   begin
      return Of_String (S);
   end Canonicalize;

end TZif.Infrastructure.Paths.Canonical.Generic;
