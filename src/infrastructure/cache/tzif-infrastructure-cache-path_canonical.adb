pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.Cache.Path_Canonical
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Path Canonical for performance optimization.
--
--  ===========================================================================

with TZif.Infrastructure.Paths.Canonical;

package body TZif.Infrastructure.Cache.Path_Canonical is

   package body Path_Canon_Impl is
      function Canonicalize
        (P           : Path_String_Type; Case_Insensitive : Boolean := False;
         Slash_Style : Character := '/') return Path_String_Type
      is
         S : constant String :=
           Infrastructure.Paths.Canonical.Canonicalize
             (To_String (P), Case_Insensitive, Slash_Style);
      begin
         return Make_Path (S);
      end Canonicalize;
   end Path_Canon_Impl;

end TZif.Infrastructure.Cache.Path_Canonical;
