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
--  Dependencies:
--    TZif.Domain.Value_Object.Source_Info
--
--  ===========================================================================

with TZif.Domain.Value_Object.Source_Info;

package TZif.Infrastructure.Cache.Path_Canonical is

   use TZif.Domain.Value_Object.Source_Info;

   --  Wrapper for Infrastructure.Paths.Canonical using Path_String_Type
   package Path_Canon_Impl is
      function Canonicalize
        (P           : Path_String_Type; Case_Insensitive : Boolean := False;
         Slash_Style : Character := '/') return Path_String_Type;
   end Path_Canon_Impl;

   --  Re-export the Canonicalize function for convenience
   function Canonicalize
     (P           : Path_String_Type; Case_Insensitive : Boolean := False;
      Slash_Style : Character := '/') return Path_String_Type renames
     Path_Canon_Impl.Canonicalize;

end TZif.Infrastructure.Cache.Path_Canonical;
