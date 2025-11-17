pragma Ada_2022;
--  ===========================================================================
--  Tzif.Application.Usecase.Export_Cache
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Export Cache use case implementation.
--
--  Architecture:
--    Application layer use case (hexagonal architecture).
--    Orchestrates domain logic via ports.
--
--  Dependencies:
--    TZif.Application.Port.Inbound.Export_Cache
--    Preelaborate
--
--  ===========================================================================

with TZif.Application.Port.Inbound.Export_Cache;

package TZif.Application.Usecase.Export_Cache with
  Preelaborate
is

   use TZif.Application.Port.Inbound.Export_Cache;

   --  ========================================================================
   --  Use Case Implementation (Generic over Repository) - GPT-5 Pattern
   --  ========================================================================

   generic
      with function Repository_Export_Cache
        (Path : Path_String; Overwrite : Boolean) return Export_Cache_Result;

   package Use_Case is

      function Execute
        (Path : Path_String; Overwrite : Boolean)
         return Export_Cache_Result renames
        Repository_Export_Cache;

   end Use_Case;

end TZif.Application.Usecase.Export_Cache;
