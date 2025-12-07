pragma Ada_2022;
--  ===========================================================================
--  TZif.Application.Usecase.Load_Source
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Load Source use case implementation.
--
--  Architecture:
--    Application layer use case (hexagonal architecture).
--    Orchestrates domain logic via ports.
--
--  Dependencies:
--    TZif.Application.Port.Inbound.Load_Source
--    Preelaborate
--
--  ===========================================================================

with TZif.Application.Port.Inbound.Load_Source;

package TZif.Application.Usecase.Load_Source with
  Preelaborate
is

   use TZif.Application.Port.Inbound.Load_Source;

   --  ========================================================================
   --  Use Case Implementation (Generic over Repository) - GPT-5 Pattern
   --  ========================================================================

   generic
      with function Repository_Load_Source
        (Path : Path_String) return Load_Source_Result;

   package Use_Case is

      function Execute (Path : Path_String) return Load_Source_Result renames
        Repository_Load_Source;

   end Use_Case;

end TZif.Application.Usecase.Load_Source;
