pragma Ada_2022;
--  ===========================================================================
--  TZif.Application.Usecase.Validate_Source
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Validate Source use case implementation.
--
--  Architecture:
--    Application layer use case (hexagonal architecture).
--    Orchestrates domain logic via ports.
--
--  Dependencies:
--    TZif.Application.Port.Inbound.Validate_Source
--    Preelaborate
--
--  ===========================================================================

with TZif.Application.Port.Inbound.Validate_Source;

package TZif.Application.Usecase.Validate_Source with
  Preelaborate
is

   use TZif.Application.Port.Inbound.Validate_Source;

   --  ========================================================================
   --  Use Case Implementation (Generic over Repository) - GPT-5 Pattern
   --  ========================================================================

   --  Generic over repository implementation.
   --  The repository MUST:
   --    - Take the port's Path_String
   --    - Return the port's Validation_Result
   generic
      with function Repository_Validate_Source
        (Path : Path_String) return Validation_Result;

   package Use_Case is

      --  Implementation of the port contract:
      --  Execute is just the bound repository function (zero overhead rename)
      function Execute (Path : Path_String) return Validation_Result renames
        Repository_Validate_Source;

   end Use_Case;

end TZif.Application.Usecase.Validate_Source;
