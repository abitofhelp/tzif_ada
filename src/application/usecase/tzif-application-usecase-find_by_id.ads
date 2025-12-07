pragma Ada_2022;
--  ===========================================================================
--  TZif.Application.Usecase.Find_By_Id
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Find By Id use case implementation.
--
--  Architecture:
--    Application layer use case (hexagonal architecture).
--    Orchestrates domain logic via ports.
--
--  Dependencies:
--    TZif.Application.Port.Inbound.Find_By_Id
--    Preelaborate
--
--  ===========================================================================

with TZif.Application.Port.Inbound.Find_By_Id;

package TZif.Application.Usecase.Find_By_Id with
  Preelaborate
is

   use TZif.Application.Port.Inbound.Find_By_Id;

   --  ========================================================================
   --  Use Case Implementation (Generic over Repository) - GPT-5 Pattern
   --  ========================================================================

   --  Generic over repository implementation.
   --  The repository MUST:
   --    - Take the port's Zone_Id_Input_Type
   --    - Return the port's Find_By_Id_Result_Type
   generic
      with function Repository_Find_By_Id
        (Id : Zone_Id_Input_Type) return Find_By_Id_Result_Type;

   package Use_Case is

      --  Implementation of the port contract:
      --  Execute is just the bound repository function (zero overhead rename)
      function Execute
        (Id : Zone_Id_Input_Type) return Find_By_Id_Result_Type renames
        Repository_Find_By_Id;

   end Use_Case;

end TZif.Application.Usecase.Find_By_Id;
