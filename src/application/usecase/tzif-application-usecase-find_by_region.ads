pragma Ada_2022;
--  ===========================================================================
--  TZif.Application.Usecase.Find_By_Region
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Find By Region use case implementation.
--
--  Architecture:
--    Application layer use case (hexagonal architecture).
--    Orchestrates domain logic via ports.
--
--  Dependencies:
--    TZif.Application.Port.Inbound.Find_By_Region
--    Preelaborate
--
--  ===========================================================================

with TZif.Application.Port.Inbound.Find_By_Region;

package TZif.Application.Usecase.Find_By_Region with
  Preelaborate
is

   use TZif.Application.Port.Inbound.Find_By_Region;

   --  ========================================================================
   --  Use Case Implementation (Generic over Repository) - GPT-5 Pattern
   --  ========================================================================

   --  Generic over repository implementation.
   --  The repository MUST:
   --    - Take the port's Region_String and Yield_Callback_Access
   --    - Return the port's Find_By_Region_Result
   generic
      with function Repository_Find_By_Region
        (Region : Region_String; Yield : Yield_Callback_Access)
         return Find_By_Region_Result;

   package Use_Case is

      --  Implementation of the port contract:
      --  Execute is just the bound repository function (zero overhead rename)
      function Execute
        (Region : Region_String; Yield : Yield_Callback_Access)
         return Find_By_Region_Result renames
        Repository_Find_By_Region;

   end Use_Case;

end TZif.Application.Usecase.Find_By_Region;
