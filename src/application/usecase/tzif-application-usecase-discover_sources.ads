pragma Ada_2022;
--  ===========================================================================
--  TZif.Application.Usecase.Discover_Sources
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Discover Sources use case implementation.
--
--  Architecture:
--    Application layer use case (hexagonal architecture).
--    Orchestrates domain logic via ports.
--
--  Dependencies:
--    TZif.Application.Port.Inbound.Discover_Sources
--    Preelaborate
--
--  ===========================================================================

with TZif.Application.Port.Inbound.Discover_Sources;

package TZif.Application.Usecase.Discover_Sources with
  Preelaborate
is

   use TZif.Application.Port.Inbound.Discover_Sources;

   --  ========================================================================
   --  Use Case Implementation (Generic over Repository) - GPT-5 Pattern
   --  ========================================================================

   --  Generic over repository implementation.
   --  The repository MUST:
   --    - Take Path_List parameter (developer-specified paths)
   --    - Return the port's Discovery_Result
   generic
      with function Repository_Discover_Sources
        (Search_Paths : Path_List) return Discovery_Result;

   package Use_Case is

      --  Implementation of the port contract:
      --  Execute is just the bound repository function (zero overhead rename)
      function Execute
        (Search_Paths : Path_List) return Discovery_Result renames
        Repository_Discover_Sources;

   end Use_Case;

end TZif.Application.Usecase.Discover_Sources;
