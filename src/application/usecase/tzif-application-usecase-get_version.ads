pragma Ada_2022;
--  ===========================================================================
--  TZif.Application.Usecase.Get_Version
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Get Version use case implementation.
--
--  Architecture:
--    Application layer use case (hexagonal architecture).
--    Orchestrates domain logic via ports.
--
--  Dependencies:
--    TZif.Application.Port.Inbound.Get_Version
--    TZif.Domain.Value_Object.Source_Info
--    Preelaborate
--
--  ===========================================================================

with TZif.Application.Port.Inbound.Get_Version;
with TZif.Domain.Value_Object.Source_Info;

package TZif.Application.Usecase.Get_Version with
  Preelaborate
is

   use TZif.Application.Port.Inbound.Get_Version;
   use TZif.Domain.Value_Object.Source_Info;

   --  ========================================================================
   --  Use Case Implementation (Generic over Repository) - GPT-5 Pattern
   --  ========================================================================

   generic
      with function Repository_Get_Version
        (Source : Source_Info_Type) return Version_Result;

   package Use_Case is

      function Execute
        (Source : Source_Info_Type) return Version_Result renames
        Repository_Get_Version;

   end Use_Case;

end TZif.Application.Usecase.Get_Version;
