pragma Ada_2022;
--  ===========================================================================
--  TZif.Application.Usecase.Get_Transition_At_Epoch
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Get Transition At Epoch use case implementation.
--
--  Architecture:
--    Application layer use case (hexagonal architecture).
--    Orchestrates domain logic via ports.
--
--  Dependencies:
--    TZif.Application.Port.Inbound.Get_Transition_At_Epoch
--    TZif.Domain.Value_Object.Epoch_Seconds
--    Preelaborate
--
--  ===========================================================================

with TZif.Application.Port.Inbound.Get_Transition_At_Epoch;
with TZif.Domain.Value_Object.Epoch_Seconds;

package TZif.Application.Usecase.Get_Transition_At_Epoch with
  Preelaborate
is

   use TZif.Application.Port.Inbound.Get_Transition_At_Epoch;
   use TZif.Domain.Value_Object.Epoch_Seconds;

   --  ========================================================================
   --  Use Case Implementation (Generic over Repository) - GPT-5 Pattern
   --  ========================================================================

   generic
      with function Repository_Get_Transition_At_Epoch
        (Id : Zone_Id_String; Epoch : Epoch_Seconds_Type)
         return Get_Transition_Result;

   package Use_Case is

      function Execute
        (Id : Zone_Id_String; Epoch : Epoch_Seconds_Type)
         return Get_Transition_Result renames
        Repository_Get_Transition_At_Epoch;

   end Use_Case;

end TZif.Application.Usecase.Get_Transition_At_Epoch;
