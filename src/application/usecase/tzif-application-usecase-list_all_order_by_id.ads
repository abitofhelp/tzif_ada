pragma Ada_2022;
--  ===========================================================================
--  TZif.Application.Usecase.List_All_Order_By_Id
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    List All Order By Id use case implementation.
--
--  Architecture:
--    Application layer use case (hexagonal architecture).
--    Orchestrates domain logic via ports.
--
--  Dependencies:
--    TZif.Application.Port.Inbound.List_All_Order_By_Id
--    TZif.Domain.Value_Object.Source_Info
--    Preelaborate
--
--  ===========================================================================

with TZif.Application.Port.Inbound.List_All_Order_By_Id;
with TZif.Domain.Value_Object.Source_Info;

package TZif.Application.Usecase.List_All_Order_By_Id with
  Preelaborate
is

   use TZif.Application.Port.Inbound.List_All_Order_By_Id;
   use TZif.Domain.Value_Object.Source_Info;

   --  ========================================================================
   --  Use Case Implementation (Generic over Repository) - GPT-5 Pattern
   --  ========================================================================

   generic
      with function Repository_List_All_Order_By_Id
        (Source : Source_Info_Type; Descending : Boolean)
         return List_All_Zones_Result;

   package Use_Case is

      function Execute
        (Source : Source_Info_Type; Descending : Boolean)
         return List_All_Zones_Result renames
        Repository_List_All_Order_By_Id;

   end Use_Case;

end TZif.Application.Usecase.List_All_Order_By_Id;
