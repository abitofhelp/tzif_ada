pragma Ada_2022;
--  ===========================================================================
--  TZif.Application.Usecase.Find_My_Id
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Find My Id use case implementation.
--
--  Architecture:
--    Application layer use case (hexagonal architecture).
--    Orchestrates domain logic via ports.
--
--  Dependencies:
--    TZif.Application.Port.Inbound.Find_My_Id
--    Preelaborate
--
--  ===========================================================================

with TZif.Application.Port.Inbound.Find_My_Id;

package TZif.Application.Usecase.Find_My_Id with
  Preelaborate
is

   --  ========================================================================
   --  Use Case Implementation (Generic over Repository)
   --  ========================================================================

   generic
      --  Repository function MUST return the port's canonical Result type
      with function Repository_Find_My_Id
         return TZif.Application.Port.Inbound.Find_My_Id.Result;

   package Use_Case is

      --  Execute the use case: Find local system timezone ID
      --
      --  Returns:
      --    Ok(Zone_Id_String) - Local timezone (e.g., "America/Phoenix")
      --    Err(Not_Found_Error) - Timezone link not found
      --    Err(IO_Error) - Cannot read timezone configuration
      --
      --  Implementation:
      --    Static dispatch to injected repository function
      function Execute
         return Application.Port.Inbound.Find_My_Id.Result renames
        Repository_Find_My_Id;

   end Use_Case;

end TZif.Application.Usecase.Find_My_Id;
