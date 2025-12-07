pragma Ada_2022;
--  ===========================================================================
--  TZif.Application.Port.Inbound.Find_By_Id
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Inbound port for Find By Id use case.
--
--  Architecture:
--    Application layer port (hexagonal architecture).
--    Defines interface for external actors to trigger use cases.
--
--  Key Types:
--    Zone_Id_Input_Type
--    Find_By_Id_Result_Type
--
--  Dependencies:
--    TZif.Domain.Error.Result
--    TZif.Domain.Entity.Zone
--    TZif.Domain.Value_Object.Zone_Id
--
--  ===========================================================================

with TZif.Domain.Error.Result;
with TZif.Domain.Entity.Zone;
with TZif.Domain.Value_Object.Zone_Id;

package TZif.Application.Port.Inbound.Find_By_Id with
  Preelaborate
is

   use TZif.Domain.Entity.Zone;
   use TZif.Domain.Value_Object.Zone_Id;

   --  ========================================================================
   --  Canonical Types (GPT-5 Pattern: defined ONCE, used everywhere)
   --  ========================================================================

   --  Input type: Zone ID from domain value object
   subtype Zone_Id_Input_Type is Zone_Id_Type;

   --  Result type: Result[Zone_Type]
   package Find_By_Id_Result is new Domain.Error.Result.Generic_Result
     (T => Zone_Type);
   subtype Find_By_Id_Result_Type is Find_By_Id_Result.Result;

   --  ========================================================================
   --  Port Contract Documentation
   --  ========================================================================
   --
   --  The Execute function signature (implemented by use case generic):
   --
   --    function Execute
   --      (Id : Zone_Id_Input_Type)
   --      return Find_By_Id_Result_Type;
   --
   --  Parameters:
   --    Id - Zone identifier (e.g., "America/Phoenix", "UTC")
   --
   --  Returns:
   --    Ok(Zone) - Successfully found and loaded zone
   --    Err(ZoneNotFound) - Zone ID doesn't exist
   --    Err(ParseError) - TZif file corrupt/invalid
   --    Err(IOError) - Filesystem error
   --
   --  Performance: O(1) hash lookup + O(1) cached parse

end TZif.Application.Port.Inbound.Find_By_Id;
