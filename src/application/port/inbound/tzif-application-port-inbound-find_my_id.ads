pragma Ada_2022;
--  ===========================================================================
--  TZif.Application.Port.Inbound.Find_My_Id
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Inbound port for Find My Id use case.
--
--  Architecture:
--    Application layer port (hexagonal architecture).
--    Defines interface for external actors to trigger use cases.
--
--  Key Types:
--    Zone_Id_String
--    Result
--
--  Dependencies:
--    TZif.Domain.Error.Result
--    TZif.Domain.Value_Object.Zone_Id
--    Preelaborate
--
--  ===========================================================================

with TZif.Domain.Error.Result;
with TZif.Domain.Value_Object.Zone_Id;

package TZif.Application.Port.Inbound.Find_My_Id with
  Preelaborate
is

   --  ========================================================================
   --  Canonical Types (from Domain)
   --  ========================================================================

   --  Zone ID bounded string from Domain
   subtype Zone_Id_String is TZif.Domain.Value_Object.Zone_Id.Zone_Id_String;

   --  ========================================================================
   --  Canonical Result Instantiation (SINGLE SOURCE OF TRUTH)
   --  ========================================================================

   --  Single instantiation of Generic_Result for this port
   --  Both use cases and adapters use THIS EXACT TYPE
   package Result_Zone_Id is new Domain.Error.Result.Generic_Result
     (T => Zone_Id_String);

   subtype Result is Result_Zone_Id.Result;

end TZif.Application.Port.Inbound.Find_My_Id;
