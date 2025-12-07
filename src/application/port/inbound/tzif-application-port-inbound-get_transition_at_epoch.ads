pragma Ada_2022;
--  ===========================================================================
--  TZif.Application.Port.Inbound.Get_Transition_At_Epoch
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Inbound port for Get Transition At Epoch use case.
--
--  Architecture:
--    Application layer port (hexagonal architecture).
--    Defines interface for external actors to trigger use cases.
--
--  Key Types:
--    Zone_Id_String
--    Get_Transition_Result
--
--  Dependencies:
--    TZif.Domain.Error.Result
--    TZif.Domain.Value_Object.Transition_Info
--    Preelaborate
--
--  ===========================================================================

with Ada.Strings.Bounded;
with TZif.Domain.Error.Result;
with TZif.Domain.Value_Object.Transition_Info;

package TZif.Application.Port.Inbound.Get_Transition_At_Epoch with
  Preelaborate
is

   use TZif.Domain.Value_Object.Transition_Info;

   --  ========================================================================
   --  Canonical Types (GPT-5 Pattern: defined ONCE, used everywhere)
   --  ========================================================================

   --  Zone ID string (bounded, max 256 chars)
   Max_Zone_Id_Length : constant := 256;
   package Zone_Id_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max_Zone_Id_Length);
   subtype Zone_Id_String is Zone_Id_Strings.Bounded_String;

   --  Result type: Result[Transition_Info_Type]
   package Get_Transition_Result_Package is new Domain.Error.Result
     .Generic_Result
     (T => Transition_Info_Type);
   subtype Get_Transition_Result is Get_Transition_Result_Package.Result;

   --  ========================================================================
   --  Port Contract Documentation
   --  ========================================================================
   --
   --  The Execute function signature (implemented by use case generic):
   --
   --    function Execute
   --      (Id    : Zone_Id_String;
   --       Epoch : Epoch_Seconds_Type)
   --      return Get_Transition_Result;
   --
   --  Parameters:
   --    Id - Zone identifier (e.g., "America/Phoenix")
   --    Epoch - Unix timestamp
   --
   --  Returns:
   --    Ok(Transition_Info) - Offset and other transition data at that time
   --    Err(ZoneNotFound) - Zone doesn't exist
   --    Err(IOError) - Filesystem error
   --
   --  Performance: O(log n) binary search in transitions

end TZif.Application.Port.Inbound.Get_Transition_At_Epoch;
