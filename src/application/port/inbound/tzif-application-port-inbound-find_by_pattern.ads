pragma Ada_2022;
--  ===========================================================================
--  TZif.Application.Port.Inbound.Find_By_Pattern
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Inbound port for Find By Pattern use case.
--
--  Architecture:
--    Application layer port (hexagonal architecture).
--    Defines interface for external actors to trigger use cases.
--
--  Key Types:
--    Pattern_String
--    Zone_Name_String
--    Yield_Callback_Access
--    Find_By_Pattern_Result
--
--  Dependencies:
--    TZif.Domain.Error.Result
--    TZif.Domain.Value_Object.Unit
--    Preelaborate
--
--  ===========================================================================

with Ada.Strings.Bounded;
with TZif.Domain.Error.Result;
with TZif.Domain.Value_Object.Unit;

package TZif.Application.Port.Inbound.Find_By_Pattern with
  Preelaborate
is

   use TZif.Domain.Value_Object.Unit;

   --  ========================================================================
   --  Canonical Types (GPT-5 Pattern: defined ONCE, used everywhere)
   --  ========================================================================

   --  Search pattern (bounded, max 256 chars)
   Max_Pattern_Length : constant := 256;
   package Pattern_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max_Pattern_Length);
   subtype Pattern_String is Pattern_Strings.Bounded_String;

   --  Zone name for yield callback (bounded, max 256 chars)
   Max_Zone_Name_Length : constant := 256;
   package Zone_Name_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max_Zone_Name_Length);
   subtype Zone_Name_String is Zone_Name_Strings.Bounded_String;

   --  Yield callback type (GPT-5: Named access type)
   type Yield_Callback_Access is
     not null access procedure (Name : Zone_Name_String);

   --  Result type: Result[Unit] (side effect operation)
   package Find_By_Pattern_Result_Package is new Domain.Error.Result
     .Generic_Result
     (T => Unit_Type);
   subtype Find_By_Pattern_Result is Find_By_Pattern_Result_Package.Result;

   --  ========================================================================
   --  Port Contract Documentation
   --  ========================================================================
   --
   --  The Execute function signature (implemented by use case generic):
   --
   --    function Execute
   --      (Pattern : Pattern_String;
   --       Yield   : Yield_Callback_Access)
   --      return Find_By_Pattern_Result;
   --
   --  Parameters:
   --    Pattern - Search pattern (substring match)
   --    Yield - Callback procedure called for each matching zone
   --
   --  Returns:
   --    Ok(Unit) - Successfully yielded all matching zones
   --    Err(IOError) - Filesystem error during scan
   --
   --  Performance: O(n) linear scan

end TZif.Application.Port.Inbound.Find_By_Pattern;
