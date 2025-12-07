pragma Ada_2022;
--  ===========================================================================
--  TZif.Application.Port.Inbound.Get_Version
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Inbound port for Get Version use case.
--
--  Architecture:
--    Application layer port (hexagonal architecture).
--    Defines interface for external actors to trigger use cases.
--
--  Key Types:
--    Version_String
--    Version_Result
--
--  Dependencies:
--    TZif.Domain.Error.Result
--    Preelaborate
--
--  ===========================================================================

with Ada.Strings.Bounded;
with TZif.Domain.Error.Result;

package TZif.Application.Port.Inbound.Get_Version with
  Preelaborate
is

   --  ========================================================================
   --  Canonical Types (GPT-5 Pattern: defined ONCE, used everywhere)
   --  ========================================================================

   --  Version string (bounded, max 32 chars for versions like "2024b")
   Max_Version_Length : constant := 32;
   package Version_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max_Version_Length);
   subtype Version_String is Version_Strings.Bounded_String;

   --  Result type: Result[Version_String]
   package Version_Result_Package is new Domain.Error.Result.Generic_Result
     (T => Version_String);
   subtype Version_Result is Version_Result_Package.Result;

   --  ========================================================================
   --  Port Contract Documentation
   --  ========================================================================
   --
   --  The Execute function signature (implemented by use case generic):
   --
   --    function Execute
   --      (Source : Source_Info_Type)
   --      return Version_Result;
   --
   --  Parameters:
   --    Source - The timezone database source to query
   --
   --  Returns:
   --    Ok(Version_String) - Version like "2024b", "2023c"
   --    Err(FileNotFound) - No +VERSION file found
   --    Err(IOError) - Filesystem error
   --
   --  Performance: O(1) file read (cached)

end TZif.Application.Port.Inbound.Get_Version;
