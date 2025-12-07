pragma Ada_2022;
--  ===========================================================================
--  TZif.Application.Port.Inbound.Validate_Source
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Inbound port for Validate Source use case.
--
--  Architecture:
--    Application layer port (hexagonal architecture).
--    Defines interface for external actors to trigger use cases.
--
--  Key Types:
--    Path_String
--    Validation_Result
--
--  Dependencies:
--    TZif.Domain.Error.Result
--    Preelaborate
--
--  ===========================================================================

with Ada.Strings.Bounded;
with TZif.Domain.Error.Result;

package TZif.Application.Port.Inbound.Validate_Source with
  Preelaborate
is

   --  ========================================================================
   --  Canonical Types (GPT-5 Pattern: defined ONCE, used everywhere)
   --  ========================================================================

   --  Filesystem path (bounded, max 4096 chars)
   Max_Path_Length : constant := 4_096;
   package Path_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max_Path_Length);
   subtype Path_String is Path_Strings.Bounded_String;

   --  Result type: Result[Boolean] (True = valid, False = invalid)
   package Validation_Result_Package is new Domain.Error.Result.Generic_Result
     (T => Boolean);
   subtype Validation_Result is Validation_Result_Package.Result;

   --  ========================================================================
   --  Port Contract Documentation
   --  ========================================================================
   --
   --  The Execute function signature (implemented by use case generic):
   --
   --    function Execute
   --      (Path : Path_String)
   --      return Validation_Result;
   --
   --  Parameters:
   --    Path - Filesystem path to validate
   --
   --  Returns:
   --    Ok(True) - Path is valid: exists, is_directory, not_symlink, readable
   --    Ok(False) - Path failed validation checks
   --    Err(IOError) - Filesystem error during validation
   --
   --  Performance: O(1) - single stat call

end TZif.Application.Port.Inbound.Validate_Source;
