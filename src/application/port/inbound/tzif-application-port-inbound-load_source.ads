pragma Ada_2022;
--  ===========================================================================
--  TZif.Application.Port.Inbound.Load_Source
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Inbound port for Load Source use case.
--
--  Architecture:
--    Application layer port (hexagonal architecture).
--    Defines interface for external actors to trigger use cases.
--
--  Key Types:
--    Path_String
--    Load_Source_Result
--
--  Dependencies:
--    TZif.Domain.Error.Result
--    TZif.Domain.Value_Object.Source_Info
--    Preelaborate
--
--  ===========================================================================

with Ada.Strings.Bounded;
with TZif.Domain.Error.Result;
with TZif.Domain.Value_Object.Source_Info;

package TZif.Application.Port.Inbound.Load_Source with
  Preelaborate
is

   use TZif.Domain.Value_Object.Source_Info;

   --  ========================================================================
   --  Canonical Types (GPT-5 Pattern: defined ONCE, used everywhere)
   --  ========================================================================

   --  Filesystem path (bounded, max 4096 chars)
   Max_Path_Length : constant := 4_096;
   package Path_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max_Path_Length);
   subtype Path_String is Path_Strings.Bounded_String;

   --  Result type: Result[Source_Info_Type]
   package Load_Source_Result_Package is new Domain.Error.Result.Generic_Result
     (T => Source_Info_Type);
   subtype Load_Source_Result is Load_Source_Result_Package.Result;

   --  ========================================================================
   --  Port Contract Documentation
   --  ========================================================================
   --
   --  The Execute function signature (implemented by use case generic):
   --
   --    function Execute
   --      (Path : Path_String)
   --      return Load_Source_Result;
   --
   --  Parameters:
   --    Path - Filesystem path to timezone database source
   --
   --  Returns:
   --    Ok(Source_Info) - Source metadata loaded
   --    Err(NotFound) - Path doesn't exist
   --    Err(IOError) - Filesystem error
   --
   --  Performance: O(1) metadata read

end TZif.Application.Port.Inbound.Load_Source;
