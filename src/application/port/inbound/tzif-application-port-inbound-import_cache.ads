pragma Ada_2022;
--  ===========================================================================
--  Tzif.Application.Port.Inbound.Import_Cache
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Inbound port for Import Cache use case.
--
--  Architecture:
--    Application layer port (hexagonal architecture).
--    Defines interface for external actors to trigger use cases.
--
--  Key Types:
--    Path_String
--    Import_Cache_Result
--
--  Dependencies:
--    TZif.Domain.Error.Result
--    TZif.Domain.Value_Object.Cache_Stats
--    Preelaborate
--
--  ===========================================================================

with Ada.Strings.Bounded;
with TZif.Domain.Error.Result;
with TZif.Domain.Value_Object.Cache_Stats;

package TZif.Application.Port.Inbound.Import_Cache with
  Preelaborate
is

   use TZif.Domain.Value_Object.Cache_Stats;

   --  ========================================================================
   --  Canonical Types (GPT-5 Pattern: defined ONCE, used everywhere)
   --  ========================================================================

   --  Filesystem path (bounded, max 4096 chars)
   Max_Path_Length : constant := 4_096;
   package Path_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max_Path_Length);
   subtype Path_String is Path_Strings.Bounded_String;

   --  Result type: Result[Import_Stats_Type]
   package Import_Cache_Result_Package is new Domain.Error.Result
     .Generic_Result
     (T => Import_Stats_Type);
   subtype Import_Cache_Result is Import_Cache_Result_Package.Result;

   --  ========================================================================
   --  Port Contract Documentation
   --  ========================================================================
   --
   --  The Execute function signature (implemented by use case generic):
   --
   --    function Execute
   --      (Path : Path_String)
   --      return Import_Cache_Result;
   --
   --  Parameters:
   --    Path - Filesystem path to cache file
   --
   --  Returns:
   --    Ok(Cache_Stats) - Import statistics
   --    Err(NotFound) - Cache file doesn't exist
   --    Err(IOError) - Filesystem error
   --
   --  Performance: O(n) where n = number of cached zones

end TZif.Application.Port.Inbound.Import_Cache;
