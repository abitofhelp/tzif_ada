pragma Ada_2022;
--  ===========================================================================
--  Tzif.Application.Port.Inbound.Export_Cache
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Inbound port for Export Cache use case.
--
--  Architecture:
--    Application layer port (hexagonal architecture).
--    Defines interface for external actors to trigger use cases.
--
--  Key Types:
--    Path_String
--    Export_Cache_Result
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

package TZif.Application.Port.Inbound.Export_Cache with
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

   --  Result type: Result[Export_Stats_Type]
   package Export_Cache_Result_Package is new Domain.Error.Result
     .Generic_Result
     (T => Export_Stats_Type);
   subtype Export_Cache_Result is Export_Cache_Result_Package.Result;

   --  ========================================================================
   --  Port Contract Documentation
   --  ========================================================================
   --
   --  The Execute function signature (implemented by use case generic):
   --
   --    function Execute
   --      (Path      : Path_String;
   --       Overwrite : Boolean)
   --      return Export_Cache_Result;
   --
   --  Parameters:
   --    Path - Filesystem path for cache file
   --    Overwrite - Allow overwriting existing file
   --
   --  Returns:
   --    Ok(Cache_Stats) - Export statistics
   --    Err(NotWritable) - Insufficient permissions
   --    Err(IOError) - Filesystem error
   --
   --  Performance: O(n) where n = number of cached zones

end TZif.Application.Port.Inbound.Export_Cache;
