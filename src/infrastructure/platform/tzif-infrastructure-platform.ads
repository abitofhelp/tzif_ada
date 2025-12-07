pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.Platform
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Platform abstraction layer.
--
--  Key Types:
--    requirement
--    Platform_String
--    Platform_String_Result
--
--  Dependencies:
--    TZif.Domain.Error.Result
--    Preelaborate
--
--  ===========================================================================

with Ada.Strings.Bounded;
with TZif.Domain.Error.Result;

package TZif.Infrastructure.Platform with
  Preelaborate
is

   --  ========================================================================
   --  Bounded String for Platform Operations
   --  ========================================================================
   --  Use bounded string to satisfy definite subtype requirement for Result
   --  type. PATH_MAX is 4096 on most POSIX systems
   --  ========================================================================

   package Platform_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (4_096);
   subtype Platform_String is Platform_Strings.Bounded_String;

   --  ========================================================================
   --  Result Type for String Operations
   --  ========================================================================

   package String_Result is new Domain.Error.Result.Generic_Result
     (Platform_String);
   subtype Platform_String_Result is String_Result.Result;

   --  ========================================================================
   --  Platform_Operations - Generic Interface
   --  ========================================================================
   --  Defines the contract that all platform implementations must satisfy.
   --  ========================================================================

   generic
      --  Read target of symbolic link
      --  Returns target path on success, error otherwise
      with function Read_Symbolic_Link
        (Path : String) return Platform_String_Result;

   package Platform_Operations is
      --  Re-export operations for use by adapters
      function Read_Link (Path : String) return Platform_String_Result renames
        Read_Symbolic_Link;
   end Platform_Operations;

end TZif.Infrastructure.Platform;
