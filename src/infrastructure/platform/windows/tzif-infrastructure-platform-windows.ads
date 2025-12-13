pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.Platform.Windows
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Windows platform-specific operations.
--
--  ===========================================================================

package TZif.Infrastructure.Platform.Windows is

   --  ========================================================================
   --  Read_Symbolic_Link
   --  ========================================================================
   --  Query system timezone and return IANA zone ID.
   --
   --  On Windows, this uses GetDynamicTimeZoneInformation Win32 API to get
   --  the Windows timezone key name, then maps it to an IANA zone ID using
   --  CLDR data (Unicode Common Locale Data Repository).
   --
   --  Parameters:
   --    Path: Ignored on Windows (provided for API compatibility)
   --
   --  Returns:
   --    Ok: IANA zone ID string (e.g., "America/New_York")
   --    Error: If timezone cannot be determined or unknown Windows timezone
   --
   --  Notes:
   --    - Windows timezone names != IANA zone IDs (uses CLDR mapping table)
   --    - Supports ~50 common Windows timezone mappings
   --    - Unknown timezones return Error with descriptive message
   --  ========================================================================

   function Read_Symbolic_Link (Path : String) return Platform_String_Result;

   --  ========================================================================
   --  Operations - Instantiation of Platform Interface
   --  ========================================================================
   --  Instantiate the generic Platform_Operations with Windows
   --  implementations. This provides the platform-agnostic interface for
   --  adapters to use.
   --  ========================================================================

   package Operations is new Infrastructure.Platform.Platform_Operations
     (Read_Symbolic_Link => Read_Symbolic_Link);

end TZif.Infrastructure.Platform.Windows;
