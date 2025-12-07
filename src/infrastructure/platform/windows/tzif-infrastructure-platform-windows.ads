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
--  Dependencies:
--    Preelaborate
--
--  ===========================================================================

package TZif.Infrastructure.Platform.Windows with
  Preelaborate
is

   --  ========================================================================
   --  Read_Symbolic_Link
   --  ========================================================================
   --  Read symbolic link or determine timezone via Windows registry.
   --
   --  Parameters:
   --    Path: Path to check (typically ignored on Windows, uses registry
   --          instead)
   --
   --  Returns:
   --    Error: "Not implemented on Windows" (for now)
   --
   --  Future Implementation:
   --    Should query Windows registry at:
   --    HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\
   --    TimeZoneInformation and map Windows timezone name to IANA zone ID.
   --
   --  Notes:
   --    - Windows uses different timezone model than POSIX
   --    - Windows timezone names != IANA zone IDs (need mapping table)
   --    - Windows 10+ includes ICU with IANA data
   --    - Symbolic links on Windows require Developer Mode or admin
   --      rights
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
