pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.Platform.Posix
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    POSIX platform-specific operations.
--
--  Platforms:
--    - Linux (all distributions)
--    - macOS (all versions)
--    - BSD variants (FreeBSD, OpenBSD, NetBSD)
--
--  ===========================================================================

package TZif.Infrastructure.Platform.POSIX is

   --  ========================================================================
   --  Read_Symbolic_Link
   --  ========================================================================
   --  Read the target path of a symbolic link using POSIX readlink(2).
   --
   --  Parameters:
   --    Path: Path to the symbolic link to read
   --
   --  Returns:
   --    Ok(target_path) if successful
   --    Error if:
   --      - Path does not exist
   --      - Path is not a symbolic link
   --      - Permission denied
   --      - System error
   --
   --  Notes:
   --    - Uses POSIX readlink(2) system call
   --    - Path must be an absolute or relative path to a symlink
   --    - Maximum path length is 4096 bytes (POSIX PATH_MAX)
   --  ========================================================================

   function Read_Symbolic_Link (Path : String) return Platform_String_Result;

   --  ========================================================================
   --  Operations - Instantiation of Platform Interface
   --  ========================================================================
   --  Instantiate the generic Platform_Operations with POSIX implementations.
   --  This provides the platform-agnostic interface for adapters to use.
   --  ========================================================================

   package Operations is new Infrastructure.Platform.Platform_Operations
     (Read_Symbolic_Link => Read_Symbolic_Link);

end TZif.Infrastructure.Platform.POSIX;
