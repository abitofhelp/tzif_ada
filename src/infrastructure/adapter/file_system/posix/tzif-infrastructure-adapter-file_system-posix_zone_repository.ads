pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.Adapter.File_System.POSIX_Zone_Repository
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    POSIX instantiation of Zone_Repository generic.
--    Provides repository operations using POSIX platform.
--
--  Architecture:
--    Infrastructure layer adapter instantiation.
--    Used by Desktop I/O adapter and POSIX tests.
--
--  Usage:
--    with TZif.Infrastructure.Adapter.File_System.POSIX_Zone_Repository;
--    -- Access functions via POSIX_Zone_Repository.Find_By_Id, etc.
--
--  ===========================================================================

with TZif.Infrastructure.Platform.POSIX;
with TZif.Infrastructure.Adapter.File_System.Zone_Repository;

package TZif.Infrastructure.Adapter.File_System.POSIX_Zone_Repository is new
  TZif.Infrastructure.Adapter.File_System.Zone_Repository
    (Platform_Ops => TZif.Infrastructure.Platform.POSIX.Operations);
