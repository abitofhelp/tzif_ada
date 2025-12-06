pragma Ada_2022;
--  ===========================================================================
--  TZif.Api.Embedded
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Embedded composition root - wires Embedded I/O adapter to application
--    ports. This is the entry point for embedded systems using TZif.
--
--  Target Platforms:
--    - STM32F769I-DK with RAM filesystem
--    - STM32H7S78-DK with 32MB PSRAM
--    - ARM Cortex-M with Ravenscar profile
--    - Any embedded system with filesystem support
--
--  Usage:
--    Embedded users should:
--    1. Set TZIF_DATA_PATH environment variable to tzdata directory
--    2. Optionally set TZIF_SYSTEM_ZONE for local timezone (default: UTC)
--    3. Import TZif.API.Embedded instead of TZif.API
--
--  Dependencies:
--    TZif.Infrastructure.IO.Embedded
--    TZif.Application.Operations
--    TZif.API.Operations
--
--  ===========================================================================

with TZif.Infrastructure.IO.Embedded;
with TZif.Application.Operations;
with TZif.API.Operations;

package TZif.API.Embedded with
  SPARK_Mode => Off
is

   --  ========================================================================
   --  Embedded I/O Instantiation
   --  ========================================================================
   --
   --  Instantiates TZif.Application.Operations.All_Operations with the
   --  Embedded I/O adapter using formal package pattern.
   --
   --  This provides a complete, ready-to-use API for embedded applications
   --  that need timezone operations with environment-variable configuration.
   --
   --  Key differences from Desktop:
   --    - Read_System_Timezone_Id uses TZIF_SYSTEM_ZONE env var (default: UTC)
   --    - TZIF_DATA_PATH environment variable required for tzdata location
   --    - Designed for Ravenscar profile compatibility
   --

   package Embedded_Ops is new TZif.Application.Operations.All_Operations
     (Byte_Array => TZif.Infrastructure.IO.Embedded.Byte_Array,
      --  Formal packages for Result monads
      Read_File_Result => TZif.Infrastructure.IO.Embedded.Read_File_Result,
      Get_Modified_Time_Result =>
        TZif.Infrastructure.IO.Embedded.Get_Modified_Time_Result,
      Timestamp_Type => TZif.Infrastructure.IO.Embedded.Timestamp_Type,
      --  I/O procedures
      Read_File => TZif.Infrastructure.IO.Embedded.Read_File,
      List_Directory_Sources =>
        TZif.Infrastructure.IO.Embedded.List_Directory_Sources,
      Get_Modified_Time => TZif.Infrastructure.IO.Embedded.Get_Modified_Time,
      Read_Version_File => TZif.Infrastructure.IO.Embedded.Read_Version_File,
      Read_System_Timezone_Id =>
        TZif.Infrastructure.IO.Embedded.Read_System_Timezone_Id,
      List_Zones_In_Source =>
        TZif.Infrastructure.IO.Embedded.List_Zones_In_Source,
      Load_Source_From_Path =>
        TZif.Infrastructure.IO.Embedded.Load_Source_From_Path,
      Validate_Source_Path =>
        TZif.Infrastructure.IO.Embedded.Validate_Source_Path,
      Find_Zones_By_Pattern =>
        TZif.Infrastructure.IO.Embedded.Find_Zones_By_Pattern,
      Find_Zones_By_Region =>
        TZif.Infrastructure.IO.Embedded.Find_Zones_By_Region,
      Find_Zones_By_Regex =>
        TZif.Infrastructure.IO.Embedded.Find_Zones_By_Regex);

   --  Instantiate the generic API facade for Embedded profile
   package API is new TZif.API.Operations.Facade (Ops => Embedded_Ops);

end TZif.API.Embedded;
