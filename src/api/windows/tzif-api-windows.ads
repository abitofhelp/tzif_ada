pragma Ada_2022;
--  ===========================================================================
--  TZif.Api.Windows
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Windows composition root - wires Windows I/O adapter to application ports.
--    This is the entry point for Windows applications using TZif.
--
--  Usage:
--    Windows users should:
--    1. Set TZIF_DATA_PATH environment variable to IANA tzdata directory
--    2. Import TZif.API.Windows instead of TZif.API
--
--  Dependencies:
--    TZif.Infrastructure.IO.Windows
--    TZif.Application.Operations
--    TZif.API.Operations
--
--  ===========================================================================

with TZif.Infrastructure.IO.Windows;
with TZif.Application.Operations;
with TZif.API.Operations;

package TZif.API.Windows with
  SPARK_Mode => Off
is

   --  ========================================================================
   --  Windows I/O Instantiation
   --  ========================================================================
   --
   --  Instantiates TZif.Application.Operations.All_Operations with the
   --  Windows I/O adapter using formal package pattern.
   --
   --  This provides a complete, ready-to-use API for Windows applications
   --  that need timezone operations with Win32 API timezone detection.
   --
   --  Key differences from Desktop:
   --    - Read_System_Timezone_Id uses Win32 API + CLDR mapping
   --    - TZIF_DATA_PATH environment variable required for tzdata location
   --

   package Windows_Ops is new TZif.Application.Operations.All_Operations
     (Byte_Array => TZif.Infrastructure.IO.Windows.Byte_Array,
      --  Formal packages for Result monads
      Read_File_Result => TZif.Infrastructure.IO.Windows.Read_File_Result,
      Get_Modified_Time_Result =>
        TZif.Infrastructure.IO.Windows.Get_Modified_Time_Result,
      Timestamp_Type => TZif.Infrastructure.IO.Windows.Timestamp_Type,
      --  I/O procedures
      Read_File => TZif.Infrastructure.IO.Windows.Read_File,
      List_Directory_Sources =>
        TZif.Infrastructure.IO.Windows.List_Directory_Sources,
      Get_Modified_Time => TZif.Infrastructure.IO.Windows.Get_Modified_Time,
      Read_Version_File => TZif.Infrastructure.IO.Windows.Read_Version_File,
      Read_System_Timezone_Id =>
        TZif.Infrastructure.IO.Windows.Read_System_Timezone_Id,
      List_Zones_In_Source =>
        TZif.Infrastructure.IO.Windows.List_Zones_In_Source,
      Load_Source_From_Path =>
        TZif.Infrastructure.IO.Windows.Load_Source_From_Path,
      Validate_Source_Path =>
        TZif.Infrastructure.IO.Windows.Validate_Source_Path,
      Find_Zones_By_Pattern =>
        TZif.Infrastructure.IO.Windows.Find_Zones_By_Pattern,
      Find_Zones_By_Region =>
        TZif.Infrastructure.IO.Windows.Find_Zones_By_Region,
      Find_Zones_By_Regex =>
        TZif.Infrastructure.IO.Windows.Find_Zones_By_Regex);

   --  Instantiate the generic API facade for Windows profile
   package API is new TZif.API.Operations.Facade (Ops => Windows_Ops);

end TZif.API.Windows;
