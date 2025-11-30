pragma Ada_2022;
--  ===========================================================================
--  Tzif.Api.Desktop
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Desktop interface and type definitions.
--
--  Dependencies:
--    TZif.Infrastructure.IO.Desktop
--    TZif.Application.Operations
--    TZif.API.Operations
--
--  ===========================================================================

with TZif.Infrastructure.IO.Desktop;
with TZif.Application.Operations;
with TZif.API.Operations;

package TZif.API.Desktop with
  SPARK_Mode => Off
is

   --  ========================================================================
   --  Desktop I/O Instantiation
   --  ========================================================================
   --
   --  Instantiates TZif.Application.Operations.All_Operations with the
   --  desktop filesystem I/O adapter using formal package pattern.
   --
   --  This provides a complete, ready-to-use API for desktop applications
   --  that need timezone operations with standard filesystem access.
   --

   package Desktop_Ops is new TZif.Application.Operations.All_Operations
     (Byte_Array => TZif.Infrastructure.IO.Desktop.Byte_Array,
      --  Formal packages for Result monads
      Read_File_Result => TZif.Infrastructure.IO.Desktop.Read_File_Result,
      Get_Modified_Time_Result =>
        TZif.Infrastructure.IO.Desktop.Get_Modified_Time_Result,
      Timestamp_Type => TZif.Infrastructure.IO.Desktop.Timestamp_Type,
      --  I/O procedures
      Read_File => TZif.Infrastructure.IO.Desktop.Read_File,
      List_Directory_Sources =>
        TZif.Infrastructure.IO.Desktop.List_Directory_Sources,
      Get_Modified_Time => TZif.Infrastructure.IO.Desktop.Get_Modified_Time);

   --  Instantiate the generic API facade for desktop profile
   package API is new TZif.API.Operations.Facade (Ops => Desktop_Ops);

end TZif.API.Desktop;
