pragma Ada_2022;
--  ===========================================================================
--  Tzif.Infrastructure.Io.Desktop
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Desktop interface and type definitions.
--
--  Key Types:
--    Byte_Array
--    TZif_Byte_Buffer
--    Read_Info
--    Read_Cache_Info
--    Write_Cache_Info
--    ... and 1 more
--
--  Dependencies:
--    Interfaces
--    TZif.Domain.Error.Result
--    TZif.Application.Port.Inbound.Find_By_Id
--
--  ===========================================================================

with Interfaces;
with Ada.Calendar;

with TZif.Domain.Error.Result;
with TZif.Application.Port.Inbound.Find_By_Id;
with TZif.Application.Port.Inbound.Discover_Sources;
with TZif.Application.Port.Inbound.Import_Cache;
with TZif.Application.Port.Inbound.Export_Cache;

package TZif.Infrastructure.IO.Desktop with
  SPARK_Mode => Off
is

   use Interfaces;

   --  ========================================================================
   --  Byte Array Type
   --  ========================================================================

   --  Maximum TZif file size: 64KB (conservative estimate)
   --  Typical TZif files are 1-3KB; largest known files < 20KB
   Max_TZif_File_Size : constant := 65_536;

   type Byte_Array is array (Positive range <>) of Unsigned_8;
   subtype TZif_Byte_Buffer is Byte_Array (1 .. Max_TZif_File_Size);

   --  ========================================================================
   --  I/O Result Types (Formal Package Instantiations)
   --  ========================================================================
   --
   --  These Result package instantiations will be passed as formal packages
   --  to TZif.Application.Operations.All_Operations.
   --
   --  Each I/O operation gets its own Result package to avoid ambiguity.
   --  ========================================================================

   --  Info type for file reads (returned on success)
   type Read_Info is record
      Bytes_Read : Natural;
   end record;

   --  Result package for Read_File operation
   package Read_File_Result is new TZif.Domain.Error.Result.Generic_Result
     (T => Read_Info);

   --  Info type for cache reads
   type Read_Cache_Info is record
      Bytes_Read : Natural;
   end record;

   --  Result package for Read_Cache_File operation
   package Read_Cache_Result is new TZif.Domain.Error.Result.Generic_Result
     (T => Read_Cache_Info);

   --  Info type for cache writes
   type Write_Cache_Info is record
      Success : Boolean;
   end record;

   --  Result package for Write_Cache_File operation
   package Write_Cache_Result is new TZif.Domain.Error.Result.Generic_Result
     (T => Write_Cache_Info);

   --  Discovery: List_Directory_Sources uses the Result package from
   --  the inbound port directly (no separate I/O Result package needed)

   --  Timestamp type for file modification times
   subtype Timestamp_Type is Ada.Calendar.Time;

   --  Result package for Get_Modified_Time operation
   package Get_Modified_Time_Result is new TZif.Domain.Error.Result
     .Generic_Result
     (Timestamp_Type);

   --  ========================================================================
   --  I/O Procedures (Generic Formal Parameters)
   --  ========================================================================

   -------------------------------------------------------------------------
   --  Read_File
   --
   --  Reads a TZif file for the given zone identifier.
   --
   --  Parameters:
   --    Id     : Zone identifier (e.g., "America/New_York")
   --    Bytes  : Output buffer for TZif data
   --    Length : Number of bytes read (0 if error)
   --    Result : Ok(bytes_read) or Error(IO_Error)
   --
   --  Implementation:
   --    - Maps Zone_Id to filesystem path via /usr/share/zoneinfo/{Id}
   --    - Opens file using Ada.Streams.Stream_IO
   --    - Reads binary content into Bytes
   --    - Sets Length to actual bytes read
   --    - Returns IO error on file not found, permission denied, etc.
   -------------------------------------------------------------------------
   procedure Read_File
     (Id     :     TZif.Application.Port.Inbound.Find_By_Id.Zone_Id_Input_Type;
      Bytes  : out Byte_Array; Length : out Natural;
      Result : out Read_File_Result.Result);

   -------------------------------------------------------------------------
   --  Read_Cache_File
   --
   --  Reads a cache file from the given path.
   --
   --  Parameters:
   --    Path   : Filesystem path to cache file
   --    Bytes  : Output buffer for cache data
   --    Length : Number of bytes read
   --    Result : Ok(Read_Cache_Info) or Error(IO_Error)
   -------------------------------------------------------------------------
   procedure Read_Cache_File
     (Path   :     TZif.Application.Port.Inbound.Import_Cache.Path_String;
      Bytes  : out Byte_Array; Length : out Natural;
      Result : out Read_Cache_Result.Result);

   -------------------------------------------------------------------------
   --  Write_Cache_File
   --
   --  Writes cache data to a file.
   --
   --  Parameters:
   --    Path      : Filesystem path for output file
   --    Bytes     : Data to write
   --    Length    : Number of bytes to write
   --    Overwrite : If True, overwrite; if False, fail if exists
   --    Result    : Ok(Write_Cache_Info) or Error(IO_Error)
   -------------------------------------------------------------------------
   procedure Write_Cache_File
     (Path   :     TZif.Application.Port.Inbound.Export_Cache.Path_String;
      Bytes  :     Byte_Array; Length : Natural; Overwrite : Boolean;
      Result : out Write_Cache_Result.Result);

   -------------------------------------------------------------------------
   --  List_Directory_Sources
   --
   --  Discovers timezone sources by scanning filesystem directories.
   --
   --  Parameters:
   --    Search_Paths : List of directories to scan (e.g., /usr/share/zoneinfo)
   --    Result       : Discovery_Result containing found sources or error
   --
   --  Implementation:
   --    - Iterates through each search path
   --    - Validates directory structure (checks for VERSION file)
   --    - Counts available zone files
   --    - Constructs Source_Info records
   --    - Returns Discovery_Result with source list
   -------------------------------------------------------------------------
   procedure List_Directory_Sources
     (Search_Paths : TZif.Application.Port.Inbound.Discover_Sources.Path_List;
      Result       : out TZif.Application.Port.Inbound.Discover_Sources
        .Discovery_Result_Package
        .Result);

   -------------------------------------------------------------------------
   --  Get_Modified_Time
   --
   --  Gets last modified timestamp for a zone file.
   --
   --  Parameters:
   --    Id        : Zone identifier
   --    Timestamp : Last modification time
   --    Result    : Ok(Timestamp_Type) or Error(IO_Error)
   --
   --  Purpose:
   --    Used for cache validation and incremental updates.
   -------------------------------------------------------------------------
   procedure Get_Modified_Time
     (Id        : TZif.Application.Port.Inbound.Find_By_Id.Zone_Id_Input_Type;
      Timestamp : out Timestamp_Type;
      Result    : out Get_Modified_Time_Result.Result);

end TZif.Infrastructure.IO.Desktop;
