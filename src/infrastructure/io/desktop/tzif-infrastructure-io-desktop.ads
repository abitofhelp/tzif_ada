pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.Io.Desktop
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
--    Timestamp_Type
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
with TZif.Application.Port.Inbound.Get_Version;
with TZif.Application.Port.Inbound.Find_My_Id;
with TZif.Application.Port.Inbound.List_All_Order_By_Id;
with TZif.Application.Port.Inbound.Load_Source;
with TZif.Application.Port.Inbound.Validate_Source;
with TZif.Application.Port.Inbound.Find_By_Pattern;
with TZif.Application.Port.Inbound.Find_By_Region;
with TZif.Application.Port.Inbound.Find_By_Regex;
with TZif.Domain.Value_Object.Source_Info;

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

   -------------------------------------------------------------------------
   --  Read_Version_File
   --
   --  Reads the +VERSION file from the timezone source directory.
   --
   --  Parameters:
   --    Source : Source info containing the path to read from
   --    Result : Ok(Version_String) or Error(Not_Found, IO_Error)
   --
   --  Implementation:
   --    - Opens {source_path}/+VERSION
   --    - Reads first line as version string
   --    - Returns bounded string version
   -------------------------------------------------------------------------
   procedure Read_Version_File
     (Source : TZif.Domain.Value_Object.Source_Info.Source_Info_Type;
      Result : out TZif.Application.Port.Inbound.Get_Version.Version_Result);

   -------------------------------------------------------------------------
   --  Read_System_Timezone_Id
   --
   --  Reads the system's timezone ID from /etc/localtime symlink.
   --
   --  Parameters:
   --    Result : Ok(Zone_Id_Type) or Error(Not_Found, IO_Error)
   --
   --  Implementation:
   --    - Reads /etc/localtime as a symlink
   --    - Extracts zone ID from target path
   --    - Returns zone ID like "America/Phoenix"
   -------------------------------------------------------------------------
   procedure Read_System_Timezone_Id
     (Result : out TZif.Application.Port.Inbound.Find_My_Id.Result);

   -------------------------------------------------------------------------
   --  List_Zones_In_Source
   --
   --  Lists all timezone IDs in a source directory, sorted.
   --
   --  Parameters:
   --    Source     : Source info containing the path to scan
   --    Descending : True for Z-A order, False for A-Z order
   --    Result     : Ok(Zone_Id_List) or Error
   --
   --  Implementation:
   --    - Recursively scans source directory for TZif files
   --    - Extracts zone IDs from file paths
   --    - Sorts alphabetically (ascending or descending)
   -------------------------------------------------------------------------
   procedure List_Zones_In_Source
     (Source     : TZif.Domain.Value_Object.Source_Info.Source_Info_Type;
      Descending : Boolean;
      Result     : out TZif.Application.Port.Inbound.List_All_Order_By_Id
        .List_All_Zones_Result);

   -------------------------------------------------------------------------
   --  Load_Source_From_Path
   --
   --  Loads timezone source metadata from filesystem path.
   --
   --  Parameters:
   --    Path   : Filesystem path to timezone database source
   --    Result : Ok(Source_Info) or Error (Not_Found, IO_Error)
   --
   --  Implementation:
   --    - Validates path exists and is directory
   --    - Reads +VERSION file if present
   --    - Counts zone files recursively
   --    - Constructs Source_Info_Type with metadata
   -------------------------------------------------------------------------
   procedure Load_Source_From_Path
     (Path   : TZif.Application.Port.Inbound.Load_Source.Path_String;
      Result : out TZif.Application.Port.Inbound.Load_Source
        .Load_Source_Result);

   -------------------------------------------------------------------------
   --  Validate_Source_Path
   --
   --  Validates that a path is a valid timezone source.
   --
   --  Parameters:
   --    Path   : Filesystem path to validate
   --    Result : Ok(Boolean) or Error (IO_Error)
   --
   --  Implementation:
   --    - Checks path exists
   --    - Checks path is directory
   --    - Checks for at least one ordinary file
   -------------------------------------------------------------------------
   procedure Validate_Source_Path
     (Path   : TZif.Application.Port.Inbound.Validate_Source.Path_String;
      Result : out TZif.Application.Port.Inbound.Validate_Source
        .Validation_Result);

   -------------------------------------------------------------------------
   --  Find_Zones_By_Pattern
   --
   --  Finds timezone IDs matching a substring pattern.
   --
   --  Parameters:
   --    Pattern : Substring to match against zone IDs
   --    Yield   : Callback invoked for each matching zone
   --    Result  : Ok(Unit) or Error (IO_Error)
   -------------------------------------------------------------------------
   procedure Find_Zones_By_Pattern
     (Pattern : TZif.Application.Port.Inbound.Find_By_Pattern.Pattern_String;
      Yield   : TZif.Application.Port.Inbound.Find_By_Pattern
        .Yield_Callback_Access;
      Result  : out TZif.Application.Port.Inbound.Find_By_Pattern
        .Find_By_Pattern_Result);

   -------------------------------------------------------------------------
   --  Find_Zones_By_Region
   --
   --  Finds timezone IDs by region prefix.
   --
   --  Parameters:
   --    Region : Region prefix (e.g., "America", "Europe")
   --    Yield  : Callback invoked for each matching zone
   --    Result : Ok(Unit) or Error (IO_Error)
   -------------------------------------------------------------------------
   procedure Find_Zones_By_Region
     (Region : TZif.Application.Port.Inbound.Find_By_Region.Region_String;
      Yield  : TZif.Application.Port.Inbound.Find_By_Region
        .Yield_Callback_Access;
      Result : out TZif.Application.Port.Inbound.Find_By_Region
        .Find_By_Region_Result);

   -------------------------------------------------------------------------
   --  Find_Zones_By_Regex
   --
   --  Finds timezone IDs matching a regular expression.
   --
   --  Parameters:
   --    Regex  : Regular expression pattern
   --    Yield  : Callback invoked for each matching zone
   --    Result : Ok(Unit) or Error (IO_Error)
   -------------------------------------------------------------------------
   procedure Find_Zones_By_Regex
     (Regex  : TZif.Application.Port.Inbound.Find_By_Regex.Regex_String;
      Yield  : TZif.Application.Port.Inbound.Find_By_Regex
        .Yield_Callback_Access;
      Result : out TZif.Application.Port.Inbound.Find_By_Regex
        .Find_By_Regex_Result);

end TZif.Infrastructure.IO.Desktop;
