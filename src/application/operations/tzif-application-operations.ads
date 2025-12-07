pragma Ada_2022;
--  ===========================================================================
--  TZif.Application.Operations
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Operations interface and type definitions.
--
--  Key Types:
--    Zone_Id_Input_Type
--    Find_By_Id_Result_Type
--    Discover_Path_List_Type
--    Discovery_Result_Type
--
--  Dependencies:
--    Interfaces
--    TZif.Domain.Error.Result
--    TZif.Application.Port.Inbound.Find_By_Id
--
--  ===========================================================================

with Interfaces;

with TZif.Domain.Error.Result;

with TZif.Application.Port.Inbound.Find_By_Id;
with TZif.Application.Port.Inbound.Discover_Sources;
with TZif.Application.Port.Inbound.Get_Transition_At_Epoch;
with TZif.Application.Port.Inbound.Get_Version;
with TZif.Application.Port.Inbound.Find_My_Id;
with TZif.Application.Port.Inbound.List_All_Order_By_Id;
with TZif.Application.Port.Inbound.Load_Source;
with TZif.Application.Port.Inbound.Validate_Source;
with TZif.Application.Port.Inbound.Find_By_Pattern;
with TZif.Application.Port.Inbound.Find_By_Region;
with TZif.Application.Port.Inbound.Find_By_Regex;
with TZif.Domain.Value_Object.Epoch_Seconds;
with TZif.Domain.Value_Object.Source_Info;

package TZif.Application.Operations with
  SPARK_Mode => On
is

   use Interfaces;

   --  ========================================================================
   --  Canonical types from inbound ports
   --  ========================================================================

   package Inbound_Find_By_Id renames TZif.Application.Port.Inbound.Find_By_Id;
   package Inbound_Discover_Sources renames
     TZif.Application.Port.Inbound.Discover_Sources;

   --  Find_By_Id types
   subtype Zone_Id_Input_Type is Inbound_Find_By_Id.Zone_Id_Input_Type;
   package Find_By_Id_Result renames Inbound_Find_By_Id.Find_By_Id_Result;
   subtype Find_By_Id_Result_Type is Find_By_Id_Result.Result;

   --  Discover_Sources types
   subtype Discover_Path_List_Type is Inbound_Discover_Sources.Path_List;
   package Discovery_Result renames
     Inbound_Discover_Sources.Discovery_Result_Package;
   subtype Discovery_Result_Type is Discovery_Result.Result;

   --  Get_Transition_At_Epoch types
   package Inbound_Get_Transition renames
     TZif.Application.Port.Inbound.Get_Transition_At_Epoch;
   subtype Transition_Zone_Id_String is Inbound_Get_Transition.Zone_Id_String;
   subtype Epoch_Seconds_Type is
     TZif.Domain.Value_Object.Epoch_Seconds.Epoch_Seconds_Type;
   package Get_Transition_Result renames
     Inbound_Get_Transition.Get_Transition_Result_Package;
   subtype Get_Transition_Result_Type is Get_Transition_Result.Result;

   --  Get_Version types
   package Inbound_Get_Version renames
     TZif.Application.Port.Inbound.Get_Version;
   subtype Version_String_Type is Inbound_Get_Version.Version_String;
   package Get_Version_Result renames
     Inbound_Get_Version.Version_Result_Package;
   subtype Get_Version_Result_Type is Get_Version_Result.Result;

   --  Source_Info type (shared by multiple operations)
   subtype Source_Info_Type is
     TZif.Domain.Value_Object.Source_Info.Source_Info_Type;

   --  Find_My_Id types
   package Inbound_Find_My_Id renames TZif.Application.Port.Inbound.Find_My_Id;
   package Find_My_Id_Result renames Inbound_Find_My_Id.Result_Zone_Id;
   subtype Find_My_Id_Result_Type is Find_My_Id_Result.Result;

   --  List_All_Zones types
   package Inbound_List_All renames
     TZif.Application.Port.Inbound.List_All_Order_By_Id;
   package List_All_Result renames
     Inbound_List_All.List_All_Zones_Result_Package;
   subtype List_All_Result_Type is List_All_Result.Result;

   --  Load_Source types
   package Inbound_Load_Source renames
     TZif.Application.Port.Inbound.Load_Source;
   subtype Load_Path_String is Inbound_Load_Source.Path_String;
   package Load_Source_Result renames
     Inbound_Load_Source.Load_Source_Result_Package;
   subtype Load_Source_Result_Type is Load_Source_Result.Result;

   --  Validate_Source types
   package Inbound_Validate_Source renames
     TZif.Application.Port.Inbound.Validate_Source;
   subtype Validate_Path_String is Inbound_Validate_Source.Path_String;
   package Validate_Source_Result renames
     Inbound_Validate_Source.Validation_Result_Package;
   subtype Validate_Source_Result_Type is Validate_Source_Result.Result;

   --  Find_By_Pattern types
   package Inbound_Find_By_Pattern renames
     TZif.Application.Port.Inbound.Find_By_Pattern;
   subtype Pattern_String_Type is Inbound_Find_By_Pattern.Pattern_String;
   subtype Pattern_Callback_Type is
     Inbound_Find_By_Pattern.Yield_Callback_Access;
   package Find_By_Pattern_Result renames
     Inbound_Find_By_Pattern.Find_By_Pattern_Result_Package;
   subtype Find_By_Pattern_Result_Type is Find_By_Pattern_Result.Result;

   --  Find_By_Region types
   package Inbound_Find_By_Region renames
     TZif.Application.Port.Inbound.Find_By_Region;
   subtype Region_String_Type is Inbound_Find_By_Region.Region_String;
   subtype Region_Callback_Type is
     Inbound_Find_By_Region.Yield_Callback_Access;
   package Find_By_Region_Result renames
     Inbound_Find_By_Region.Find_By_Region_Result_Package;
   subtype Find_By_Region_Result_Type is Find_By_Region_Result.Result;

   --  Find_By_Regex types
   package Inbound_Find_By_Regex renames
     TZif.Application.Port.Inbound.Find_By_Regex;
   subtype Regex_String_Type is Inbound_Find_By_Regex.Regex_String;
   subtype Regex_Callback_Type is Inbound_Find_By_Regex.Yield_Callback_Access;
   package Find_By_Regex_Result renames
     Inbound_Find_By_Regex.Find_By_Regex_Result_Package;
   subtype Find_By_Regex_Result_Type is Find_By_Regex_Result.Result;

   --  ========================================================================
   --  Generic I/O Plugin Contract
   --  ========================================================================
   --
   --  This generic defines the minimal filesystem/cache operations required
   --  by all TZif use cases that perform I/O.
   --
   --  Each Result type is a formal package instantiation of Generic_Result.
   --  This allows the generic body to access Is_Ok, Value, Error_Info, etc.
   --  via qualified names (e.g., Read_File_Result.Is_Ok) without ambiguity.
   --
   --  ========================================================================

   generic
      -------------------------------------------------------------------------
      --  Raw byte buffer type used by the parser
      -------------------------------------------------------------------------
      type Byte_Array is array (Positive range <>) of Unsigned_8;

      -------------------------------------------------------------------------
      --  Formal package: Result monad for file reads
      --
      --  Any instantiation of Generic_Result is acceptable here; the
      --  operations (Is_Ok, Value, Error_Info, etc.) are accessed through
      --  this formal package to avoid ambiguity.
      -------------------------------------------------------------------------
      with package Read_File_Result is new TZif.Domain.Error.Result
        .Generic_Result
        (<>);

      -------------------------------------------------------------------------
      --  Read a single TZif file for the given zone identifier
      --
      --  Parameters:
      --    Id     - Canonical zone identifier
      --    Bytes  - Output buffer filled with TZif bytes
      --    Length - Number of valid bytes written to Bytes
      --    Result - I/O Result monad (success or failure)
      -------------------------------------------------------------------------
      with procedure Read_File
        (Id : Zone_Id_Input_Type; Bytes : out Byte_Array; Length : out Natural;
         Result : out Read_File_Result.Result);

      -------------------------------------------------------------------------
      --  List directory contents to discover timezone sources
      --  Uses Discovery_Result package (canonical from inbound port)
      -------------------------------------------------------------------------
      with procedure List_Directory_Sources
        (Search_Paths :     Discover_Path_List_Type;
         Result       : out Discovery_Result.Result);

      -------------------------------------------------------------------------
      --  Formal package: Result monad for timestamp queries
      -------------------------------------------------------------------------
      type Timestamp_Type is private;
      with package Get_Modified_Time_Result is new TZif.Domain.Error.Result
        .Generic_Result
        (<>);

      -------------------------------------------------------------------------
      --  Get last modified time for a zone source
      -------------------------------------------------------------------------
      with procedure Get_Modified_Time
        (Id     :     Zone_Id_Input_Type; Timestamp : out Timestamp_Type;
         Result : out Get_Modified_Time_Result.Result);

      -------------------------------------------------------------------------
      --  Read version from timezone source
      --  Reads the +VERSION file from the source path
      -------------------------------------------------------------------------
      with procedure Read_Version_File
        (Source :     Source_Info_Type;
         Result : out Get_Version_Result.Result);

      -------------------------------------------------------------------------
      --  Read system timezone identifier
      --  Reads /etc/localtime symlink and extracts zone ID
      -------------------------------------------------------------------------
      with procedure Read_System_Timezone_Id
        (Result : out Find_My_Id_Result.Result);

      -------------------------------------------------------------------------
      --  List all zones in a source directory
      --  Scans directory recursively for TZif files and returns sorted list
      -------------------------------------------------------------------------
      with procedure List_Zones_In_Source
        (Source     :     Source_Info_Type;
         Descending :     Boolean;
         Result     : out List_All_Result.Result);

      -------------------------------------------------------------------------
      --  Load source metadata from filesystem path
      --  Reads directory, counts zones, reads +VERSION file
      -------------------------------------------------------------------------
      with procedure Load_Source_From_Path
        (Path   :     Load_Path_String;
         Result : out Load_Source_Result.Result);

      -------------------------------------------------------------------------
      --  Validate source path
      --  Checks path exists, is directory, contains zone files
      -------------------------------------------------------------------------
      with procedure Validate_Source_Path
        (Path   :     Validate_Path_String;
         Result : out Validate_Source_Result.Result);

      -------------------------------------------------------------------------
      --  Find zones by substring pattern
      --  Scans zone list and yields matches via callback
      -------------------------------------------------------------------------
      with procedure Find_Zones_By_Pattern
        (Pattern :     Pattern_String_Type;
         Yield   :     Pattern_Callback_Type;
         Result  : out Find_By_Pattern_Result.Result);

      -------------------------------------------------------------------------
      --  Find zones by region prefix
      --  Scans zone list and yields matches via callback
      -------------------------------------------------------------------------
      with procedure Find_Zones_By_Region
        (Region :     Region_String_Type;
         Yield  :     Region_Callback_Type;
         Result : out Find_By_Region_Result.Result);

      -------------------------------------------------------------------------
      --  Find zones by regular expression
      --  Scans zone list and yields matches via callback
      -------------------------------------------------------------------------
      with procedure Find_Zones_By_Regex
        (Regex  :     Regex_String_Type;
         Yield  :     Regex_Callback_Type;
         Result : out Find_By_Regex_Result.Result);

   package All_Operations with
     SPARK_Mode => On
   is
      ----------------------------------------------------------------------
      --  Find_By_Id
      --
      --  SPARK-friendly, procedure-based API:
      --    - No exceptions, no hidden I/O
      --    - Id: canonical zone identifier
      --    - Result: canonical Find_By_Id_Result_Type
      ----------------------------------------------------------------------
      procedure Find_By_Id
        (Id : Zone_Id_Input_Type; Result : out Find_By_Id_Result_Type);

      ----------------------------------------------------------------------
      --  Discover_Sources
      --
      --  Uses List_Directory_Sources + domain logic to produce
      --  Discovery_Result_Type.
      ----------------------------------------------------------------------
      procedure Discover_Sources
        (Search_Paths :     Discover_Path_List_Type;
         Result       : out Discovery_Result_Type);

      ----------------------------------------------------------------------
      --  Get_Transition_At_Epoch
      --
      --  SPARK-friendly procedure-based API:
      --    - Reads zone file via Read_File I/O plugin
      --    - Parses TZif data via Domain.Parser
      --    - Finds transition at specified epoch
      --    - Returns Transition_Info via Result monad
      --
      --  Parameters:
      --    Id     : Zone identifier (bounded string)
      --    Epoch  : Unix timestamp to query
      --    Result : Ok(Transition_Info) or Error
      ----------------------------------------------------------------------
      procedure Get_Transition_At_Epoch
        (Id     :     Transition_Zone_Id_String;
         Epoch  :     Epoch_Seconds_Type;
         Result : out Get_Transition_Result_Type);

      ----------------------------------------------------------------------
      --  Get_Version
      --
      --  Returns the IANA database version from the source's +VERSION file.
      --
      --  Parameters:
      --    Source : The timezone data source to query
      --    Result : Ok(Version_String) or Error
      ----------------------------------------------------------------------
      procedure Get_Version
        (Source :     Source_Info_Type;
         Result : out Get_Version_Result_Type);

      ----------------------------------------------------------------------
      --  Find_My_Id
      --
      --  Detects the system's local timezone by reading /etc/localtime.
      --
      --  Parameters:
      --    Result : Ok(Zone_Id_Type) or Error (Not_Found, IO_Error)
      ----------------------------------------------------------------------
      procedure Find_My_Id (Result : out Find_My_Id_Result_Type);

      ----------------------------------------------------------------------
      --  List_All_Zones
      --
      --  Lists all timezone IDs in a source, sorted by ID.
      --
      --  Parameters:
      --    Source     : The timezone data source to scan
      --    Descending : True for Z-A order, False for A-Z order
      --    Result     : Ok(Zone_Id_List) or Error
      ----------------------------------------------------------------------
      procedure List_All_Zones
        (Source     :     Source_Info_Type;
         Descending :     Boolean;
         Result     : out List_All_Result_Type);

      ----------------------------------------------------------------------
      --  Load_Source
      --
      --  Loads timezone source metadata from filesystem path.
      --
      --  Parameters:
      --    Path   : Filesystem path to timezone database source
      --    Result : Ok(Source_Info) or Error (Not_Found, IO_Error)
      ----------------------------------------------------------------------
      procedure Load_Source
        (Path : Load_Path_String; Result : out Load_Source_Result_Type);

      ----------------------------------------------------------------------
      --  Validate_Source
      --
      --  Validates that a path is a valid timezone source.
      --
      --  Parameters:
      --    Path   : Filesystem path to validate
      --    Result : Ok(Boolean) or Error (IO_Error)
      ----------------------------------------------------------------------
      procedure Validate_Source
        (Path   :     Validate_Path_String;
         Result : out Validate_Source_Result_Type);

      ----------------------------------------------------------------------
      --  Find_By_Pattern
      --
      --  Finds timezone IDs matching a substring pattern.
      --
      --  Parameters:
      --    Pattern : Substring to match against zone IDs
      --    Yield   : Callback invoked for each matching zone
      --    Result  : Ok(Unit) or Error (IO_Error)
      ----------------------------------------------------------------------
      procedure Find_By_Pattern
        (Pattern :     Pattern_String_Type;
         Yield   :     Pattern_Callback_Type;
         Result  : out Find_By_Pattern_Result_Type);

      ----------------------------------------------------------------------
      --  Find_By_Region
      --
      --  Finds timezone IDs by region prefix.
      --
      --  Parameters:
      --    Region : Region prefix (e.g., "America", "Europe")
      --    Yield  : Callback invoked for each matching zone
      --    Result : Ok(Unit) or Error (IO_Error)
      ----------------------------------------------------------------------
      procedure Find_By_Region
        (Region :     Region_String_Type;
         Yield  :     Region_Callback_Type;
         Result : out Find_By_Region_Result_Type);

      ----------------------------------------------------------------------
      --  Find_By_Regex
      --
      --  Finds timezone IDs matching a regular expression.
      --
      --  Parameters:
      --    Regex  : Regular expression pattern
      --    Yield  : Callback invoked for each matching zone
      --    Result : Ok(Unit) or Error (IO_Error)
      ----------------------------------------------------------------------
      procedure Find_By_Regex
        (Regex  :     Regex_String_Type;
         Yield  :     Regex_Callback_Type;
         Result : out Find_By_Regex_Result_Type);

   end All_Operations;

end TZif.Application.Operations;
