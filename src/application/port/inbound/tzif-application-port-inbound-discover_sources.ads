pragma Ada_2022;
pragma Unevaluated_Use_Of_Old (Allow);
--  ===========================================================================
--  TZif.Application.Port.Inbound.Discover_Sources
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Inbound port for Discover Sources use case.
--
--  Architecture:
--    Application layer port (hexagonal architecture).
--    Defines interface for external actors to trigger use cases.
--
--  Key Types:
--    Path_String
--    Path_List
--    Source_Info_List
--    Error_List
--    Discovery_Data_Type
--    ... and 1 more
--
--  Dependencies:
--    TZif.Domain.Error
--    TZif.Domain.Error.Result
--    TZif.Domain.Value_Object.Source_Info
--    TZif.Domain.Types.Bounded_Vector
--
--  SPARK Compatibility:
--    Uses bounded vectors instead of Ada.Containers.Vectors for SPARK
--    formal verification. Capacity limits defined in TZif_Config.
--
--  ===========================================================================

with Ada.Strings.Bounded;
with TZif_Config;
with TZif.Domain.Error;
with TZif.Domain.Error.Result;
with TZif.Domain.Types.Bounded_Vector;
with TZif.Domain.Value_Object.Source_Info;

package TZif.Application.Port.Inbound.Discover_Sources with
  SPARK_Mode => On,
  Preelaborate
is

   use TZif.Domain.Value_Object.Source_Info;

   --  ========================================================================
   --  Canonical Types (GPT-5 Pattern: defined ONCE, used everywhere)
   --  ========================================================================

   --  Path string for directory scanning
   Max_Path_Length : constant := TZif_Config.Max_Path_Length;
   package Path_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max_Path_Length);
   subtype Path_String is Path_Strings.Bounded_String;

   --  Default value for Path_String (required for SPARK-compatible containers)
   function Default_Path return Path_String is
     (Path_Strings.Null_Bounded_String) with
     Inline;

   --  Path list (developer-specified search paths) - SPARK-compatible bounded
   package Path_Vectors is new TZif.Domain.Types.Bounded_Vector
     (Element_Type  => Path_String,
      Capacity      => TZif_Config.Max_Search_Paths,
      Default_Value => Default_Path);
   subtype Path_List is Path_Vectors.Vector;

   --  Source Info Collection Type - SPARK-compatible bounded
   package Source_Info_Vectors is new TZif.Domain.Types.Bounded_Vector
     (Element_Type  => Source_Info_Type,
      Capacity      => TZif_Config.Max_Sources,
      Default_Value => Default_Source_Info);
   subtype Source_Info_List is Source_Info_Vectors.Vector;

   --  Error collection for non-fatal errors - SPARK-compatible bounded
   package Error_Vectors is new TZif.Domain.Types.Bounded_Vector
     (Element_Type  => TZif.Domain.Error.Error_Type,
      Capacity      => TZif_Config.Max_Discovery_Errors,
      Default_Value => TZif.Domain.Error.Default_Error);
   subtype Error_List is Error_Vectors.Vector;

   --  Discovery data: sources found + errors encountered
   type Discovery_Data_Type is record
      Sources : Source_Info_List;  --  Discovered sources with metadata
      Errors  : Error_List;        --  Non-fatal errors during scan
   end record;

   --  Result type: Result[Discovery_Data]
   package Discovery_Result_Package is new Domain.Error.Result.Generic_Result
     (T => Discovery_Data_Type);
   subtype Discovery_Result is Discovery_Result_Package.Result;

   --  ========================================================================
   --  Helper Functions
   --  ========================================================================

   --  Create path string from String
   function Make_Path (S : String) return Path_String is
     (Path_Strings.To_Bounded_String (S)) with
     Inline;

   --  ========================================================================
   --  Port Contract Documentation
   --  ========================================================================
   --
   --  The Execute function signature (implemented by use case generic):
   --
   --    function Execute
   --      (Search_Paths : Path_List)
   --      return Discovery_Result;
   --
   --  Parameters:
   --    Search_Paths - List of filesystem paths to scan for timezone sources
   --                   Developer must explicitly specify paths to scan
   --                   Empty list returns error (no default behavior)
   --
   --  Returns:
   --    Ok(Discovery_Data) - Sources found + non-fatal errors
   --      - Discovery_Data.Sources: Discovered sources with metadata (ULID,
   --                                path, version, zone count)
   --      - Discovery_Data.Errors: Non-fatal errors encountered (permission
   --                               denied, invalid paths, etc.)
   --    Err(Invalid_Parameter) - Empty search path list provided
   --    Err(Discovery_Failed) - All paths failed, no sources found
   --
   --  Behavior:
   --    - Scans each provided path for timezone data sources
   --    - Validates sources (must contain +VERSION file and TZif files)
   --    - Automatically parallelizes based on CPU cores:
   --      * 1 core: sequential
   --      * 2-4 cores: use (N-1) cores
   --      * 5+ cores: use 50% of cores (max 8 tasks)
   --    - Collects non-fatal errors (permission denied, invalid source)
   --    - Returns partial success if any sources found
   --    - Deduplicates sources by canonical path
   --
   --  Performance: O(n*m) where n = paths, m = avg files per path
   --               Parallelized across paths
   --
   --  Error Handling:
   --    Fatal (returns Err):
   --      - Empty search path list
   --      - All paths fail AND no sources found
   --    Non-Fatal (collected in Discovery_Data.Errors):
   --      - Path not found
   --      - Permission denied
   --      - Invalid source (no VERSION file)
   --      - IO errors during scan
   --
   --  Example:
   --    Paths : Path_List;
   --    Paths.Append(Make_Path("/usr/share/zoneinfo"));
   --    Paths.Append(Make_Path("/custom/tzdata"));
   --
   --    Result := Repository.Discover_Sources(Paths);
   --
   --    if Is_Ok(Result) then
   --       Data := Value(Result);
   --       -- Process Data.Sources
   --       -- Log Data.Errors
   --    end if;

end TZif.Application.Port.Inbound.Discover_Sources;
