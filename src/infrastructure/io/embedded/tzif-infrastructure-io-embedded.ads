pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.IO.Embedded
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Embedded platform I/O adapter for TZif operations.
--    Designed for embedded systems like STM32F769I with RAM filesystem.
--
--  Key Differences from Desktop:
--    - Read_System_Timezone_Id uses TZIF_SYSTEM_ZONE env var (default: UTC)
--    - No default zoneinfo path (uses TZIF_DATA_PATH env var)
--    - Designed for Ravenscar profile compatibility
--
--  Environment Variables:
--    TZIF_DATA_PATH   - Path to tzdata directory (required)
--    TZIF_SYSTEM_ZONE - System timezone ID (optional, default: UTC)
--
--  Dependencies:
--    Interfaces
--    TZif.Domain.Error.Result
--    TZif.Application.Port.Inbound.*
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

package TZif.Infrastructure.IO.Embedded with
  SPARK_Mode => Off
is

   use Interfaces;

   --  ========================================================================
   --  Byte Array Type
   --  ========================================================================

   --  Maximum TZif file size: 64KB (conservative estimate)
   Max_TZif_File_Size : constant := 65_536;

   type Byte_Array is array (Positive range <>) of Unsigned_8;
   subtype TZif_Byte_Buffer is Byte_Array (1 .. Max_TZif_File_Size);

   --  ========================================================================
   --  I/O Result Types (Formal Package Instantiations)
   --  ========================================================================

   type Read_Info is record
      Bytes_Read : Natural;
   end record;

   package Read_File_Result is new TZif.Domain.Error.Result.Generic_Result
     (T => Read_Info);

   subtype Timestamp_Type is Ada.Calendar.Time;

   package Get_Modified_Time_Result is new TZif.Domain.Error.Result
     .Generic_Result
     (Timestamp_Type);

   --  ========================================================================
   --  I/O Procedures
   --  ========================================================================

   procedure Read_File
     (Id     :     TZif.Application.Port.Inbound.Find_By_Id.Zone_Id_Input_Type;
      Bytes  : out Byte_Array; Length : out Natural;
      Result : out Read_File_Result.Result);

   procedure List_Directory_Sources
     (Search_Paths : TZif.Application.Port.Inbound.Discover_Sources.Path_List;
      Result       : out TZif.Application.Port.Inbound.Discover_Sources
        .Discovery_Result_Package
        .Result);

   procedure Get_Modified_Time
     (Id        : TZif.Application.Port.Inbound.Find_By_Id.Zone_Id_Input_Type;
      Timestamp : out Timestamp_Type;
      Result    : out Get_Modified_Time_Result.Result);

   procedure Read_Version_File
     (Source : TZif.Domain.Value_Object.Source_Info.Source_Info_Type;
      Result : out TZif.Application.Port.Inbound.Get_Version.Version_Result);

   -------------------------------------------------------------------------
   --  Read_System_Timezone_Id
   --
   --  Embedded implementation: Uses TZIF_SYSTEM_ZONE environment variable.
   --
   --  Parameters:
   --    Result : Ok(Zone_Id_Type) or default UTC if not configured
   --
   --  Implementation:
   --    - Reads TZIF_SYSTEM_ZONE environment variable
   --    - Returns configured zone ID or "UTC" as default
   --    - No system-level timezone detection on embedded platforms
   -------------------------------------------------------------------------
   procedure Read_System_Timezone_Id
     (Result : out TZif.Application.Port.Inbound.Find_My_Id.Result);

   procedure List_Zones_In_Source
     (Source     : TZif.Domain.Value_Object.Source_Info.Source_Info_Type;
      Descending : Boolean;
      Result     : out TZif.Application.Port.Inbound.List_All_Order_By_Id
        .List_All_Zones_Result);

   procedure Load_Source_From_Path
     (Path   : TZif.Application.Port.Inbound.Load_Source.Path_String;
      Result : out TZif.Application.Port.Inbound.Load_Source
        .Load_Source_Result);

   procedure Validate_Source_Path
     (Path   : TZif.Application.Port.Inbound.Validate_Source.Path_String;
      Result : out TZif.Application.Port.Inbound.Validate_Source
        .Validation_Result);

   procedure Find_Zones_By_Pattern
     (Pattern : TZif.Application.Port.Inbound.Find_By_Pattern.Pattern_String;
      Yield   : TZif.Application.Port.Inbound.Find_By_Pattern
        .Yield_Callback_Access;
      Result  : out TZif.Application.Port.Inbound.Find_By_Pattern
        .Find_By_Pattern_Result);

   procedure Find_Zones_By_Region
     (Region : TZif.Application.Port.Inbound.Find_By_Region.Region_String;
      Yield  : TZif.Application.Port.Inbound.Find_By_Region
        .Yield_Callback_Access;
      Result : out TZif.Application.Port.Inbound.Find_By_Region
        .Find_By_Region_Result);

   procedure Find_Zones_By_Regex
     (Regex  : TZif.Application.Port.Inbound.Find_By_Regex.Regex_String;
      Yield  : TZif.Application.Port.Inbound.Find_By_Regex
        .Yield_Callback_Access;
      Result : out TZif.Application.Port.Inbound.Find_By_Regex
        .Find_By_Regex_Result);

end TZif.Infrastructure.IO.Embedded;
