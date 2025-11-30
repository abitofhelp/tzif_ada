pragma Ada_2022;
--  ===========================================================================
--  Tzif.Application.Operations
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
      --  TODO: Add other I/O-related use cases as they are migrated:
      --    - Validate_Source
      --    - Load_Source
      --    - Get_Version
      --    - Get_Transition_At_Epoch
      --    - List_All_Order_By_Id
      --    - Find_By_Pattern / Find_By_Regex / Find_By_Region
      ----------------------------------------------------------------------

   end All_Operations;

end TZif.Application.Operations;
