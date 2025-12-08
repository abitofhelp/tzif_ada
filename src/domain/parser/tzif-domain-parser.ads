pragma Ada_2022;
--  ===========================================================================
--  TZif.Domain.Parser
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    TZif binary format parser implementation.
--
--  Supported Versions:
--    - TZif version 1 (legacy)
--    - TZif version 2 (64-bit)
--    - TZif version 3 (with extensions)
--
--  Key Types:
--    Byte_Array
--    Parse_Result_Type
--
--  Dependencies:
--    Interfaces
--    TZif.Domain.Error.Result
--    TZif.Domain.TZif_Data
--
--  ===========================================================================

with Interfaces;
with TZif.Domain.Error.Result;
with TZif.Domain.TZif_Data;

package TZif.Domain.Parser with
  SPARK_Mode => On
is

   use Interfaces;
   use TZif.Domain.TZif_Data;

   --  ========================================================================
   --  Byte Array Type
   --  ========================================================================

   --  Raw byte buffer for TZif binary data
   type Byte_Array is array (Positive range <>) of Unsigned_8;

   --  ========================================================================
   --  Parse Result Type
   --  ========================================================================

   --  Result type for parse operations
   package Parse_Result is new TZif.Domain.Error.Result.Generic_Result
     (T => TZif_Data_Type);
   subtype Parse_Result_Type is Parse_Result.Result;

   --  ========================================================================
   --  Parser Operations
   --  ========================================================================

   -------------------------------------------------------------------------
   --  Parse_From_Bytes
   --
   --  Parses TZif binary data from a byte buffer.
   --
   --  This is the main entry point for TZif parsing. It:
   --    1. Reads the TZif magic number ("TZif")
   --    2. Determines the format version (1, 2, or 3)
   --    3. Dispatches to the appropriate version-specific parser
   --    4. Returns parsed TZif_Data_Type or parse error
   --
   --  Parameters:
   --    Bytes  : Input buffer containing TZif binary data
   --    Length : Number of valid bytes in the buffer
   --    Result : Ok(TZif_Data_Type) or Error(Parse_Error)
   --
   --  Preconditions:
   --    - Length <= Bytes'Length (buffer contains at least Length bytes)
   --    - Length > 0 (non-empty buffer)
   --    - Bytes contains valid TZif binary data starting at index 1
   --
   --  Postconditions:
   --    - If Ok: Result contains valid TZif_Data_Type
   --    - If Error: Result contains descriptive parse error
   --
   --  SPARK Notes:
   --    - No I/O performed (operates purely on byte array)
   --    - Deterministic (same input → same output)
   --    - No side effects
   --    - Memory safe (all bounds checked)
   -------------------------------------------------------------------------
   procedure Parse_From_Bytes
     (Bytes  :     Byte_Array; Length : Natural;
      Result : out Parse_Result_Type) with
     Pre  => Length <= Bytes'Length and then Length > 0,
     Post => True;
   --  ROADMAP: Strengthen postconditions (see roadmap.md):
   --    If Result.Is_Ok then Result.Value is valid TZif_Data_Type
   --    If Result.Is_Error then Result.Error explains parse failure

end TZif.Domain.Parser;
