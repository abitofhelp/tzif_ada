pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.TZif_Parser
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
--    Parse_Result_Type
--
--  Dependencies:
--    TZif.Domain.Error.Result
--    TZif.Domain.TZif_Data
--
--  ===========================================================================

with Ada.Streams.Stream_IO;
with TZif.Domain.Error.Result;
with TZif.Domain.TZif_Data;

package TZif.Infrastructure.TZif_Parser is

   use TZif.Domain.TZif_Data;

   --  ========================================================================
   --  Result Type
   --  ========================================================================

   --  Result type for parse operations using domain Result monad
   package Parse_Result is new Domain.Error.Result.Generic_Result
     (T => TZif_Data_Type);

   subtype Parse_Result_Type is Parse_Result.Result;

   --  ========================================================================
   --  Parser Functions
   --  ========================================================================

   --  Parse TZif file from an open stream
   --  Returns Result containing either parsed data or error
   function Parse_From_Stream
     (Stream : not null Ada.Streams.Stream_IO.Stream_Access)
      return Parse_Result_Type;

   --  Parse TZif file from file path
   --  Opens file, parses, and closes file
   function Parse_From_File (File_Path : String) return Parse_Result_Type;

end TZif.Infrastructure.TZif_Parser;
