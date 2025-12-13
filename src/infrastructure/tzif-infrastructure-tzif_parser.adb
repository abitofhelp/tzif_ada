pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.TZif_Parser
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Thin I/O wrapper for TZif parsing. Handles file operations and
--    delegates actual parsing to Domain.Parser.
--
--  Architecture:
--    Infrastructure layer - performs I/O operations only.
--    Pure parsing logic lives in TZif.Domain.Parser.
--
--  Implementation Notes:
--    Uses Functional.Try.Map_To_Result_With_Param to wrap exception-prone
--    file I/O operations, converting exceptions to Result types for
--    railway-oriented error handling. This is the ONLY place exceptions are
--    caught in the infrastructure layer.
--
--  ===========================================================================

with Ada.Streams;
with Functional.Scoped;
with Functional.Try;
with Functional.Try.Map_To_Result_With_Param;
with Interfaces;
with TZif.Domain.Error;
with TZif.Domain.Parser;

package body TZif.Infrastructure.TZif_Parser is

   use Ada.Streams;
   use Ada.Streams.Stream_IO;
   use TZif.Domain.Error;
   use TZif.Domain.Error.Error_Strings;

   --  ========================================================================
   --  Make Error Result (used by Map_To_Result)
   --  ========================================================================

   function Make_Parse_Error
     (Kind : Error_Kind; Message : String) return Parse_Result.Result
   is
   begin
      return Parse_Result.Error (Kind, Message);
   end Make_Parse_Error;

   --  ========================================================================
   --  Parse_From_Stream
   --
   --  Read all bytes from stream into buffer, then delegate to Domain.Parser.
   --  ========================================================================

   --  Stream parsing context passed to raw action
   type Stream_Context is record
      Stream : Stream_Access;
   end record;

   --  Raw action that may raise exceptions - wrapped by Functional.Try
   function Raw_Parse_Stream (Ctx : Stream_Context) return Parse_Result.Result is
      Max_Size   : constant := 65_536;
      Buffer     : TZif.Domain.Parser.Byte_Array (1 .. Max_Size);
      Total_Read : Natural := 0;
      Chunk      : Stream_Element_Array (1 .. 4_096);
      Last       : Stream_Element_Offset;
      Domain_Res : TZif.Domain.Parser.Parse_Result_Type;
   begin
      --  Read stream contents into buffer
      loop
         Read (Ctx.Stream.all, Chunk, Last);
         exit when Last < Chunk'First;

         for I in Chunk'First .. Last loop
            if Total_Read < Max_Size then
               Total_Read          := Total_Read + 1;
               Buffer (Total_Read) := Interfaces.Unsigned_8 (Chunk (I));
            end if;
         end loop;

         exit when Last < Chunk'Last;
      end loop;

      --  Validate we read data
      if Total_Read = 0 then
         return Parse_Result.Error (IO_Error, "No data read from stream");
      end if;

      --  Delegate to Domain.Parser
      TZif.Domain.Parser.Parse_From_Bytes (Buffer, Total_Read, Domain_Res);

      --  Convert Domain.Parser result to our result
      if TZif.Domain.Parser.Parse_Result.Is_Ok (Domain_Res) then
         return Parse_Result.Ok
           (TZif.Domain.Parser.Parse_Result.Value (Domain_Res));
      else
         declare
            Err : constant Error_Type :=
              TZif.Domain.Parser.Parse_Result.Error_Info (Domain_Res);
         begin
            return Parse_Result.Error (Err.Kind, To_String (Err.Message));
         end;
      end if;
   end Raw_Parse_Stream;

   --  Declarative exception-to-Result mapping for stream parsing
   package Try_Parse_Stream is new Functional.Try.Map_To_Result_With_Param
     (Error_Kind_Type    => Error_Kind,
      Param_Type         => Stream_Context,
      Result_Type        => Parse_Result.Result,
      Make_Error         => Make_Parse_Error,
      Default_Error_Kind => IO_Error,
      Action             => Raw_Parse_Stream);

   --  Exception mappings: Name_Error -> Not_Found, others -> default (IO_Error)
   Stream_Mappings : constant Try_Parse_Stream.Mapping_Array :=
     [(Ada.Streams.Stream_IO.Name_Error'Identity, Not_Found_Error)];

   function Parse_From_Stream
     (Stream : not null Stream_Access) return Parse_Result_Type
   is
   begin
      return Try_Parse_Stream.Run ((Stream => Stream), Stream_Mappings);
   end Parse_From_Stream;

   --  ========================================================================
   --  Parse_From_File
   --
   --  Open file, read bytes, delegate to Domain.Parser, close file.
   --  Uses Functional.Scoped for automatic file cleanup on any exit path.
   --  ========================================================================

   --  Scoped file guard - automatically closes file when leaving scope
   package File_Guard is new Functional.Scoped.Conditional_Guard_For
     (Resource       => File_Type,
      Should_Release => Is_Open,
      Release        => Close);

   --  Raw action that may raise exceptions - wrapped by Functional.Try
   function Raw_Parse_File (File_Path : String) return Parse_Result.Result is
      File   : aliased File_Type;
      Guard  : File_Guard.Guard (File'Access);  --  Auto-close on scope exit
      pragma Unreferenced (Guard);  --  RAII: releases via Finalize
      Stream : Stream_Access;
   begin
      Open (File, In_File, File_Path);
      Stream := Stream_IO.Stream (File);
      return Raw_Parse_Stream ((Stream => Stream));
      --  Guard.Finalize called here - closes file if open
   end Raw_Parse_File;

   --  Declarative exception-to-Result mapping for file parsing
   package Try_Parse_File is new Functional.Try.Map_To_Result_With_Param
     (Error_Kind_Type    => Error_Kind,
      Param_Type         => String,
      Result_Type        => Parse_Result.Result,
      Make_Error         => Make_Parse_Error,
      Default_Error_Kind => IO_Error,
      Action             => Raw_Parse_File);

   --  Exception mappings: Name_Error -> Not_Found, others -> default (IO_Error)
   File_Mappings : constant Try_Parse_File.Mapping_Array :=
     [(Ada.Streams.Stream_IO.Name_Error'Identity, Not_Found_Error)];

   function Parse_From_File (File_Path : String) return Parse_Result_Type is
   begin
      return Try_Parse_File.Run (File_Path, File_Mappings);
   end Parse_From_File;

end TZif.Infrastructure.TZif_Parser;
