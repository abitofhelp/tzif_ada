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
--    Uses Functional.Try.Try_To_Any_Result_With_Param to wrap exception-prone
--    file I/O operations, converting exceptions to Result types for
--    railway-oriented error handling. This is the ONLY place exceptions are
--    caught in the infrastructure layer.
--
--  ===========================================================================

with Ada.Exceptions;
with Ada.Streams;
with Functional.Scoped;
with Functional.Try;
with Interfaces;
with TZif.Domain.Error;
with TZif.Domain.Parser;

package body TZif.Infrastructure.TZif_Parser is

   use Ada.Streams;
   use Ada.Streams.Stream_IO;
   use TZif.Domain.Error;
   use TZif.Domain.Error.Error_Strings;

   --  ========================================================================
   --  Exception Mapping for I/O Errors (used by Functional.Try)
   --  ========================================================================

   function Map_Exception
     (Occ : Ada.Exceptions.Exception_Occurrence) return Error_Type
   is
      use Ada.Exceptions;
      Exc_Name : constant String := Exception_Name (Occ);
   begin
      if Exc_Name = "STORAGE_ERROR" then
         return
           (Kind    => Resource_Error,
            Message =>
              To_Bounded_String
                ("Out of memory during file I/O: " & Exception_Message (Occ)));
      elsif Exc_Name = "ADA.IO_EXCEPTIONS.END_ERROR" then
         return
           (Kind    => Parse_Error,
            Message =>
              To_Bounded_String
                ("Unexpected end of file: " & Exception_Message (Occ)));
      elsif Exc_Name = "ADA.IO_EXCEPTIONS.DATA_ERROR" then
         return
           (Kind    => Parse_Error,
            Message =>
              To_Bounded_String
                ("Corrupted or malformed data: " & Exception_Message (Occ)));
      elsif Exc_Name = "ADA.IO_EXCEPTIONS.NAME_ERROR" then
         return
           (Kind    => Not_Found_Error,
            Message =>
              To_Bounded_String
                ("File not found: " & Exception_Message (Occ)));
      elsif Exc_Name = "ADA.IO_EXCEPTIONS.USE_ERROR" then
         return
           (Kind    => IO_Error,
            Message =>
              To_Bounded_String
                ("Cannot open file: " & Exception_Message (Occ)));
      else
         return
           (Kind    => IO_Error,
            Message =>
              To_Bounded_String
                ("File I/O error: " & Exception_Message (Occ)));
      end if;
   end Map_Exception;

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
   function Raw_Parse_Stream (Ctx : Stream_Context) return TZif_Data_Type is
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
         raise Constraint_Error with "No data read from stream";
      end if;

      --  Delegate to Domain.Parser
      TZif.Domain.Parser.Parse_From_Bytes (Buffer, Total_Read, Domain_Res);

      --  Extract result or re-raise as exception for Try to catch
      if TZif.Domain.Parser.Parse_Result.Is_Ok (Domain_Res) then
         return TZif.Domain.Parser.Parse_Result.Value (Domain_Res);
      else
         declare
            Err : constant Error_Type :=
              TZif.Domain.Parser.Parse_Result.Error_Info (Domain_Res);
         begin
            raise Constraint_Error with To_String (Err.Message);
         end;
      end if;
   end Raw_Parse_Stream;

   --  Instantiate Functional.Try for stream parsing
   function Try_Parse_Stream is new Functional.Try.Try_To_Any_Result_With_Param
     (T             => TZif_Data_Type,
      E             => Error_Type,
      Param         => Stream_Context,
      Result_Type   => Parse_Result.Result,
      Ok            => Parse_Result.Ok,
      New_Error     => Parse_Result.From_Error,
      Map_Exception => Map_Exception,
      Action        => Raw_Parse_Stream);

   function Parse_From_Stream
     (Stream : not null Stream_Access) return Parse_Result_Type
   is
   begin
      return Try_Parse_Stream ((Stream => Stream));
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
   function Raw_Parse_File (File_Path : String) return TZif_Data_Type is
      File   : aliased File_Type;
      Guard  : File_Guard.Guard (File'Access);  --  Auto-close on scope exit
      Stream : Stream_Access;
   begin
      Open (File, In_File, File_Path);
      Stream := Stream_IO.Stream (File);
      return Raw_Parse_Stream ((Stream => Stream));
      --  Guard.Finalize called here - closes file if open
   end Raw_Parse_File;

   --  Instantiate Functional.Try for file parsing
   function Try_Parse_File is new Functional.Try.Try_To_Any_Result_With_Param
     (T             => TZif_Data_Type,
      E             => Error_Type,
      Param         => String,
      Result_Type   => Parse_Result.Result,
      Ok            => Parse_Result.Ok,
      New_Error     => Parse_Result.From_Error,
      Map_Exception => Map_Exception,
      Action        => Raw_Parse_File);

   function Parse_From_File (File_Path : String) return Parse_Result_Type is
   begin
      return Try_Parse_File (File_Path);
   end Parse_From_File;

end TZif.Infrastructure.TZif_Parser;
