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
--  ===========================================================================

with Ada.Exceptions;
with Ada.Streams;
with Interfaces;
with TZif.Domain.Error;
with TZif.Domain.Parser;

package body TZif.Infrastructure.TZif_Parser is

   use Ada.Streams;
   use Ada.Streams.Stream_IO;
   use TZif.Domain.Error;
   use TZif.Domain.Error.Error_Strings;

   --  ========================================================================
   --  Exception Mapping for I/O Errors
   --  ========================================================================

   function From_Exception
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
   end From_Exception;

   --  ========================================================================
   --  Parse_From_Stream
   --
   --  Read all bytes from stream into buffer, then delegate to Domain.Parser.
   --  ========================================================================

   function Parse_From_Stream
     (Stream : not null Stream_Access) return Parse_Result_Type
   is
      --  Buffer size matches Domain.Parser expectations
      Max_Size    : constant := 65_536;
      Buffer      : TZif.Domain.Parser.Byte_Array (1 .. Max_Size);
      Total_Read  : Natural := 0;
      Chunk       : Stream_Element_Array (1 .. 4_096);
      Last        : Stream_Element_Offset;
      Domain_Res  : TZif.Domain.Parser.Parse_Result_Type;
   begin
      --  Read stream contents into buffer
      loop
         begin
            Read (Stream.all, Chunk, Last);
            exit when Last < Chunk'First;

            for I in Chunk'First .. Last loop
               if Total_Read < Max_Size then
                  Total_Read          := Total_Read + 1;
                  Buffer (Total_Read) := Interfaces.Unsigned_8 (Chunk (I));
               end if;
            end loop;

            exit when Last < Chunk'Last;
         exception
            when Ada.Streams.Stream_IO.End_Error =>
               exit;
         end;
      end loop;

      --  Delegate to Domain.Parser
      if Total_Read = 0 then
         return
           Parse_Result.Error (IO_Error, "No data read from stream");
      end if;

      TZif.Domain.Parser.Parse_From_Bytes (Buffer, Total_Read, Domain_Res);

      --  Convert Domain result to Infrastructure result
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

   exception
      when Occ : others =>
         return Parse_Result.From_Error (From_Exception (Occ));
   end Parse_From_Stream;

   --  ========================================================================
   --  Parse_From_File
   --
   --  Open file, read bytes, delegate to Domain.Parser, close file.
   --  ========================================================================

   function Parse_From_File (File_Path : String) return Parse_Result_Type is
      File   : File_Type;
      Stream : Stream_Access;
      Result : Parse_Result_Type;
   begin
      Open (File, In_File, File_Path);

      begin
         Stream := Stream_IO.Stream (File);
         Result := Parse_From_Stream (Stream);
      exception
         when Occ : others =>
            if Is_Open (File) then
               Close (File);
            end if;
            return Parse_Result.From_Error (From_Exception (Occ));
      end;

      Close (File);
      return Result;

   exception
      when Occ : others =>
         return Parse_Result.From_Error (From_Exception (Occ));
   end Parse_From_File;

end TZif.Infrastructure.TZif_Parser;
