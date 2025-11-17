pragma Ada_2022;
--  ===========================================================================
--  Tzif.Infrastructure.Io.Desktop
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Desktop implementation.
--
--  ===========================================================================

with Ada.Streams.Stream_IO;
with Ada.Exceptions;
with TZif.Domain.Error;
with TZif.Domain.Value_Object.Zone_Id;

package body TZif.Infrastructure.IO.Desktop with
  SPARK_Mode => Off
is

   use Ada.Streams.Stream_IO;
   use TZif.Domain.Value_Object.Zone_Id;

   --  Standard zoneinfo base path on POSIX systems
   Zoneinfo_Base : constant String := "/usr/share/zoneinfo/";

   ----------------------------------------------------------------------
   --  Read_File
   --
   --  Read TZif binary file from filesystem.
   --
   --  Implementation:
   --    1. Construct file path from zone ID
   --    2. Open file for reading
   --    3. Read bytes into buffer
   --    4. Return byte count or error
   ----------------------------------------------------------------------
   procedure Read_File
     (Id     :     TZif.Application.Port.Inbound.Find_By_Id.Zone_Id_Input_Type;
      Bytes  : out Byte_Array; Length : out Natural;
      Result : out Read_File_Result.Result)
   is
      use Ada.Exceptions;
      File      : File_Type;
      Stream    : Stream_Access;
      File_Path : constant String := Zoneinfo_Base & To_String (Id);
   begin
      Length := 0;

      --  Step 1: Open TZif file
      begin
         Open (File, In_File, File_Path);
      exception
         when E : Name_Error =>
            Result :=
              Read_File_Result.Error
                (TZif.Domain.Error.Infrastructure_Error,
                 "Zone file not found: " & File_Path & ": " &
                 Exception_Message (E));
            return;
         when E : Use_Error  =>
            Result :=
              Read_File_Result.Error
                (TZif.Domain.Error.Infrastructure_Error,
                 "Cannot access zone file: " & File_Path & ": " &
                 Exception_Message (E));
            return;
         when E : others     =>
            Result :=
              Read_File_Result.Error
                (TZif.Domain.Error.Infrastructure_Error,
                 "File open error for " & File_Path & ": " &
                 Exception_Message (E));
            return;
      end;

      --  Step 2: Read file bytes
      begin
         Stream := Ada.Streams.Stream_IO.Stream (File);

         --  Read bytes one at a time into buffer
         while not End_Of_File (File) and then Length < Bytes'Length loop
            Length := Length + 1;
            Unsigned_8'Read (Stream, Bytes (Length));
         end loop;

         Close (File);

         --  Return success with read info
         Result := Read_File_Result.Ok ((Bytes_Read => Length));

      exception
         when E : End_Error  =>
            if Is_Open (File) then
               Close (File);
            end if;
            Result :=
              Read_File_Result.Error
                (TZif.Domain.Error.Infrastructure_Error,
                 "Unexpected end of file for " & File_Path & ": " &
                 Exception_Message (E));
         when E : Data_Error =>
            if Is_Open (File) then
               Close (File);
            end if;
            Result :=
              Read_File_Result.Error
                (TZif.Domain.Error.Infrastructure_Error,
                 "File data corrupted for " & File_Path & ": " &
                 Exception_Message (E));
         when E : others     =>
            if Is_Open (File) then
               Close (File);
            end if;
            Result :=
              Read_File_Result.Error
                (TZif.Domain.Error.Infrastructure_Error,
                 "File read error for " & File_Path & ": " &
                 Exception_Message (E));
      end;

   end Read_File;

   ----------------------------------------------------------------------
   --  Read_Cache_File
   --
   --  FUTURE FEATURE: Cache persistence deferred to future release
   --  See docs/roadmap.md #12 "Cache Persistence Investigation"
   --
   --  Current in-memory cache performs excellently (20ms cold start).
   --  Implementation deferred pending user demand and performance
   --  requirements.
   ----------------------------------------------------------------------
   procedure Read_Cache_File
     (Path   :     TZif.Application.Port.Inbound.Import_Cache.Path_String;
      Bytes  : out Byte_Array; Length : out Natural;
      Result : out Read_Cache_Result.Result)
   is
      pragma Unreferenced (Path, Bytes);
   begin
      --  FUTURE: Implement cache file reading:
      --    - Open cache file at given path
      --    - Read JSON content into Bytes buffer
      --    - Set Length to number of bytes read
      --    - Return Ok(Read_Cache_Info) or Error(IO_Error)

      --  STUB: Return error for now
      Length := 0;
      Result :=
        Read_Cache_Result.Error
          (TZif.Domain.Error.Infrastructure_Error,
           "Read_Cache_File: Not yet implemented (see roadmap.md)");

   end Read_Cache_File;

   ----------------------------------------------------------------------
   --  Write_Cache_File
   --
   --  FUTURE FEATURE: Cache persistence deferred to future release
   --  See docs/roadmap.md #12 "Cache Persistence Investigation"
   --
   --  Current in-memory cache performs excellently (20ms cold start).
   --  Implementation deferred pending user demand and performance
   --  requirements.
   ----------------------------------------------------------------------
   procedure Write_Cache_File
     (Path   :     TZif.Application.Port.Inbound.Export_Cache.Path_String;
      Bytes  :     Byte_Array; Length : Natural; Overwrite : Boolean;
      Result : out Write_Cache_Result.Result)
   is
      pragma Unreferenced (Path, Bytes, Length, Overwrite);
   begin
      --  FUTURE: Implement cache file writing:
      --    - Check if file exists
      --    - If exists and not Overwrite, return error
      --    - Create/open file at given path
      --    - Write Bytes(1..Length) to file
      --    - Return Ok(Write_Cache_Info) or Error(IO_Error)

      --  STUB: Return error for now
      Result :=
        Write_Cache_Result.Error
          (TZif.Domain.Error.Infrastructure_Error,
           "Write_Cache_File: Not yet implemented (see roadmap.md)");

   end Write_Cache_File;

   ----------------------------------------------------------------------
   --  List_Directory_Sources
   --
   --  Discover timezone sources in directory paths.
   ----------------------------------------------------------------------
   procedure List_Directory_Sources
     (Search_Paths : TZif.Application.Port.Inbound.Discover_Sources.Path_List;
      Result       : out TZif.Application.Port.Inbound.Discover_Sources
        .Discovery_Result_Package
        .Result)
   is
      use TZif.Application.Port.Inbound.Discover_Sources;
      Empty_Data : constant Discovery_Data_Type :=
        (Sources => Source_Info_Vectors.Empty_Vector,
         Errors  => Error_Vectors.Empty_Vector);
   begin
      --  TODO: Implement directory source discovery:
      --    - For each path in Search_Paths:
      --      - Check if directory exists
      --      - Look for VERSION file
      --      - Count timezone files
      --      - Create Source_Info record
      --    - Return Ok(Discovery_Data) or Error(IO_Error)

      --  STUB: Return empty success for now
      pragma Unreferenced (Search_Paths);
      Result := Discovery_Result_Package.Ok (Empty_Data);

   end List_Directory_Sources;

   ----------------------------------------------------------------------
   --  Get_Modified_Time
   --
   --  DROPPED FEATURE: No known use case
   --
   --  This operation was originally planned but dropped during v1.0.0
   --  development. Cache invalidation is handled through other
   --  mechanisms defined in the SRS.
   --  If a use case emerges, this can be reconsidered in future releases.
   ----------------------------------------------------------------------
   procedure Get_Modified_Time
     (Id        : TZif.Application.Port.Inbound.Find_By_Id.Zone_Id_Input_Type;
      Timestamp : out Timestamp_Type;
      Result    : out Get_Modified_Time_Result.Result)
   is
      use Ada.Calendar;
   begin
      --  DROPPED: No implementation needed
      --  Returning current time as placeholder to satisfy signature

      pragma Unreferenced (Id);
      Timestamp := Clock;
      Result    := Get_Modified_Time_Result.Ok (Timestamp);

   end Get_Modified_Time;

end TZif.Infrastructure.IO.Desktop;
