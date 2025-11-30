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
