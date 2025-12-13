pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.Io.Desktop
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Desktop implementation.
--
--  Implementation Notes:
--    Uses Functional.Try and Functional.Scoped for exception boundary
--    handling. All I/O operations that may raise exceptions are wrapped with
--    Functional.Try.Map_To_Result, and file handles use
--    Functional.Scoped.Conditional_Guard_For for automatic cleanup.
--
--  ===========================================================================

with Ada.Streams.Stream_IO;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Characters.Handling;
with Functional.Option;
with Functional.Result;
with Functional.Scoped;
with Functional.Try;
with Functional.Try.Map_To_Result;
with Functional.Try.Map_To_Result_With_Param;
with GNAT.Regpat;
with TZif.Domain.Error;
with TZif.Domain.Value_Object.Zone_Id;
with TZif.Domain.Value_Object.Unit;
with TZif.Infrastructure.Platform;
with TZif.Infrastructure.Platform.POSIX;
with TZif.Infrastructure.ULID;

package body TZif.Infrastructure.IO.Desktop with
  SPARK_Mode => Off
is

   use TZif.Domain.Value_Object.Zone_Id;
   use TZif.Domain.Value_Object.Source_Info;

   --  Rename Stream_IO to avoid namespace conflicts with Ada.Directories
   package SIO renames Ada.Streams.Stream_IO;

   --  ========================================================================
   --  Scoped File Guard for Stream_IO
   --  ========================================================================

   package Stream_File_Guard is new Functional.Scoped.Conditional_Guard_For
     (Resource       => SIO.File_Type,
      Should_Release => SIO.Is_Open,
      Release        => SIO.Close);

   --  ========================================================================
   --  Scoped File Guard for Text_IO
   --  ========================================================================

   package Text_File_Guard is new Functional.Scoped.Conditional_Guard_For
     (Resource       => Ada.Text_IO.File_Type,
      Should_Release => Ada.Text_IO.Is_Open,
      Release        => Ada.Text_IO.Close);
   pragma Unreferenced (Text_File_Guard);  --  For later refactoring

   --  ========================================================================
   --  Default Zoneinfo Path
   --  ========================================================================
   --
   --  This default path is used for operations that don't receive an explicit
   --  source path. The developer should always provide explicit paths when
   --  creating tzif/zoneinfo instances for cross-platform compatibility.
   --
   --  Platform notes:
   --    - Linux/BSD/macOS: /usr/share/zoneinfo is typically pre-installed
   --    - Windows: No default exists; developer must provide path to
   --               downloaded IANA tzdata (https://www.iana.org/time-zones)
   --  ========================================================================

   Zoneinfo_Base : constant String := "/usr/share/zoneinfo/";

   ----------------------------------------------------------------------
   --  Read_File
   --
   --  Read TZif binary file from filesystem.
   --
   --  Implementation:
   --    Uses Functional.Try for exception-to-Result conversion and
   --    Functional.Scoped for automatic file cleanup.
   ----------------------------------------------------------------------

   --  Context for Read_File raw action
   type Read_File_Context is record
      File_Path   : access constant String;
      Bytes       : access Byte_Array;
      Bytes_Read  : Natural := 0;
   end record;

   --  Raw action that may raise - reads file bytes into buffer
   --  Returns Result type (Ok on success) for Map_To_Result_With_Param
   function Raw_Read_File
     (Ctx : Read_File_Context) return Read_File_Result.Result
   is
      File   : aliased SIO.File_Type;
      Guard  : Stream_File_Guard.Guard (File'Access);
      pragma Unreferenced (Guard);  --  RAII: releases via Finalize
      Stream : SIO.Stream_Access;
      Length : Natural := 0;
   begin
      SIO.Open (File, SIO.In_File, Ctx.File_Path.all);
      Stream := SIO.Stream (File);

      --  Read bytes one at a time into buffer
      while not SIO.End_Of_File (File)
        and then Length < Ctx.Bytes'Length
      loop
         Length := Length + 1;
         Unsigned_8'Read (Stream, Ctx.Bytes (Length));
      end loop;

      --  Guard.Finalize will close file automatically
      return Read_File_Result.Ok ((Bytes_Read => Length));
   end Raw_Read_File;

   --  Make_Error for Read_File (adds context-appropriate prefixes)
   function Make_Read_Error
     (Kind : TZif.Domain.Error.Error_Kind; Message : String)
      return Read_File_Result.Result
   is
      use TZif.Domain.Error;
   begin
      case Kind is
         when Not_Found_Error =>
            return Read_File_Result.Error (Not_Found_Error,
              "File not found: " & Message);
         when IO_Error =>
            return Read_File_Result.Error (IO_Error,
              "Cannot access file: " & Message);
         when Parse_Error =>
            return Read_File_Result.Error (Parse_Error,
              "File read error: " & Message);
         when others =>
            return Read_File_Result.Error (IO_Error,
              "I/O error: " & Message);
      end case;
   end Make_Read_Error;

   --  Instantiate Functional.Try.Map_To_Result_With_Param for Read_File
   package Try_Read_File is new Functional.Try.Map_To_Result_With_Param
     (Error_Kind_Type    => TZif.Domain.Error.Error_Kind,
      Param_Type         => Read_File_Context,
      Result_Type        => Read_File_Result.Result,
      Make_Error         => Make_Read_Error,
      Default_Error_Kind => TZif.Domain.Error.IO_Error,
      Action             => Raw_Read_File);

   --  Convert IO_Mappings to Try_Read_File.Mapping_Array
   Read_File_Mappings : constant Try_Read_File.Mapping_Array :=
     [(SIO.Name_Error'Identity, TZif.Domain.Error.Not_Found_Error),
      (SIO.Use_Error'Identity,  TZif.Domain.Error.IO_Error),
      (SIO.End_Error'Identity,  TZif.Domain.Error.Parse_Error),
      (SIO.Data_Error'Identity, TZif.Domain.Error.Parse_Error)];

   procedure Read_File
     (Id     :     TZif.Application.Port.Inbound.Find_By_Id.Zone_Id_Input_Type;
      Bytes  : out Byte_Array; Length : out Natural;
      Result : out Read_File_Result.Result)
   is
      File_Path : aliased constant String := Zoneinfo_Base & To_String (Id);
      Ctx       : Read_File_Context;
   begin
      Length := 0;
      Ctx := (File_Path  => File_Path'Unchecked_Access,
              Bytes      => Bytes'Unrestricted_Access,
              Bytes_Read => 0);

      Result := Try_Read_File.Run (Ctx, Read_File_Mappings);

      --  Extract bytes read from successful result
      if Read_File_Result.Is_Ok (Result) then
         Length := Read_File_Result.Value (Result).Bytes_Read;
      end if;
   end Read_File;

   ----------------------------------------------------------------------
   --  List_Directory_Sources
   --
   --  Discover timezone sources in directory paths.
   --
   --  Implementation:
   --    For each path in Search_Paths:
   --      1. Check if directory exists
   --      2. Look for +VERSION or VERSION file
   --      3. Count TZif files (files with TZif magic number)
   --      4. Create Source_Info record with generated ULID
   --    Return Ok(Discovery_Data) or collect errors
   ----------------------------------------------------------------------
   procedure List_Directory_Sources
     (Search_Paths : TZif.Application.Port.Inbound.Discover_Sources.Path_List;
      Result       : out TZif.Application.Port.Inbound.Discover_Sources
        .Discovery_Result_Package
        .Result)
   is
      use Ada.Strings.Fixed;
      use Ada.Directories;
      package Discover renames TZif.Application.Port.Inbound.Discover_Sources;

      Data : Discover.Discovery_Data_Type :=
        (Sources => Discover.Source_Info_Vectors.Empty_Vector,
         Errors  => Discover.Error_Vectors.Empty_Vector);

      --  Read first few bytes of file to check TZif magic
      --  Uses Functional.Try with Option for safe file reading
      package Bool_Option is new Functional.Option (T => Boolean);

      function Raw_Check_TZif_Magic (Path : String) return Boolean is
         File   : aliased SIO.File_Type;
         Guard  : Stream_File_Guard.Guard (File'Access);
         pragma Unreferenced (Guard);  --  RAII: releases via Finalize
         Stream : SIO.Stream_Access;
         Magic  : String (1 .. 4);
      begin
         if Kind (Path) /= Ordinary_File then
            return False;
         end if;
         SIO.Open (File, SIO.In_File, Path);
         Stream := SIO.Stream (File);
         String'Read (Stream, Magic);
         return Magic = "TZif";
      end Raw_Check_TZif_Magic;

      function Try_Check_TZif is new Functional.Try.Try_To_Option_With_Param
        (T          => Boolean,
         Param      => String,
         Option_Pkg => Bool_Option,
         Action     => Raw_Check_TZif_Magic);

      function Is_TZif_File (Path : String) return Boolean is
         Result : constant Bool_Option.Option := Try_Check_TZif (Path);
      begin
         return Bool_Option.Unwrap_Or (Result, Default => False);
      end Is_TZif_File;

      --  Read version from file using Functional.Try + Scoped
      Max_Version_Len : constant := 64;
      subtype Version_Buffer is String (1 .. Max_Version_Len);
      package Version_Option is new Functional.Option (T => Version_Buffer);

      function Raw_Read_Version (Path : String) return Version_Buffer is
         File   : aliased SIO.File_Type;
         Guard  : Stream_File_Guard.Guard (File'Access);
         pragma Unreferenced (Guard);  --  RAII: releases via Finalize
         Stream : SIO.Stream_Access;
         Buffer : Version_Buffer := [others => ' '];
         Len    : Natural := 0;
         Ch     : Character;
      begin
         SIO.Open (File, SIO.In_File, Path);
         Stream := SIO.Stream (File);

         --  Read until newline or buffer full
         while not SIO.End_Of_File (File) and then Len < Buffer'Last loop
            Character'Read (Stream, Ch);
            exit when Ch = ASCII.LF or else Ch = ASCII.CR;
            Len := Len + 1;
            Buffer (Len) := Ch;
         end loop;

         return Buffer;
      end Raw_Read_Version;

      function Try_Read_Version is new Functional.Try.Try_To_Option_With_Param
        (T          => Version_Buffer,
         Param      => String,
         Option_Pkg => Version_Option,
         Action     => Raw_Read_Version);

      function Read_Version_File (Path : String) return String is
         Default_Version : Version_Buffer := [others => ' '];
         Opt    : constant Version_Option.Option := Try_Read_Version (Path);
         Buffer : Version_Buffer;
         Len    : Natural := Max_Version_Len;
      begin
         Default_Version (1 .. 7) := "unknown";
         Buffer := Version_Option.Unwrap_Or (Opt, Default => Default_Version);
         --  Trim trailing spaces
         while Len > 0 and then Buffer (Len) = ' ' loop
            Len := Len - 1;
         end loop;
         return Trim (Buffer (1 .. Len), Ada.Strings.Both);
      end Read_Version_File;

      --  Count TZif files in directory (recursive)
      --  Uses Functional.Try to handle directory access errors
      package Natural_Option is new Functional.Option (T => Natural);

      --  Forward declaration for recursive calls
      function Count_TZif_Files (Dir_Path : String) return Natural;

      function Raw_Count_TZif_Files (Dir_Path : String) return Natural is
         Count  : Natural := 0;
         Search : Search_Type;
         Item   : Directory_Entry_Type;
      begin
         Start_Search (Search, Dir_Path, "*", [others => True]);

         while More_Entries (Search) loop
            Get_Next_Entry (Search, Item);

            declare
               Name      : constant String := Simple_Name (Item);
               Full_Path : constant String := Full_Name (Item);
            begin
               --  Skip . and ..
               if Name = "." or else Name = ".." then
                  null;
               elsif Kind (Item) = Directory then
                  --  Skip posix, right, etc. directories
                  if Name /= "posix"
                    and then Name /= "right"
                    and then Name /= "posixrules"
                  then
                     Count := Count + Count_TZif_Files (Full_Path);
                  end if;
               elsif Kind (Item) = Ordinary_File then
                  --  Check if it's a TZif file
                  if Is_TZif_File (Full_Path) then
                     Count := Count + 1;
                  end if;
               end if;
            end;
         end loop;

         End_Search (Search);
         return Count;
      end Raw_Count_TZif_Files;

      function Try_Count_TZif is new Functional.Try.Try_To_Option_With_Param
        (T          => Natural,
         Param      => String,
         Option_Pkg => Natural_Option,
         Action     => Raw_Count_TZif_Files);

      function Count_TZif_Files (Dir_Path : String) return Natural is
         Result : constant Natural_Option.Option := Try_Count_TZif (Dir_Path);
      begin
         return Natural_Option.Unwrap_Or (Result, Default => 0);
      end Count_TZif_Files;

      --  Generate simple ULID-like identifier
      function Generate_Simple_ULID return ULID_Type is
         use Ada.Calendar;
         Now        : constant Time    := Clock;
         Y          : Year_Number;
         Mo         : Month_Number;
         D          : Day_Number;
         Secs       : Day_Duration;
         Timestamp  : Natural;
         Result_Str : String (1 .. 26) := [others => '0'];
         Base32     : constant String  := "0123456789ABCDEFGHJKMNPQRSTVWXYZ";
         Idx        : Natural;
      begin
         Split (Now, Y, Mo, D, Secs);
         --  Simple timestamp encoding
         Timestamp := (Y - 2000) * 10_000_000 + Mo * 100_000 + D * 1_000 +
           Natural (Secs / 100.0);

         --  Encode first 10 chars from timestamp
         for I in reverse 1 .. 10 loop
            Idx              := (Timestamp mod 32) + 1;
            Result_Str (I)   := Base32 (Idx);
            Timestamp        := Timestamp / 32;
         end loop;

         --  Fill rest with pseudo-random based on Day_Duration
         declare
            Rand : Natural := Natural (Secs * 1_000.0) mod 1_000_000;
         begin
            for I in 11 .. 26 loop
               Idx            := (Rand mod 32) + 1;
               Result_Str (I) := Base32 (Idx);
               Rand           := (Rand * 7 + 13) mod 1_000_000;
            end loop;
         end;

         return Make_ULID (Result_Str);
      end Generate_Simple_ULID;

      --  Helper to add error to Data.Errors
      procedure Add_Error
        (Kind : TZif.Domain.Error.Error_Kind; Msg : String)
      is
         Err : constant TZif.Domain.Error.Error_Type :=
           (Kind    => Kind,
            Message =>
              TZif.Domain.Error.Error_Strings.To_Bounded_String (Msg));
      begin
         if not Discover.Error_Vectors.Is_Full (Data.Errors) then
            Discover.Error_Vectors.Unchecked_Append (Data.Errors, Err);
         end if;
      end Add_Error;

      --  Process a single path and return Source_Info or error
      --  Uses Functional.Try to catch any unexpected exceptions
      type Path_Result_Kind is (Source_Found, Error_Found, No_Source);
      type Path_Result (Kind : Path_Result_Kind := No_Source) is record
         case Kind is
            when Source_Found =>
               Source : Source_Info_Type;
            when Error_Found =>
               Error : TZif.Domain.Error.Error_Type;
            when No_Source =>
               null;
         end case;
      end record;

      function Process_Single_Path (Dir_Path : String) return Path_Result is
      begin
         --  Check if path exists
         if not Exists (Dir_Path) then
            return (Kind  => Error_Found,
                    Error => (Kind    => TZif.Domain.Error.Not_Found_Error,
                              Message => TZif.Domain.Error.Error_Strings
                                .To_Bounded_String
                                  ("Path not found: " & Dir_Path)));

         elsif Kind (Dir_Path) /= Directory then
            return (Kind  => Error_Found,
                    Error => (Kind    => TZif.Domain.Error.Validation_Error,
                              Message => TZif.Domain.Error.Error_Strings
                                .To_Bounded_String
                                  ("Not a directory: " & Dir_Path)));

         else
            --  Look for VERSION file
            declare
               Version_Path1 : constant String := Dir_Path & "/+VERSION";
               Version_Path2 : constant String := Dir_Path & "/VERSION";
               Version_Str   : Version_String_Type;
            begin
               if Exists (Version_Path1) then
                  Version_Str :=
                    Make_Version (Read_Version_File (Version_Path1));
               elsif Exists (Version_Path2) then
                  Version_Str :=
                    Make_Version (Read_Version_File (Version_Path2));
               else
                  Version_Str := Make_Version ("unknown");
               end if;

               --  Count TZif files
               declare
                  Zone_Count : constant Natural :=
                    Count_TZif_Files (Dir_Path);
               begin
                  if Zone_Count > 0 then
                     return (Kind   => Source_Found,
                             Source => Make_Source_Info
                               (ULID       => Generate_Simple_ULID,
                                Path       => Make_Path (Dir_Path),
                                Version    => Version_Str,
                                Zone_Count => Zone_Count));
                  else
                     return (Kind  => Error_Found,
                             Error => (Kind    =>
                                         TZif.Domain.Error.Validation_Error,
                                       Message =>
                                         TZif.Domain.Error.Error_Strings
                                           .To_Bounded_String
                                             ("No TZif files found in: " &
                                              Dir_Path)));
                  end if;
               end;
            end;
         end if;
      end Process_Single_Path;

      --  Result package for internal exception handling
      package Path_Result_Pkg is new Functional.Result
        (T => Path_Result, E => TZif.Domain.Error.Error_Type);

      --  Make error Result from kind and message
      function Make_Path_Error
        (Kind : TZif.Domain.Error.Error_Kind; Message : String)
         return Path_Result_Pkg.Result
      is
         pragma Unreferenced (Kind);
      begin
         return Path_Result_Pkg.New_Error
           ((Kind    => TZif.Domain.Error.IO_Error,
             Message => TZif.Domain.Error.Error_Strings.To_Bounded_String
               (Message)));
      end Make_Path_Error;

   begin
      --  Process each search path using index-based iteration
      for I in 1 .. Discover.Path_Vectors.Length (Search_Paths) loop
         declare
            Path_Elem : constant Discover.Path_String :=
              Discover.Path_Vectors.Unchecked_Element (Search_Paths, I);
            Dir_Path  : constant String :=
              Discover.Path_Strings.To_String (Path_Elem);

            --  Raw action wrapped in Result for Map_To_Result
            function Raw_Process return Path_Result_Pkg.Result is
            begin
               return Path_Result_Pkg.Ok (Process_Single_Path (Dir_Path));
            end Raw_Process;

            --  Declarative exception-to-Result mapping
            package Try_Process is new Functional.Try.Map_To_Result
              (Error_Kind_Type    => TZif.Domain.Error.Error_Kind,
               Result_Type        => Path_Result_Pkg.Result,
               Make_Error         => Make_Path_Error,
               Default_Error_Kind => TZif.Domain.Error.IO_Error,
               Action             => Raw_Process);

            Path_Res : Path_Result_Pkg.Result;
         begin
            Path_Res := Try_Process.Run_Catch_All;

            if Path_Result_Pkg.Is_Ok (Path_Res) then
               declare
                  Inner : constant Path_Result :=
                    Path_Result_Pkg.Value (Path_Res);
               begin
                  case Inner.Kind is
                     when Source_Found =>
                        if not Discover.Source_Info_Vectors.Is_Full
                            (Data.Sources)
                        then
                           Discover.Source_Info_Vectors.Unchecked_Append
                             (Data.Sources, Inner.Source);
                        end if;
                     when Error_Found =>
                        Add_Error (Inner.Error.Kind,
                          TZif.Domain.Error.Error_Strings.To_String
                            (Inner.Error.Message));
                     when No_Source =>
                        null;
                  end case;
               end;
            else
               --  Exception was caught and mapped to error
               declare
                  Err : constant TZif.Domain.Error.Error_Type :=
                    Path_Result_Pkg.Error (Path_Res);
               begin
                  Add_Error (Err.Kind,
                    TZif.Domain.Error.Error_Strings.To_String (Err.Message));
               end;
            end if;
         end;
      end loop;

      --  Return result
      Result := Discover.Discovery_Result_Package.Ok (Data);

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

   ----------------------------------------------------------------------
   --  Read_Version_File
   --
   --  Reads the +VERSION file from the timezone source directory.
   --
   --  Implementation:
   --    Uses Functional.Try for exception-to-Result conversion and
   --    Functional.Scoped for automatic file cleanup.
   ----------------------------------------------------------------------
   procedure Read_Version_File
     (Source : TZif.Domain.Value_Object.Source_Info.Source_Info_Type;
      Result : out TZif.Application.Port.Inbound.Get_Version.Version_Result)
   is
      use Ada.Directories;
      package Get_Version renames TZif.Application.Port.Inbound.Get_Version;

      Path_Str     : constant String :=
        TZif.Domain.Value_Object.Source_Info.To_String
          (TZif.Domain.Value_Object.Source_Info.Get_Path (Source));
      Version_File : constant String := Path_Str & "/+VERSION";

      --  Raw action that may raise - reads version string from file
      --  Returns Result type for Map_To_Result
      function Raw_Read_Version_File return Get_Version.Version_Result is
         File   : aliased SIO.File_Type;
         Guard  : Stream_File_Guard.Guard (File'Access);
         pragma Unreferenced (Guard);  --  RAII: releases via Finalize
         Stream : SIO.Stream_Access;
         Buffer : String (1 .. 32) := [others => ' '];
         Len    : Natural          := 0;
         Ch     : Character;
      begin
         SIO.Open (File, SIO.In_File, Version_File);
         Stream := SIO.Stream (File);

         --  Read until newline or buffer full
         while not SIO.End_Of_File (File) and then Len < Buffer'Last loop
            Character'Read (Stream, Ch);
            exit when Ch = ASCII.LF or else Ch = ASCII.CR;
            Len          := Len + 1;
            Buffer (Len) := Ch;
         end loop;

         --  Guard.Finalize will close file automatically
         return Get_Version.Version_Result_Package.Ok
           (Get_Version.Version_Strings.To_Bounded_String
              (Ada.Strings.Fixed.Trim (Buffer (1 .. Len), Ada.Strings.Both)));
      end Raw_Read_Version_File;

      --  Make_Error for version file reading
      function Make_Version_Error
        (Kind : TZif.Domain.Error.Error_Kind; Message : String)
         return Get_Version.Version_Result
      is
         use TZif.Domain.Error;
      begin
         case Kind is
            when Not_Found_Error =>
               return Get_Version.Version_Result_Package.Error
                 (Not_Found_Error, "Version file not found: " & Message);
            when others =>
               return Get_Version.Version_Result_Package.Error
                 (IO_Error, "Error reading version: " & Message);
         end case;
      end Make_Version_Error;

      --  Instantiate Functional.Try.Map_To_Result for version file reading
      package Try_Read_Version is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => TZif.Domain.Error.Error_Kind,
         Result_Type        => Get_Version.Version_Result,
         Make_Error         => Make_Version_Error,
         Default_Error_Kind => TZif.Domain.Error.IO_Error,
         Action             => Raw_Read_Version_File);

      Version_Mappings : constant Try_Read_Version.Mapping_Array :=
        [(SIO.Name_Error'Identity, TZif.Domain.Error.Not_Found_Error),
         (SIO.Use_Error'Identity,  TZif.Domain.Error.IO_Error)];

   begin
      --  Check if file exists first (avoids exception for common case)
      if not Exists (Version_File) or else Kind (Version_File) /= Ordinary_File
      then
         Result :=
           Get_Version.Version_Result_Package.Error
             (TZif.Domain.Error.Not_Found_Error,
              "Version file not found: " & Version_File);
         return;
      end if;

      Result := Try_Read_Version.Run (Version_Mappings);
   end Read_Version_File;

   ----------------------------------------------------------------------
   --  Read_System_Timezone_Id
   --
   --  Reads the system's timezone ID from /etc/localtime symlink.
   --
   --  Implementation:
   --    Uses Functional.Try.Map_To_Result for declarative exception mapping.
   --    Platform.POSIX.Operations.Read_Link already returns Result type.
   ----------------------------------------------------------------------
   procedure Read_System_Timezone_Id
     (Result : out TZif.Application.Port.Inbound.Find_My_Id.Result)
   is
      use Ada.Directories;
      package Find_My_Id renames TZif.Application.Port.Inbound.Find_My_Id;

      Localtime_Path : constant String := "/etc/localtime";

      --  Raw action that may raise - extracts zone ID from symlink target
      --  Returns Result type for Map_To_Result
      function Raw_Extract_Zone_Id return Find_My_Id.Result is
         use TZif.Infrastructure.Platform.POSIX;
         Link_Result :
           constant TZif.Infrastructure.Platform.Platform_String_Result :=
           Operations.Read_Link (Localtime_Path);
      begin
         if not TZif.Infrastructure.Platform.String_Result.Is_Ok (Link_Result)
         then
            raise Constraint_Error with "/etc/localtime is not a symlink";
         end if;

         declare
            Link_Target_Bounded :
              constant TZif.Infrastructure.Platform.Platform_String :=
              TZif.Infrastructure.Platform.String_Result.Value (Link_Result);
            Link_Target         : constant String                   :=
              TZif.Infrastructure.Platform.Platform_Strings.To_String
                (Link_Target_Bounded);
            Marker              : constant String                   :=
              "zoneinfo/";
            Marker_Pos          : Natural                           := 0;
         begin
            --  Find "zoneinfo/" marker
            for I in Link_Target'First .. Link_Target'Last - Marker'Length + 1
            loop
               if Link_Target (I .. I + Marker'Length - 1) = Marker then
                  Marker_Pos := I;
                  exit;
               end if;
            end loop;

            if Marker_Pos = 0 then
               raise Constraint_Error
                 with "Cannot extract zone ID from: " & Link_Target;
            end if;

            declare
               Zone_Id_Start : constant Positive := Marker_Pos + Marker'Length;
               Zone_Id_Str   : constant String   :=
                 Link_Target (Zone_Id_Start .. Link_Target'Last);
            begin
               return Find_My_Id.Result_Zone_Id.Ok
                 (Make_Zone_Id (Zone_Id_Str));
            end;
         end;
      end Raw_Extract_Zone_Id;

      --  Make_Error for system zone ID extraction
      function Make_Syszone_Error
        (Kind : TZif.Domain.Error.Error_Kind; Message : String)
         return Find_My_Id.Result
      is
         use TZif.Domain.Error;
      begin
         case Kind is
            when Not_Found_Error =>
               return Find_My_Id.Result_Zone_Id.Error
                 (Not_Found_Error, "System timezone not found: " & Message);
            when Validation_Error =>
               return Find_My_Id.Result_Zone_Id.Error
                 (Validation_Error, "Invalid symlink target: " & Message);
            when others =>
               return Find_My_Id.Result_Zone_Id.Error
                 (IO_Error, "Error reading system timezone: " & Message);
         end case;
      end Make_Syszone_Error;

      --  Instantiate Functional.Try.Map_To_Result for zone ID extraction
      package Try_Extract_Zone_Id is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => TZif.Domain.Error.Error_Kind,
         Result_Type        => Find_My_Id.Result,
         Make_Error         => Make_Syszone_Error,
         Default_Error_Kind => TZif.Domain.Error.IO_Error,
         Action             => Raw_Extract_Zone_Id);

      Syszone_Mappings : constant Try_Extract_Zone_Id.Mapping_Array :=
        [(Ada.Directories.Name_Error'Identity,
          TZif.Domain.Error.Not_Found_Error),
         (Constraint_Error'Identity, TZif.Domain.Error.Validation_Error)];

   begin
      if not Exists (Localtime_Path) then
         Result :=
           Find_My_Id.Result_Zone_Id.Error
             (TZif.Domain.Error.Not_Found_Error, "/etc/localtime not found");
         return;
      end if;

      Result := Try_Extract_Zone_Id.Run (Syszone_Mappings);
   end Read_System_Timezone_Id;

   ----------------------------------------------------------------------
   --  List_Zones_In_Source
   --
   --  Lists all timezone IDs in a source directory, sorted.
   --
   --  Implementation:
   --    Uses Functional.Try.Map_To_Result_With_Param for declarative mapping.
   --    Inner exception handlers for intentional error recovery are preserved.
   ----------------------------------------------------------------------
   procedure List_Zones_In_Source
     (Source     : TZif.Domain.Value_Object.Source_Info.Source_Info_Type;
      Descending : Boolean;
      Result     : out TZif.Application.Port.Inbound.List_All_Order_By_Id
        .List_All_Zones_Result)
   is
      use Ada.Directories;
      package List_All renames
        TZif.Application.Port.Inbound.List_All_Order_By_Id;

      Path_Str : constant String :=
        TZif.Domain.Value_Object.Source_Info.To_String
          (TZif.Domain.Value_Object.Source_Info.Get_Path (Source));

      --  Context for raw action
      type Scan_Context is record
         Descending : Boolean;
      end record;

      --  Raw action that may raise - returns Result type for Map_To_Result
      function Raw_Scan_And_Sort
        (Ctx : Scan_Context) return List_All.List_All_Zones_Result
      is
         Zones : List_All.Zone_Id_List;

         procedure Scan_Directory (Dir_Path : String; Prefix : String := "") is
            Search : Search_Type;
            pragma Warnings (Off, Search);
            Item : Directory_Entry_Type;
         begin
            Start_Search (Search, Dir_Path, "*");

            while More_Entries (Search) loop
               Get_Next_Entry (Search, Item);

               declare
                  Name : constant String := Simple_Name (Item);
               begin
                  if Name'Length > 0 and then Name (Name'First) /= '.' then
                     declare
                        Full_Path : constant String := Full_Name (Item);
                        Zone_Name : constant String :=
                          (if Prefix = "" then Name else Prefix & "/" & Name);
                     begin
                        case Kind (Item) is
                           when Directory =>
                              Scan_Directory (Full_Path, Zone_Name);

                           when Ordinary_File =>
                              if Name /= "zone.tab"
                                and then Name /= "zone1970.tab"
                                and then Name /= "iso3166.tab"
                                and then Name /= "leapseconds"
                                and then Name /= "tzdata.zi"
                                and then Name /= "+VERSION"
                              then
                                 begin
                                    if not List_All.Zone_Id_Vectors.Is_Full
                                      (Zones)
                                    then
                                       List_All.Zone_Id_Vectors
                                         .Unchecked_Append
                                           (Zones, Make_Zone_Id (Zone_Name));
                                    end if;
                                 exception
                                    when Constraint_Error =>
                                       --  DESIGN DECISION: Skip zone
                                       --  names exceeding bounded string
                                       --  limit during batch scan.
                                       --  Functional.Try not applicable
                                       --  here as we need continue-on-
                                       --  error semantics.
                                       null;
                                 end;
                              end if;

                           when others =>
                              --  Skip non-directory/non-file entries
                              null;
                        end case;
                     end;
                  end if;
               end;
            end loop;

            End_Search (Search);
         exception
            when Ada.Directories.Name_Error | Ada.Directories.Use_Error =>
               --  Skip inaccessible directories silently (intentional)
               null;
         end Scan_Directory;

         function Less_Than (Left, Right : Zone_Id_Type) return Boolean is
           (To_String (Left) < To_String (Right));

         procedure Sort_Zones is new List_All.Zone_Id_Vectors.Generic_Sort
           ("<" => Less_Than);

      begin
         Scan_Directory (Path_Str);
         Sort_Zones (Zones);
         if Ctx.Descending then
            List_All.Zone_Id_Vectors.Reverse_Elements (Zones);
         end if;
         return List_All.List_All_Zones_Result_Package.Ok (Zones);
      end Raw_Scan_And_Sort;

      --  Make_Error for list zones operation
      function Make_Listzones_Error
        (Kind : TZif.Domain.Error.Error_Kind; Message : String)
         return List_All.List_All_Zones_Result
      is
         use TZif.Domain.Error;
      begin
         case Kind is
            when Not_Found_Error =>
               return List_All.List_All_Zones_Result_Package.Error
                 (Not_Found_Error, "Source not found: " & Message);
            when others =>
               return List_All.List_All_Zones_Result_Package.Error
                 (IO_Error, "Error listing zones: " & Message);
         end case;
      end Make_Listzones_Error;

      --  Instantiate Functional.Try.Map_To_Result_With_Param
      package Try_Scan_And_Sort is new
        Functional.Try.Map_To_Result_With_Param
          (Error_Kind_Type    => TZif.Domain.Error.Error_Kind,
           Param_Type         => Scan_Context,
           Result_Type        => List_All.List_All_Zones_Result,
           Make_Error         => Make_Listzones_Error,
           Default_Error_Kind => TZif.Domain.Error.IO_Error,
           Action             => Raw_Scan_And_Sort);

      Listzones_Mappings : constant Try_Scan_And_Sort.Mapping_Array :=
        [(Ada.Directories.Name_Error'Identity,
          TZif.Domain.Error.Not_Found_Error),
         (Ada.Directories.Use_Error'Identity, TZif.Domain.Error.IO_Error)];

   begin
      if not Exists (Path_Str) or else Kind (Path_Str) /= Directory then
         Result :=
           List_All.List_All_Zones_Result_Package.Error
             (TZif.Domain.Error.Not_Found_Error,
              "Source path not found: " & Path_Str);
         return;
      end if;

      Result := Try_Scan_And_Sort.Run ((Descending => Descending),
                                       Listzones_Mappings);
   end List_Zones_In_Source;

   ----------------------------------------------------------------------
   --  Load_Source_From_Path
   --
   --  Loads timezone source metadata from filesystem path.
   --
   --  Implementation:
   --    Uses Functional.Try.Map_To_Result for declarative exception mapping
   --    and Functional.Scoped for automatic file cleanup.
   ----------------------------------------------------------------------
   procedure Load_Source_From_Path
     (Path   : TZif.Application.Port.Inbound.Load_Source.Path_String;
      Result : out TZif.Application.Port.Inbound.Load_Source
        .Load_Source_Result)
   is
      use Ada.Directories;
      use Ada.Text_IO;
      package Load_Source renames TZif.Application.Port.Inbound.Load_Source;

      --  Re-enable Text_File_Guard for this procedure
      package Local_Text_Guard is new Functional.Scoped.Conditional_Guard_For
        (Resource       => File_Type,
         Should_Release => Is_Open,
         Release        => Close);

      Path_Str : constant String :=
        Load_Source.Path_Strings.To_String (Path);

      --  Raw action that may raise - returns Result type for Map_To_Result
      function Raw_Load_Source return Load_Source.Load_Source_Result is
         ULID         : constant ULID_Type        :=
           TZif.Infrastructure.ULID.Generate;
         Path_Val     : constant Path_String_Type := Make_Path (Path_Str);
         Version_File : constant String           := Path_Str & "/+VERSION";
         Version_Str  : String (1 .. 32);
         Last         : Natural;
         File         : aliased File_Type;
         Guard        : Local_Text_Guard.Guard (File'Access);
         pragma Unreferenced (Guard);  --  RAII: releases via Finalize

         Zone_Count   : Natural := 0;

         procedure Count_Zones (Dir_Path : String) is

            procedure Count_Recursive (P : String) is
               S : Search_Type;
               pragma Warnings (Off, S);
               I : Directory_Entry_Type;
            begin
               if Zone_Count > 1_000 then
                  return;
               end if;

               Start_Search (S, P, "*");
               while More_Entries (S) loop
                  Get_Next_Entry (S, I);
                  declare
                     N : constant String := Simple_Name (I);
                  begin
                     if N'Length > 0 and then N (N'First) /= '.' then
                        case Kind (I) is
                           when Directory =>
                              Count_Recursive (Full_Name (I));

                           when Ordinary_File =>
                              if N /= "zone.tab"
                                and then N /= "zone1970.tab"
                                and then N /= "iso3166.tab"
                                and then N /= "leapseconds"
                                and then N /= "tzdata.zi"
                                and then N /= "+VERSION"
                              then
                                 Zone_Count := Zone_Count + 1;
                              end if;

                           when others =>
                              null;
                        end case;
                     end if;
                  end;
               end loop;
               End_Search (S);
            exception
               when Ada.Directories.Name_Error
                  | Ada.Directories.Use_Error =>
                  --  Skip inaccessible directories (intentional)
                  null;
            end Count_Recursive;

         begin
            Count_Recursive (Dir_Path);
         end Count_Zones;

         Version : Version_String_Type;
      begin
         if Exists (Version_File) and then Kind (Version_File) = Ordinary_File
         then
            Open (File, In_File, Version_File);
            Get_Line (File, Version_Str, Last);
            --  Guard.Finalize will close file automatically
         else
            Version_Str (1 .. 7) := "unknown";
            Last                 := 7;
         end if;

         Version := Make_Version (Version_Str (1 .. Last));
         Count_Zones (Path_Str);
         return Load_Source.Load_Source_Result_Package.Ok
           (Make_Source_Info (ULID, Path_Val, Version, Zone_Count));
      end Raw_Load_Source;

      --  Make_Error for load source operation
      function Make_Loadsource_Error
        (Kind : TZif.Domain.Error.Error_Kind; Message : String)
         return Load_Source.Load_Source_Result
      is
         use TZif.Domain.Error;
      begin
         case Kind is
            when Not_Found_Error =>
               return Load_Source.Load_Source_Result_Package.Error
                 (Not_Found_Error, "Source path not found: " & Message);
            when others =>
               return Load_Source.Load_Source_Result_Package.Error
                 (IO_Error, "Error loading source: " & Message);
         end case;
      end Make_Loadsource_Error;

      --  Instantiate Functional.Try.Map_To_Result
      package Try_Load_Source is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => TZif.Domain.Error.Error_Kind,
         Result_Type        => Load_Source.Load_Source_Result,
         Make_Error         => Make_Loadsource_Error,
         Default_Error_Kind => TZif.Domain.Error.IO_Error,
         Action             => Raw_Load_Source);

      Loadsource_Mappings : constant Try_Load_Source.Mapping_Array :=
        [(Ada.Directories.Name_Error'Identity,
          TZif.Domain.Error.Not_Found_Error),
         (Ada.Directories.Use_Error'Identity, TZif.Domain.Error.IO_Error)];

   begin
      if not Exists (Path_Str) then
         Result :=
           Load_Source.Load_Source_Result_Package.Error
             (TZif.Domain.Error.Not_Found_Error,
              "Path not found: " & Path_Str);
         return;
      end if;

      if Kind (Path_Str) /= Directory then
         Result :=
           Load_Source.Load_Source_Result_Package.Error
             (TZif.Domain.Error.Validation_Error,
              "Path is not a directory: " & Path_Str);
         return;
      end if;

      Result := Try_Load_Source.Run (Loadsource_Mappings);
   end Load_Source_From_Path;

   ----------------------------------------------------------------------
   --  Validate_Source_Path
   --
   --  Validates that a path is a valid timezone source.
   --
   --  Implementation:
   --    Uses Functional.Try.Map_To_Result for declarative exception mapping.
   ----------------------------------------------------------------------
   procedure Validate_Source_Path
     (Path   : TZif.Application.Port.Inbound.Validate_Source.Path_String;
      Result : out TZif.Application.Port.Inbound.Validate_Source
        .Validation_Result)
   is
      use Ada.Directories;
      package Validate_Source renames
        TZif.Application.Port.Inbound.Validate_Source;

      Path_Str : constant String :=
        Validate_Source.Path_Strings.To_String (Path);

      --  Raw action that may raise - returns Result type for Map_To_Result
      function Raw_Validate return Validate_Source.Validation_Result is
         Search     : Search_Type;
         pragma Warnings (Off, Search);
         Item       : Directory_Entry_Type;
         Found_TZif : Boolean := False;
      begin
         Start_Search (Search, Path_Str, "*");
         while More_Entries (Search) and then not Found_TZif loop
            Get_Next_Entry (Search, Item);
            if Kind (Item) = Ordinary_File then
               Found_TZif := True;
            end if;
         end loop;
         End_Search (Search);
         return Validate_Source.Validation_Result_Package.Ok (Found_TZif);
      exception
         when Name_Error | Use_Error =>
            --  Inaccessible directory = not valid (intentional)
            return Validate_Source.Validation_Result_Package.Ok (False);
      end Raw_Validate;

      --  Make_Error for validation operation
      function Make_Validate_Error
        (Kind : TZif.Domain.Error.Error_Kind; Message : String)
         return Validate_Source.Validation_Result
      is
         pragma Unreferenced (Kind, Message);
      begin
         --  Validation errors should return Ok(False), not Err
         return Validate_Source.Validation_Result_Package.Ok (False);
      end Make_Validate_Error;

      --  Instantiate Functional.Try.Map_To_Result
      package Try_Validate is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => TZif.Domain.Error.Error_Kind,
         Result_Type        => Validate_Source.Validation_Result,
         Make_Error         => Make_Validate_Error,
         Default_Error_Kind => TZif.Domain.Error.IO_Error,
         Action             => Raw_Validate);

      --  Empty mappings since we handle errors internally by returning False
      Validate_Mappings : constant Try_Validate.Mapping_Array :=
        Try_Validate.Empty_Mappings;

   begin
      if not Exists (Path_Str) then
         Result := Validate_Source.Validation_Result_Package.Ok (False);
         return;
      end if;

      if Kind (Path_Str) /= Directory then
         Result := Validate_Source.Validation_Result_Package.Ok (False);
         return;
      end if;

      Result := Try_Validate.Run (Validate_Mappings);
   end Validate_Source_Path;

   ----------------------------------------------------------------------
   --  Find_Zones_By_Pattern
   --
   --  Finds timezone IDs matching a substring pattern.
   --
   --  Implementation:
   --    Uses Functional.Try.Map_To_Result for declarative exception mapping.
   --    Inner exception handler for inaccessible directories preserved.
   ----------------------------------------------------------------------
   procedure Find_Zones_By_Pattern
     (Pattern : TZif.Application.Port.Inbound.Find_By_Pattern.Pattern_String;
      Yield   : TZif.Application.Port.Inbound.Find_By_Pattern
        .Yield_Callback_Access;
      Result  : out TZif.Application.Port.Inbound.Find_By_Pattern
        .Find_By_Pattern_Result)
   is
      use Ada.Directories;
      package Find_By_Pattern renames
        TZif.Application.Port.Inbound.Find_By_Pattern;

      Pattern_Str : constant String :=
        Find_By_Pattern.Pattern_Strings.To_String (Pattern);

      --  Raw action that may raise - returns Result type for Map_To_Result
      function Raw_Scan return Find_By_Pattern.Find_By_Pattern_Result is

         procedure Scan_Directory (Dir_Path : String; Prefix : String := "") is
            Search : Search_Type;
            pragma Warnings (Off, Search);
            Item : Directory_Entry_Type;
         begin
            Start_Search (Search, Dir_Path, "*");

            while More_Entries (Search) loop
               Get_Next_Entry (Search, Item);

               declare
                  Name : constant String := Simple_Name (Item);
               begin
                  if Name'Length > 0 and then Name (Name'First) /= '.' then
                     declare
                        Full_Path : constant String := Full_Name (Item);
                        Zone_Name : constant String :=
                          (if Prefix = "" then Name else Prefix & "/" & Name);
                     begin
                        case Kind (Item) is
                           when Directory =>
                              Scan_Directory (Full_Path, Zone_Name);

                           when Ordinary_File =>
                              declare
                                 Lower_Zone : constant String :=
                                   Ada.Characters.Handling.To_Lower
                                     (Zone_Name);
                                 Lower_Pattern : constant String :=
                                   Ada.Characters.Handling.To_Lower
                                     (Pattern_Str);
                              begin
                                 if Ada.Strings.Fixed.Index
                                     (Lower_Zone, Lower_Pattern) >
                                   0
                                 then
                                    Yield
                                      (Find_By_Pattern.Zone_Name_Strings
                                         .To_Bounded_String
                                         (Zone_Name));
                                 end if;
                              end;

                           when others =>
                              null;
                        end case;
                     end;
                  end if;
               end;
            end loop;

            End_Search (Search);
         exception
            when Ada.Directories.Name_Error | Ada.Directories.Use_Error =>
               --  Skip inaccessible directories (intentional)
               null;
         end Scan_Directory;

      begin
         if Exists (Zoneinfo_Base)
           and then Kind (Zoneinfo_Base) = Directory
         then
            Scan_Directory (Zoneinfo_Base);
         end if;
         return Find_By_Pattern.Find_By_Pattern_Result_Package.Ok
           (TZif.Domain.Value_Object.Unit.Unit);
      end Raw_Scan;

      --  Make_Error for pattern search operation
      function Make_Pattern_Error
        (Kind : TZif.Domain.Error.Error_Kind; Message : String)
         return Find_By_Pattern.Find_By_Pattern_Result
      is
         use TZif.Domain.Error;
      begin
         case Kind is
            when Not_Found_Error =>
               return Find_By_Pattern.Find_By_Pattern_Result_Package.Error
                 (Not_Found_Error, "Source not found: " & Message);
            when others =>
               return Find_By_Pattern.Find_By_Pattern_Result_Package.Error
                 (IO_Error, "Error searching pattern: " & Message);
         end case;
      end Make_Pattern_Error;

      --  Instantiate Functional.Try.Map_To_Result
      package Try_Scan is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => TZif.Domain.Error.Error_Kind,
         Result_Type        => Find_By_Pattern.Find_By_Pattern_Result,
         Make_Error         => Make_Pattern_Error,
         Default_Error_Kind => TZif.Domain.Error.IO_Error,
         Action             => Raw_Scan);

      Pattern_Mappings : constant Try_Scan.Mapping_Array :=
        [(Ada.Directories.Name_Error'Identity,
          TZif.Domain.Error.Not_Found_Error),
         (Ada.Directories.Use_Error'Identity, TZif.Domain.Error.IO_Error)];

   begin
      Result := Try_Scan.Run (Pattern_Mappings);
   end Find_Zones_By_Pattern;

   ----------------------------------------------------------------------
   --  Find_Zones_By_Region
   --
   --  Finds timezone IDs by region prefix.
   --
   --  Implementation:
   --    Uses Functional.Try.Map_To_Result for declarative exception mapping.
   --    Inner exception handler for inaccessible directories preserved.
   ----------------------------------------------------------------------
   procedure Find_Zones_By_Region
     (Region : TZif.Application.Port.Inbound.Find_By_Region.Region_String;
      Yield  : TZif.Application.Port.Inbound.Find_By_Region
        .Yield_Callback_Access;
      Result : out TZif.Application.Port.Inbound.Find_By_Region
        .Find_By_Region_Result)
   is
      use Ada.Directories;
      package Find_By_Region renames
        TZif.Application.Port.Inbound.Find_By_Region;

      Region_Str : constant String :=
        Find_By_Region.Region_Strings.To_String (Region);

      --  Raw action that may raise - returns Result type for Map_To_Result
      function Raw_Scan return Find_By_Region.Find_By_Region_Result is

         procedure Scan_Directory (Dir_Path : String; Prefix : String := "") is
            Search : Search_Type;
            pragma Warnings (Off, Search);
            Item : Directory_Entry_Type;
         begin
            Start_Search (Search, Dir_Path, "*");

            while More_Entries (Search) loop
               Get_Next_Entry (Search, Item);

               declare
                  Name : constant String := Simple_Name (Item);
               begin
                  if Name'Length > 0 and then Name (Name'First) /= '.' then
                     declare
                        Full_Path : constant String := Full_Name (Item);
                        Zone_Name : constant String :=
                          (if Prefix = "" then Name else Prefix & "/" & Name);
                     begin
                        case Kind (Item) is
                           when Directory =>
                              Scan_Directory (Full_Path, Zone_Name);

                           when Ordinary_File =>
                              if Zone_Name'Length >= Region_Str'Length
                                and then
                                  Zone_Name
                                    (Zone_Name'First ..
                                         Zone_Name'First + Region_Str'Length -
                                         1) =
                                  Region_Str
                              then
                                 Yield
                                   (Find_By_Region.Zone_Name_Strings
                                      .To_Bounded_String
                                      (Zone_Name));
                              end if;

                           when others =>
                              null;
                        end case;
                     end;
                  end if;
               end;
            end loop;

            End_Search (Search);
         exception
            when Ada.Directories.Name_Error | Ada.Directories.Use_Error =>
               --  Skip inaccessible directories (intentional)
               null;
         end Scan_Directory;

      begin
         if Exists (Zoneinfo_Base)
           and then Kind (Zoneinfo_Base) = Directory
         then
            Scan_Directory (Zoneinfo_Base);
         end if;
         return Find_By_Region.Find_By_Region_Result_Package.Ok
           (TZif.Domain.Value_Object.Unit.Unit);
      end Raw_Scan;

      --  Make_Error for region search operation
      function Make_Region_Error
        (Kind : TZif.Domain.Error.Error_Kind; Message : String)
         return Find_By_Region.Find_By_Region_Result
      is
         use TZif.Domain.Error;
      begin
         case Kind is
            when Not_Found_Error =>
               return Find_By_Region.Find_By_Region_Result_Package.Error
                 (Not_Found_Error, "Source not found: " & Message);
            when others =>
               return Find_By_Region.Find_By_Region_Result_Package.Error
                 (IO_Error, "Error searching region: " & Message);
         end case;
      end Make_Region_Error;

      --  Instantiate Functional.Try.Map_To_Result
      package Try_Scan is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => TZif.Domain.Error.Error_Kind,
         Result_Type        => Find_By_Region.Find_By_Region_Result,
         Make_Error         => Make_Region_Error,
         Default_Error_Kind => TZif.Domain.Error.IO_Error,
         Action             => Raw_Scan);

      Region_Mappings : constant Try_Scan.Mapping_Array :=
        [(Ada.Directories.Name_Error'Identity,
          TZif.Domain.Error.Not_Found_Error),
         (Ada.Directories.Use_Error'Identity, TZif.Domain.Error.IO_Error)];

   begin
      Result := Try_Scan.Run (Region_Mappings);
   end Find_Zones_By_Region;

   ----------------------------------------------------------------------
   --  Find_Zones_By_Regex
   --
   --  Finds timezone IDs matching a regular expression.
   --
   --  Implementation:
   --    Uses Functional.Try.Map_To_Result for declarative exception mapping.
   --    Maps GNAT.Regpat.Expression_Error to Validation_Error for invalid
   --    regex patterns.
   ----------------------------------------------------------------------
   procedure Find_Zones_By_Regex
     (Regex  : TZif.Application.Port.Inbound.Find_By_Regex.Regex_String;
      Yield  : TZif.Application.Port.Inbound.Find_By_Regex
        .Yield_Callback_Access;
      Result : out TZif.Application.Port.Inbound.Find_By_Regex
        .Find_By_Regex_Result)
   is
      use Ada.Directories;
      use GNAT.Regpat;
      package Find_By_Regex renames
        TZif.Application.Port.Inbound.Find_By_Regex;

      Regex_Str : constant String :=
        Find_By_Regex.Regex_Strings.To_String (Regex);

      procedure Scan_Directory
        (Dir_Path : String; Prefix : String := ""; Pattern : Pattern_Matcher)
      is
         Search : Search_Type;
         pragma Warnings (Off, Search);
         Item : Directory_Entry_Type;
      begin
         Start_Search (Search, Dir_Path, "*");

         while More_Entries (Search) loop
            Get_Next_Entry (Search, Item);

            declare
               Name : constant String := Simple_Name (Item);
            begin
               if Name'Length > 0 and then Name (Name'First) /= '.' then
                  declare
                     Full_Path : constant String := Full_Name (Item);
                     Zone_Name : constant String :=
                       (if Prefix = "" then Name else Prefix & "/" & Name);
                  begin
                     case Kind (Item) is
                        when Directory =>
                           Scan_Directory (Full_Path, Zone_Name, Pattern);

                        when Ordinary_File =>
                           if Match (Pattern, Zone_Name) then
                              Yield
                                (Find_By_Regex.Zone_Name_Strings
                                   .To_Bounded_String
                                   (Zone_Name));
                           end if;

                        when others =>
                           null;
                     end case;
                  end;
               end if;
            end;
         end loop;

         End_Search (Search);
      exception
         when Ada.Directories.Name_Error | Ada.Directories.Use_Error =>
            --  Skip inaccessible directories (intentional)
            null;
      end Scan_Directory;

      --  Raw function that may raise - returns Result type for Map_To_Result
      function Raw_Scan return Find_By_Regex.Find_By_Regex_Result is
         Pattern : constant Pattern_Matcher := Compile (Regex_Str);
      begin
         if Exists (Zoneinfo_Base) and then Kind (Zoneinfo_Base) = Directory
         then
            Scan_Directory (Zoneinfo_Base, "", Pattern);
         end if;
         return Find_By_Regex.Find_By_Regex_Result_Package.Ok
           (TZif.Domain.Value_Object.Unit.Unit);
      end Raw_Scan;

      --  Make_Error for regex search operation
      function Make_Regex_Error
        (Kind : TZif.Domain.Error.Error_Kind; Message : String)
         return Find_By_Regex.Find_By_Regex_Result
      is
         use TZif.Domain.Error;
      begin
         case Kind is
            when Validation_Error =>
               return Find_By_Regex.Find_By_Regex_Result_Package.Error
                 (Validation_Error, "Invalid regex pattern: " & Regex_Str);
            when Not_Found_Error =>
               return Find_By_Regex.Find_By_Regex_Result_Package.Error
                 (Not_Found_Error, "Directory not found: " & Message);
            when others =>
               return Find_By_Regex.Find_By_Regex_Result_Package.Error
                 (IO_Error, "Error searching regex: " & Message);
         end case;
      end Make_Regex_Error;

      --  Instantiate Functional.Try.Map_To_Result with regex-aware mapping
      package Try_Scan is new Functional.Try.Map_To_Result
        (Error_Kind_Type    => TZif.Domain.Error.Error_Kind,
         Result_Type        => Find_By_Regex.Find_By_Regex_Result,
         Make_Error         => Make_Regex_Error,
         Default_Error_Kind => TZif.Domain.Error.IO_Error,
         Action             => Raw_Scan);

      Regex_Mappings : constant Try_Scan.Mapping_Array :=
        [(Expression_Error'Identity, TZif.Domain.Error.Validation_Error),
         (Ada.Directories.Name_Error'Identity,
          TZif.Domain.Error.Not_Found_Error),
         (Ada.Directories.Use_Error'Identity, TZif.Domain.Error.IO_Error)];

   begin
      Result := Try_Scan.Run (Regex_Mappings);
   end Find_Zones_By_Regex;

end TZif.Infrastructure.IO.Desktop;
