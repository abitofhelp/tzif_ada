pragma Ada_2022;
--  ===========================================================================
--  Tzif.Application.Operations
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Operations implementation.
--
--  ===========================================================================

with TZif.Domain.Parser;
with TZif.Domain.TZif_Data;
with TZif.Domain.Entity.Zone;
with TZif.Domain.Value_Object.Zone_Id;
with TZif.Domain.Value_Object.Timezone_Type;
with TZif.Domain.Value_Object.Transition_Info;
with TZif.Domain.Error;

package body TZif.Application.Operations with
  SPARK_Mode => On
is

   package body All_Operations is

      use TZif.Domain.Error.Error_Strings;

      --  Get_Modified_Time is a dropped feature (see roadmap.md)
      pragma Unreferenced (Get_Modified_Time);

      ----------------------------------------------------------------------
      --  Find_By_Id
      --
      --  Implementation:
      --    1. Call Read_File (I/O plugin) to get TZif bytes
      --    2. Call Domain.Parser.Parse_From_Bytes (SPARK core)
      --    3. Map parse result to Find_By_Id_Result_Type
      --    4. Handle errors at each step using qualified names
      ----------------------------------------------------------------------
      procedure Find_By_Id
        (Id : Zone_Id_Input_Type; Result : out Find_By_Id_Result_Type)
      is
         --  Local buffer for TZif bytes
         --  ROADMAP: Make buffer size configurable (see roadmap.md)
         Buffer : Byte_Array (1 .. 65_536);
         Length : Natural := 0;

         --  I/O Result using the formal package
         IO_Res : Read_File_Result.Result;

         --  Parse result from domain parser
         package Parse_Result renames TZif.Domain.Parser.Parse_Result;
         Parse_Res : Parse_Result.Result;
      begin
         --  Step 1: Read raw TZif bytes via injected I/O plugin
         Read_File (Id, Buffer, Length, IO_Res);

         --  Step 2: Check I/O result using qualified name
         if not Read_File_Result.Is_Ok (IO_Res) then
            declare
               Err : constant TZif.Domain.Error.Error_Type :=
                 Read_File_Result.Error_Info (IO_Res);
            begin
               Result :=
                 Find_By_Id_Result.Error (Err.Kind, To_String (Err.Message));
            end;
            return;
         end if;

         --  Step 3: Verify we read bytes
         if Length = 0 then
            Result :=
              Find_By_Id_Result.Error
                (TZif.Domain.Error.IO_Error,
                 "No data read from zone file");
            return;
         end if;

         --  Step 4: Parse TZif bytes into domain data
         --  Convert generic Byte_Array to Parser.Byte_Array
         declare
            Parser_Buffer : TZif.Domain.Parser.Byte_Array (1 .. Length);
         begin
            for I in 1 .. Length loop
               Parser_Buffer (I) := Buffer (I);
            end loop;

            TZif.Domain.Parser.Parse_From_Bytes
              (Bytes => Parser_Buffer, Length => Length, Result => Parse_Res);
         end;

         --  Step 5: Check parse result using qualified name
         if not Parse_Result.Is_Ok (Parse_Res) then
            declare
               Err : constant TZif.Domain.Error.Error_Type :=
                 Parse_Result.Error_Info (Parse_Res);
            begin
               Result :=
                 Find_By_Id_Result.Error (Err.Kind, To_String (Err.Message));
            end;
            return;
         end if;

         --  Step 6: Success - construct Zone from parsed data
         declare
            use TZif.Domain.Entity.Zone;
            use TZif.Domain.Value_Object.Zone_Id;
            Parsed    : constant TZif.Domain.TZif_Data.TZif_Data_Type :=
              Parse_Result.Value (Parse_Res);
            Id_String : constant String := To_String (Id);
            Zone_Obj  : constant Zone_Type := Make_Zone (Id_String, Parsed);
         begin
            Result := Find_By_Id_Result.Ok (Zone_Obj);
         end;

      end Find_By_Id;

      ----------------------------------------------------------------------
      --  Discover_Sources
      ----------------------------------------------------------------------
      procedure Discover_Sources
        (Search_Paths :     Discover_Path_List_Type;
         Result       : out Discovery_Result_Type)
      is
      begin
         --  Call I/O plugin directly - it returns Discovery_Result.Result
         --  which matches our output type (Discovery_Result_Type)
         List_Directory_Sources (Search_Paths, Result);

      end Discover_Sources;

      ----------------------------------------------------------------------
      --  Get_Transition_At_Epoch
      --
      --  Implementation:
      --    1. Convert bounded Zone_Id_String to Zone_Id_Type for Read_File
      --    2. Call Read_File (I/O plugin) to get TZif bytes
      --    3. Call Domain.Parser.Parse_From_Bytes (SPARK core)
      --    4. Find transition type at specified epoch
      --    5. Construct Transition_Info and return
      ----------------------------------------------------------------------
      procedure Get_Transition_At_Epoch
        (Id     :     Transition_Zone_Id_String;
         Epoch  :     Epoch_Seconds_Type;
         Result : out Get_Transition_Result_Type)
      is
         use TZif.Domain.Value_Object.Zone_Id;
         use TZif.Domain.Value_Object.Timezone_Type;
         use TZif.Domain.Value_Object.Transition_Info;

         --  Convert bounded string to Zone_Id_Type for Read_File
         Zone_Id_Str : constant String :=
           Inbound_Get_Transition.Zone_Id_Strings.To_String (Id);
         Zone_Id_Val : constant Zone_Id_Input_Type := Make_Zone_Id (Zone_Id_Str);

         --  Local buffer for TZif bytes (same as Find_By_Id)
         Buffer : Byte_Array (1 .. 65_536);
         Length : Natural := 0;

         --  I/O Result using the formal package
         IO_Res : Read_File_Result.Result;

         --  Parse result from domain parser
         package Parse_Result renames TZif.Domain.Parser.Parse_Result;
         Parse_Res : Parse_Result.Result;
      begin
         --  Step 1: Read raw TZif bytes via injected I/O plugin
         Read_File (Zone_Id_Val, Buffer, Length, IO_Res);

         --  Step 2: Check I/O result
         if not Read_File_Result.Is_Ok (IO_Res) then
            declare
               Err : constant TZif.Domain.Error.Error_Type :=
                 Read_File_Result.Error_Info (IO_Res);
            begin
               Result :=
                 Get_Transition_Result.Error
                   (Err.Kind,
                    TZif.Domain.Error.Error_Strings.To_String (Err.Message));
            end;
            return;
         end if;

         --  Step 3: Verify we read bytes
         if Length = 0 then
            Result :=
              Get_Transition_Result.Error
                (TZif.Domain.Error.IO_Error, "No data read from zone file");
            return;
         end if;

         --  Step 4: Parse TZif bytes into domain data
         declare
            Parser_Buffer : TZif.Domain.Parser.Byte_Array (1 .. Length);
         begin
            for I in 1 .. Length loop
               Parser_Buffer (I) := Buffer (I);
            end loop;

            TZif.Domain.Parser.Parse_From_Bytes
              (Bytes => Parser_Buffer, Length => Length, Result => Parse_Res);
         end;

         --  Step 5: Check parse result
         if not Parse_Result.Is_Ok (Parse_Res) then
            declare
               Err : constant TZif.Domain.Error.Error_Type :=
                 Parse_Result.Error_Info (Parse_Res);
            begin
               Result :=
                 Get_Transition_Result.Error
                   (Err.Kind,
                    TZif.Domain.Error.Error_Strings.To_String (Err.Message));
            end;
            return;
         end if;

         --  Step 6: Find transition type at specified epoch
         declare
            TZif_Data  : constant TZif.Domain.TZif_Data.TZif_Data_Type :=
              Parse_Result.Value (Parse_Res);
            Type_Index : constant Natural                              :=
              TZif.Domain.TZif_Data.Find_Type_At_Time (TZif_Data, Epoch);
            TZ_Type    : constant Timezone_Type_Record                 :=
              TZif.Domain.TZif_Data.Get_Type (TZif_Data, Type_Index);
            Info       : constant Transition_Info_Type                 :=
              Make_Transition_Info
                (Epoch_Time   => Epoch, UTC_Offset => TZ_Type.UTC_Offset,
                 Is_DST       => TZ_Type.Is_DST,
                 Abbreviation => Get_Abbreviation (TZ_Type));
         begin
            Result := Get_Transition_Result.Ok (Info);
         end;

      end Get_Transition_At_Epoch;

      ----------------------------------------------------------------------
      --  Get_Version
      --
      --  Simply delegates to the injected I/O procedure.
      --  No domain logic needed - this is a pure read operation.
      ----------------------------------------------------------------------
      procedure Get_Version
        (Source :     Source_Info_Type;
         Result : out Get_Version_Result_Type)
      is
      begin
         --  Delegate to injected I/O plugin
         Read_Version_File (Source, Result);
      end Get_Version;

      ----------------------------------------------------------------------
      --  Find_My_Id
      --
      --  Simply delegates to the injected I/O procedure.
      --  No domain logic needed - this is a pure read operation.
      ----------------------------------------------------------------------
      procedure Find_My_Id (Result : out Find_My_Id_Result_Type) is
      begin
         --  Delegate to injected I/O plugin
         Read_System_Timezone_Id (Result);
      end Find_My_Id;

      ----------------------------------------------------------------------
      --  List_All_Zones
      --
      --  Simply delegates to the injected I/O procedure.
      --  Sorting is done in the I/O layer.
      ----------------------------------------------------------------------
      procedure List_All_Zones
        (Source     :     Source_Info_Type;
         Descending :     Boolean;
         Result     : out List_All_Result_Type)
      is
      begin
         --  Delegate to injected I/O plugin
         List_Zones_In_Source (Source, Descending, Result);
      end List_All_Zones;

      ----------------------------------------------------------------------
      --  Load_Source
      --
      --  Simply delegates to the injected I/O procedure.
      --  Source metadata extraction is done in the I/O layer.
      ----------------------------------------------------------------------
      procedure Load_Source
        (Path : Load_Path_String; Result : out Load_Source_Result_Type)
      is
      begin
         --  Delegate to injected I/O plugin
         Load_Source_From_Path (Path, Result);
      end Load_Source;

      ----------------------------------------------------------------------
      --  Validate_Source
      --
      --  Simply delegates to the injected I/O procedure.
      --  Path validation is done in the I/O layer.
      ----------------------------------------------------------------------
      procedure Validate_Source
        (Path : Validate_Path_String; Result : out Validate_Source_Result_Type)
      is
      begin
         --  Delegate to injected I/O plugin
         Validate_Source_Path (Path, Result);
      end Validate_Source;

      ----------------------------------------------------------------------
      --  Find_By_Pattern
      --
      --  Simply delegates to the injected I/O procedure.
      --  Pattern matching is done in the I/O layer.
      ----------------------------------------------------------------------
      procedure Find_By_Pattern
        (Pattern :     Pattern_String_Type;
         Yield   :     Pattern_Callback_Type;
         Result  : out Find_By_Pattern_Result_Type)
      is
      begin
         --  Delegate to injected I/O plugin
         Find_Zones_By_Pattern (Pattern, Yield, Result);
      end Find_By_Pattern;

      ----------------------------------------------------------------------
      --  Find_By_Region
      --
      --  Simply delegates to the injected I/O procedure.
      --  Region matching is done in the I/O layer.
      ----------------------------------------------------------------------
      procedure Find_By_Region
        (Region :     Region_String_Type;
         Yield  :     Region_Callback_Type;
         Result : out Find_By_Region_Result_Type)
      is
      begin
         --  Delegate to injected I/O plugin
         Find_Zones_By_Region (Region, Yield, Result);
      end Find_By_Region;

      ----------------------------------------------------------------------
      --  Find_By_Regex
      --
      --  Simply delegates to the injected I/O procedure.
      --  Regex matching is done in the I/O layer.
      ----------------------------------------------------------------------
      procedure Find_By_Regex
        (Regex  :     Regex_String_Type;
         Yield  :     Regex_Callback_Type;
         Result : out Find_By_Regex_Result_Type)
      is
      begin
         --  Delegate to injected I/O plugin
         Find_Zones_By_Regex (Regex, Yield, Result);
      end Find_By_Regex;

   end All_Operations;

end TZif.Application.Operations;
