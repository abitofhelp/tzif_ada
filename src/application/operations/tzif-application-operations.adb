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
with TZif.Domain.Error;

package body TZif.Application.Operations with
  SPARK_Mode => On
is

   package body All_Operations is

      use TZif.Domain.Error.Error_Strings;

      --  Formal parameters wired but not yet used in stub implementations
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
         --  TODO: Make buffer size configurable
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
                (TZif.Domain.Error.Infrastructure_Error,
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

   end All_Operations;

end TZif.Application.Operations;
