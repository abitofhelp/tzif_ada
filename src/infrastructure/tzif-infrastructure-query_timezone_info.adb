pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.Query_Timezone_Info
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Query Timezone Info implementation.
--
--  ===========================================================================

with TZif.Infrastructure.TZif_Parser;
with TZif.Domain.Service.Timezone_Lookup;
with TZif.Domain.TZif_Data;
with TZif.Domain.Value_Object.Timezone_Type;

package body TZif.Infrastructure.Query_Timezone_Info is

   use TZif.Infrastructure.TZif_Parser;
   use TZif.Domain.TZif_Data;
   use TZif.Domain.Value_Object.Timezone_Type;
   use TZif.Domain.Error;

   package Tz_Lookup renames TZif.Domain.Service.Timezone_Lookup;

   --  ========================================================================
   --  Query_Timezone_Info
   --  ========================================================================

   function Query_Timezone_Info
     (Timezone_File : String; Epoch_Time : Epoch_Seconds_Type)
      return Query_Result_Type
   is
      --  Step 1: Parse timezone file
      Parse_Result : constant Parse_Result_Type :=
        Parse_From_File (Timezone_File);
   begin
      --  Check if parsing succeeded
      if not Infrastructure.TZif_Parser.Parse_Result.Is_Ok (Parse_Result) then
         --  Return parsing error
         declare
            Parse_Error   : constant Error_Type :=
              Infrastructure.TZif_Parser.Parse_Result.Error_Info
                (Parse_Result);
            Error_Message : constant String     :=
              Error_Strings.To_String (Parse_Error.Message);
         begin
            return Info_Result.Error (Parse_Error.Kind, Error_Message);
         end;
      end if;

      --  Step 2: Extract parsed data
      declare
         Data : constant TZif_Data_Type :=
           Infrastructure.TZif_Parser.Parse_Result.Value (Parse_Result);

         --  Step 3: Query domain service for timezone information
         Offset_Result : constant Tz_Lookup.UTC_Offset_Option :=
           Tz_Lookup.Find_UTC_Offset_At_Time (Data, Epoch_Time);

         DST_Result : constant Tz_Lookup.Boolean_Option :=
           Tz_Lookup.Is_DST_At_Time (Data, Epoch_Time);

         Abbrev_Result : constant Tz_Lookup.Abbreviation_Option :=
           Tz_Lookup.Get_Abbreviation_At_Time (Data, Epoch_Time);
      begin
         --  Check if all lookups succeeded (valid timezone types exist)
         if Tz_Lookup.UTC_Offset_Options.Is_None (Offset_Result)
           or else Tz_Lookup.Boolean_Options.Is_None (DST_Result)
           or else Tz_Lookup.Abbreviation_Options.Is_None (Abbrev_Result)
         then
            return
              Info_Result.Error
                (Not_Found_Error, "No timezone types available in TZif data");
         end if;

         --  Step 4: Build result with unwrapped values
         declare
            Abbrev : constant String :=
              Abbreviation_Strings.To_String
                (Tz_Lookup.Abbreviation_Options.Value (Abbrev_Result));
            Info   : Timezone_Info;
         begin
            Info.UTC_Offset :=
              Tz_Lookup.UTC_Offset_Options.Value (Offset_Result);
            Info.Is_DST     :=
              Tz_Lookup.Boolean_Options.Value (DST_Result);
            Info.Abbr_Length := Abbrev'Length;

            --  Copy abbreviation (pad if necessary)
            for I in 1 .. Info.Abbreviation'Length loop
               if I <= Abbrev'Length then
                  Info.Abbreviation (I) := Abbrev (Abbrev'First + I - 1);
               else
                  Info.Abbreviation (I) := ' ';
               end if;
            end loop;

            return Info_Result.Ok (Info);
         end;
      end;
   end Query_Timezone_Info;

end TZif.Infrastructure.Query_Timezone_Info;
