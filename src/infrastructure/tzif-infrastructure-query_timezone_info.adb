pragma Ada_2022;
--  ===========================================================================
--  Tzif.Infrastructure.Query_Timezone_Info
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

package body TZif.Infrastructure.Query_Timezone_Info is

   use TZif.Infrastructure.TZif_Parser;
   use TZif.Domain.Service.Timezone_Lookup;
   use TZif.Domain.TZif_Data;

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
            use TZif.Domain.Error;
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
         UTC_Offset : constant UTC_Offset_Type :=
           Find_UTC_Offset_At_Time (Data, Epoch_Time);

         Is_DST : constant Boolean :=
           Domain.Service.Timezone_Lookup.Is_DST_At_Time (Data, Epoch_Time);

         Abbrev : constant String :=
           Domain.Service.Timezone_Lookup.Get_Abbreviation_At_Time
             (Data, Epoch_Time);

         --  Step 4: Build result
         Info : Timezone_Info;
      begin
         Info.UTC_Offset  := UTC_Offset;
         Info.Is_DST      := Is_DST;
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
   end Query_Timezone_Info;

end TZif.Infrastructure.Query_Timezone_Info;
