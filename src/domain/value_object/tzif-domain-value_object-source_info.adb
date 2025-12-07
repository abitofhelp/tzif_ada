pragma Ada_2022;
--  ===========================================================================
--  TZif.Domain.Value_Object.Source_Info
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Source Info value object - immutable domain data.
--
--  ===========================================================================

with TZif.Domain.Error;

package body TZif.Domain.Value_Object.Source_Info is

   use ULID_Strings;
   use TZif.Domain.Error;

   --  ========================================================================
   --  ULID Functions
   --  ========================================================================

   function Is_Valid_ULID_String (S : String) return Boolean is
      --  O(1) lookup table for valid Base32 characters
      --  Replaces O(n*m) nested loop with O(n) single pass
      Valid_Chars : constant array (Character) of Boolean :=
        ['0' .. '9' => True,  --  Digits 0-9
         'A' .. 'H' => True,  --  Letters A-H (no I)
         'J'        => True,  --  J (no I)
         'K' .. 'N' => True,  --  K-N (no L)
         'P' .. 'T' => True,  --  P-T (no O)
         'V' .. 'Z' => True,  --  V-Z (no U)
         others     => False];
   begin
      --  Check length
      if S'Length /= ULID_Length then
         return False;
      end if;

      --  Check all characters are valid base32 (O(n) with lookup table)
      for C of S loop
         if not Valid_Chars (C) then
            return False;
         end if;
      end loop;

      return True;
   end Is_Valid_ULID_String;

   function Make_ULID (Value : String) return ULID_Type is
   begin
      return ULID_Strings.To_Bounded_String (Value);
   end Make_ULID;

   function Parse_ULID (S : String) return ULID_Result.Result is
   begin
      if not Is_Valid_ULID_String (S) then
         return
           ULID_Result.Error
             (Validation_Error,
              "Invalid ULID string: must be" &
              Natural'Image (ULID_Length) &
              " characters of Crockford base32 alphabet");
      end if;

      return ULID_Result.Ok (Make_ULID (S));
   end Parse_ULID;

   function Null_ULID return ULID_Type is
   begin
      return Make_ULID ("00000000000000000000000000");
   end Null_ULID;

   function Is_Null (ID : ULID_Type) return Boolean is
     (ID = Null_ULID);

   --  ========================================================================
   --  Make_Version
   --  ========================================================================

   function Make_Version (Value : String) return Version_String_Type is
   begin
      return Version_Strings.To_Bounded_String (Value);
   end Make_Version;

   --  ========================================================================
   --  Make_Path
   --  ========================================================================

   function Make_Path (Value : String) return Path_String_Type is
   begin
      return Path_Strings.To_Bounded_String (Value);
   end Make_Path;

   --  ========================================================================
   --  Make_Source_Info
   --  ========================================================================

   function Make_Source_Info
     (ULID : ULID_Type; Path : Path_String_Type; Version : Version_String_Type;
      Zone_Count : Natural) return Source_Info_Type
   is
   begin
      return
        Source_Info_Type'
          (ULID       => ULID, Path => Path, Version => Version,
           Zone_Count => Zone_Count);
   end Make_Source_Info;

   --  ========================================================================
   --  Accessors
   --  ========================================================================

   function Get_ULID (Source : Source_Info_Type) return ULID_Type is
     (Source.ULID);

   function Get_Path (Source : Source_Info_Type) return Path_String_Type is
     (Source.Path);

   function Get_Version
     (Source : Source_Info_Type) return Version_String_Type is
     (Source.Version);

   function Get_Zone_Count (Source : Source_Info_Type) return Natural is
     (Source.Zone_Count);

   --  ========================================================================
   --  Comparison
   --  ========================================================================

   overriding function "=" (Left, Right : Source_Info_Type) return Boolean is
     (Left.ULID = Right.ULID);

   function "<" (Left, Right : Source_Info_Type) return Boolean is
     (Left.ULID < Right.ULID);

end TZif.Domain.Value_Object.Source_Info;
