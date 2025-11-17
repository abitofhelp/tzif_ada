pragma Ada_2022;
--  ===========================================================================
--  Tzif.Domain.Value_Object.Source_Info
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Source Info value object - immutable domain data.
--
--  ===========================================================================

package body TZif.Domain.Value_Object.Source_Info is

   use ULID_Strings;

   --  ========================================================================
   --  Make_ULID
   --  ========================================================================

   function Make_ULID (Value : String) return ULID_Type is
   begin
      return ULID_Strings.To_Bounded_String (Value);
   end Make_ULID;

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
