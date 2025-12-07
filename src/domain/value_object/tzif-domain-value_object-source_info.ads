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
--  Responsibilities:
--    - Define Source Info type and operations
--    - Provide constructors and accessors
--
--  Key Types:
--    ULID_Type
--    Version_String_Type
--    Path_String_Type
--    Source_Info_Type
--    Source_Info_Type
--
--  Dependencies:
--    TZif_Config
--    Preelaborate
--    Pre => Value'Length = ULID_Length
--
--  ===========================================================================

with Ada.Strings;
with Ada.Strings.Bounded;
with TZif_Config;
with TZif.Domain.Error.Result;

package TZif.Domain.Value_Object.Source_Info with
  Preelaborate
is

   --  ========================================================================
   --  ULID Type (26 characters, Base32 encoded)
   --  ========================================================================
   --  ULIDs are lexicographically sortable, timestamp-based unique identifiers
   --  Format: 10 bytes timestamp + 16 bytes randomness = 26 Base32 characters
   --  ========================================================================

   ULID_Length : constant := 26;

   package ULID_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (ULID_Length);
   subtype ULID_Type is ULID_Strings.Bounded_String;

   --  Crockford's Base32 alphabet (for validation)
   Base32_Alphabet : constant String := "0123456789ABCDEFGHJKMNPQRSTVWXYZ";

   --  Null/zero ULID (all '0' characters)
   function Null_ULID return ULID_Type
     with Inline,
          Post => To_String (Null_ULID'Result) = [1 .. ULID_Length => '0'];

   --  Validate ULID string format
   function Is_Valid_ULID_String (S : String) return Boolean
     with Post =>
       (if Is_Valid_ULID_String'Result then
          S'Length = ULID_Length
          and then
          (for all C of S =>
             (for some Valid of Base32_Alphabet => C = Valid)));

   --  Unsafe constructor (precondition enforces validity)
   function Make_ULID (Value : String) return ULID_Type
     with Pre => Is_Valid_ULID_String (Value);

   --  Safe ULID parser with Result monad
   package ULID_Result is new TZif.Domain.Error.Result.Generic_Result
     (T => ULID_Type);

   function Parse_ULID (S : String) return ULID_Result.Result
     with Post =>
       (if ULID_Result.Is_Ok (Parse_ULID'Result) then
          not Is_Null (ULID_Result.Value (Parse_ULID'Result)));

   function To_String (ULID : ULID_Type) return String is
     (ULID_Strings.To_String (ULID))
     with Post => To_String'Result'Length = ULID_Length,
          Inline;

   --  Check if ULID is null/zero
   function Is_Null (ID : ULID_Type) return Boolean
     with Inline;

   --  ULID comparison (lexicographic)
   function "=" (Left, Right : ULID_Type) return Boolean renames
     ULID_Strings."=";
   function "<" (Left, Right : ULID_Type) return Boolean renames
     ULID_Strings."<";

   --  ========================================================================
   --  Version String Type
   --  ========================================================================
   --  Timezone database version (e.g., "2024b", "2023c")
   --  ========================================================================

   package Version_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (32);
   subtype Version_String_Type is Version_Strings.Bounded_String;

   function Make_Version (Value : String) return Version_String_Type with
     Pre => Value'Length <= 32;

   function To_String (Version : Version_String_Type) return String is
     (Version_Strings.To_String (Version));

   --  ========================================================================
   --  Path String Type
   --  ========================================================================

   package Path_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (TZif_Config.Max_Path_Length);
   subtype Path_String_Type is Path_Strings.Bounded_String;

   function Make_Path (Value : String) return Path_String_Type with
     Pre =>
      Value'Length > 0 and then Value'Length <= TZif_Config.Max_Path_Length;

   function To_String (Path : Path_String_Type) return String is
     (Path_Strings.To_String (Path));

   --  ========================================================================
   --  Source_Info_Type - The Value Object
   --  ========================================================================

   type Source_Info_Type is private;

   --  Constructor
   function Make_Source_Info
     (ULID : ULID_Type; Path : Path_String_Type; Version : Version_String_Type;
      Zone_Count : Natural) return Source_Info_Type;

   --  Accessors
   function Get_ULID (Source : Source_Info_Type) return ULID_Type;
   function Get_Path (Source : Source_Info_Type) return Path_String_Type;
   function Get_Version (Source : Source_Info_Type) return Version_String_Type;
   function Get_Zone_Count (Source : Source_Info_Type) return Natural;

   --  Comparison (by ULID)
   overriding function "=" (Left, Right : Source_Info_Type) return Boolean;
   function "<" (Left, Right : Source_Info_Type) return Boolean;

private

   type Source_Info_Type is record
      ULID       : ULID_Type;
      Path       : Path_String_Type;
      Version    : Version_String_Type;
      Zone_Count : Natural;
   end record;

end TZif.Domain.Value_Object.Source_Info;
