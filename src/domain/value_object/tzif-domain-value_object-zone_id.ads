pragma Ada_2022;
--  ===========================================================================
--  TZif.Domain.Value_Object.Zone_Id
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Zone Id value object - immutable domain data.
--
--  Responsibilities:
--    - Define Zone Id type and operations
--    - Provide constructors and accessors
--
--  Key Types:
--    Zone_Id_Type
--    Zone_Id_String
--    Zone_Id_Type
--
--  Dependencies:
--    TZif_Config
--    Preelaborate
--    Post => To_String'Result'Length <= TZif_Config.Max_Zone_ID_Length
--
--  ===========================================================================

with Ada.Strings.Bounded;
with TZif_Config;

package TZif.Domain.Value_Object.Zone_Id with
  Preelaborate
is

   --  ========================================================================
   --  Bounded String for Zone Identifiers
   --  ========================================================================

   package Zone_Id_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max => TZif_Config.Max_Zone_ID_Length);

   --  ========================================================================
   --  Zone_Id Type
   --  ========================================================================

   --  Private type ensures construction through validated or explicit
   --  constructors. Validation-focused Result wrappers live in the
   --  child package TZif.Domain.Value_Object.Zone_Id.Result.
   type Zone_Id_Type is private;

   --  Aliased string view (for compatibility with existing terminology)
   subtype Zone_Id_String is Zone_Id_Type;

   --  ========================================================================
   --  Default Value (for SPARK-compatible bounded containers)
   --  ========================================================================

   --  Returns an empty Zone_Id (used for array initialization)
   function Default_Zone_Id return Zone_Id_Type with
     Inline;

   --  ========================================================================
   --  Validation (Single Source of Truth for Business Rules)
   --  ========================================================================

   --  Check if string is a valid Zone_Id.
   --  Business Rules:
   --    - Must not be empty
   --    - Must not exceed Max_Zone_ID_Length
   function Is_Valid (Id : String) return Boolean is
     (Id'Length > 0 and then Id'Length <= TZif_Config.Max_Zone_ID_Length);

   --  ========================================================================
   --  Constructor Functions
   --  ========================================================================

   --  Unchecked constructor (precondition enforces validity).
   --  Use for internal/trusted code where caller has already validated.
   --  For public API, use Make_Zone_Id from Zone_Id.Result package.
   function Make_Zone_Id (Id : String) return Zone_Id_Type
     with Pre => Is_Valid (Id);

   --  Create zone ID with truncation (no exception, no validation).
   --  Extra characters on the right are dropped to fit within the
   --  bounded length.
   function Make_Zone_Id_Truncate (Id : String) return Zone_Id_Type;

   --  ========================================================================
   --  Query Functions
   --  ========================================================================

   --  Convert to string
   function To_String (Id : Zone_Id_Type) return String with
     Post => To_String'Result'Length <= TZif_Config.Max_Zone_ID_Length;

   --  Length of the zone identifier
   function Length (Id : Zone_Id_Type) return Natural with
     Post => Length'Result <= TZif_Config.Max_Zone_ID_Length;

   --  Check if zone ID is empty
   function Is_Empty (Id : Zone_Id_Type) return Boolean;

   --  Check if zone ID string matches a given String
   function Matches (Id : Zone_Id_Type; Value : String) return Boolean;

   --  ========================================================================
   --  Comparison Functions
   --  ========================================================================

   --  Equality
   overriding function "=" (Left, Right : Zone_Id_Type) return Boolean;

   --  Less than (for ordering/sorting)
   function "<" (Left, Right : Zone_Id_Type) return Boolean;

private

   --  Internal representation: bounded string wrapper
   type Zone_Id_Type is record
      ID : Zone_Id_Strings.Bounded_String;
   end record;

   --  Private unchecked constructor used by Result wrappers
   --  Pre: Id'Length must be <= Max_Zone_ID_Length
   function Make_Unchecked (Id : String) return Zone_Id_Type with
     Pre => Id'Length <= TZif_Config.Max_Zone_ID_Length;

end TZif.Domain.Value_Object.Zone_Id;
