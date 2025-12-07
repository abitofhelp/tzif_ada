pragma Ada_2022;
--  ===========================================================================
--  TZif.Domain.Value_Object.Zone_Id.Result
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Result value object - immutable domain data.
--
--  Responsibilities:
--    - Define Result type and operations
--    - Provide constructors and accessors
--    - Validate domain constraints
--
--  Key Types:
--    Result
--
--  Dependencies:
--    TZif.Domain.Error
--    TZif.Domain.Error.Result
--    Preelaborate
--
--  ===========================================================================

with TZif.Domain.Error;
with TZif.Domain.Error.Result;

package TZif.Domain.Value_Object.Zone_Id.Result with
  Preelaborate
is

   --  ========================================================================
   --  Specialized Result Type for Zone_Id
   --  ========================================================================

   package Impl is new TZif.Domain.Error.Result.Generic_Result
     (T => TZif.Domain.Value_Object.Zone_Id.Zone_Id_Type);

   --  Public alias for the specialized Result type
   subtype Result is Impl.Result;

   --  Re-export core Result operations for convenience

   function Ok
     (Value : TZif.Domain.Value_Object.Zone_Id.Zone_Id_Type)
      return Result renames
     Impl.Ok;

   function Error
     (Kind : TZif.Domain.Error.Error_Kind; Message : String)
      return Result renames
     Impl.Error;

   function Is_Ok (Self : Result) return Boolean renames Impl.Is_Ok;

   function Is_Error (Self : Result) return Boolean renames Impl.Is_Error;

   function Value
     (Self : Result)
      return TZif.Domain.Value_Object.Zone_Id.Zone_Id_Type renames
     Impl.Value;

   function Error_Info
     (Self : Result) return TZif.Domain.Error.Error_Type renames
     Impl.Error_Info;

   --  ========================================================================
   --  Smart Constructor
   --  ========================================================================

   --  Validate and create zone ID using Result monad (functional error
   --  handling). Returns Ok(Zone_Id) if valid, Error if empty or exceeds
   --  max length.
   --
   --  NO EXCEPTIONS: Uses pure functional validation via length checks.
   function Validate_Zone_Id (Id : String) return Result;

end TZif.Domain.Value_Object.Zone_Id.Result;
