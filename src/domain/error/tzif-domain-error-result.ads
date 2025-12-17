pragma Ada_2022;
--  ===========================================================================
--  TZif.Domain.Error.Result - Generic Result monad for error handling
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Result[T] monad for functional error handling. This is the core
--    error handling primitive for the domain layer.
--
--  Architecture Notes:
--    - Generic over success type T
--    - Uses TZif.Domain.Error.Error_Type for all errors
--    - Pure domain implementation (no external dependencies)
--    - Minimal API: constructors, predicates, extractors only
--
--  Usage:
--    with TZif.Domain.Error.Result;
--
--    package String_Result is new TZif.Domain.Error.Result.Generic_Result
--      (T => String);
--
--    R : String_Result.Result := String_Result.Ok ("success");
--    if String_Result.Is_Ok (R) then
--       Value := String_Result.Value (R);
--    end if;
--
--  Combinators:
--    This package provides only essential operations. For advanced
--    combinators (Map, And_Then, Fallback, Recover, etc.), see
--    Functional.Result in the functional crate, which provides a
--    fully SPARK-proven implementation with 30+ operations.
--
--  See Also:
--    TZif.Domain.Error - Error types used by this monad
--    Functional.Result - Full combinator library (infrastructure layer)
--  ===========================================================================

package TZif.Domain.Error.Result
  with Preelaborate, SPARK_Mode => On
is

   --  ========================================================================
   --  Generic Result Type: Either monad for T or Error_Type
   --  ========================================================================

   --  This generic package must be instantiated for each success type T
   --  Example: package String_Result is new Generic_Result (T => String);
   --
   --  Flow:
   --  1. Operations that can fail return Result[T] instead of raising
   --  2. Caller checks Is_Ok/Is_Error before extracting value
   --  3. Forces explicit error handling at compile time

   generic
      type T is private;  --  The success value type
   package Generic_Result is

      --  Opaque result type - internal representation hidden
      type Result is private;

      --  =====================================================================
      --  Constructors
      --  =====================================================================

      function Ok (Value : T) return Result
      with
         Inline,
         Post => Is_Ok (Ok'Result);

      function Error (Kind : Error_Kind; Message : String) return Result
      with
         Inline,
         Post => Is_Error (Error'Result);

      --  From_Error: construct Result from pre-existing Error_Type record
      --  Used at infrastructure boundaries for exception-to-Result conversion
      function From_Error (Err : Error_Type) return Result
      with
         Inline,
         Post => Is_Error (From_Error'Result);

      --  =====================================================================
      --  Query functions
      --  =====================================================================

      function Is_Ok (Self : Result) return Boolean
      with Inline, Global => null;

      function Is_Error (Self : Result) return Boolean
      with Inline, Global => null;

      --  =====================================================================
      --  Value extraction
      --  =====================================================================

      function Value (Self : Result) return T
      with Pre => Is_Ok (Self), Inline;

      function Error_Info (Self : Result) return Error_Type
      with Pre => Is_Error (Self), Inline;

   private

      --  Internal representation: discriminated record (tagged union pattern)
      type Result_State is (Ok_State, Error_State);

      type Result (State : Result_State := Error_State) is record
         case State is
            when Ok_State =>
               Success_Value : T;

            when Error_State =>
               Error_Value : Error_Type;
         end case;
      end record;

      --  Expression function completions for SPARK proof visibility
      function Is_Ok (Self : Result) return Boolean is (Self.State = Ok_State);
      function Is_Error (Self : Result) return Boolean is
        (Self.State = Error_State);

   end Generic_Result;

end TZif.Domain.Error.Result;
