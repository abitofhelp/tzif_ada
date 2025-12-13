pragma Ada_2022;
--  ===========================================================================
--  TZif.Domain.Error.Result
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Result interface and type definitions.
--
--  Key Types:
--    T
--    T
--    Result
--    Result_State
--    Result
--
--  Dependencies:
--    Preelaborate
--    Inline
--    Inline
--
--  ===========================================================================

package TZif.Domain.Error.Result with
  Preelaborate
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

      --  Constructors
      function Ok (Value : T) return Result with
        Inline;

      function Error (Kind : Error_Kind; Message : String) return Result with
        Inline;

      --  =====================================================================
      --  Convenience constructor from Error_Type record.
      --  Used primarily at infrastructure boundaries when converting
      --  exceptions to Results via Functional.Try.Map_To_Result.
      --
      --  Example:
      --    Err => Int32_Result.From_Error
      --
      --  This allows infrastructure code to map exceptions to Error_Type,
      --  then convert to Result without exposing Error constructor details.
      --  =====================================================================
      function From_Error (Err : Error_Type) return Result with
        Inline;

      --  Query functions
      function Is_Ok (Self : Result) return Boolean with
        Inline;

      function Is_Error (Self : Result) return Boolean with
        Inline;

      --  Value extraction (caller must check Is_Ok/Is_Error first)
      function Value (Self : Result) return T with
        Pre => Is_Ok (Self), Inline;

      function Error_Info (Self : Result) return Error_Type with
        Pre => Is_Error (Self), Inline;

      --  =====================================================================
      --  Combinators (Railway-Oriented Programming)
      --  =====================================================================

      --  Same-type bind: T -> Result[T]
      --  Applies F only when Self is Ok; otherwise propagates error
      generic
         with function F (X : T) return Result;
      function And_Then (Self : Result) return Result;

      --  Note: Map and And_Then_To for type transformation require
      --  instantiation at the call site with both Result types.
      --  See Infrastructure layer for usage examples.

      --  Map_Error: transform the error value (E -> E)
      generic
         with function G (E : Error_Type) return Error_Type;
      function Map_Error (Self : Result) return Result;

      --  With_Context: enrich the error with a location/breadcrumb
      --  Requires a domain function to append context to errors
      generic
         with function Add (E : Error_Type; Where : String) return Error_Type;
      function With_Context (Self : Result; Where : String) return Result;

      --  =====================================================================
      --  Extractors with defaults
      --  =====================================================================

      --  Unwrap_Or: extract value or return default
      function Unwrap_Or (Self : Result; Default : T) return T with
        Post =>
          (if Is_Ok (Self) then Unwrap_Or'Result = Value (Self)
           else Unwrap_Or'Result = Default);

      --  Unwrap_Or_With: extract value or compute default lazily
      generic
         with function F return T;
      function Unwrap_Or_With (Self : Result) return T;

      --  =====================================================================
      --  Mapping and transformation
      --  =====================================================================

      --  Map: transform Ok value (keeps same type)
      generic
         with function F (X : T) return T;
      function Map (Self : Result) return Result;

      --  Bimap: transform both Ok and Error values simultaneously
      generic
         with function Map_Ok (X : T) return T;
         with function Map_Err (E : Error_Type) return Error_Type;
      function Bimap (Self : Result) return Result;

      --  =====================================================================
      --  Fallback and recovery
      --  =====================================================================

      --  Fallback: try alternative on error (eager evaluation)
      function Fallback (A, B : Result) return Result;

      --  Fallback_With: try alternative on error (lazy evaluation)
      generic
         with function F return Result;
      function Fallback_With (Self : Result) return Result;

      --  Recover: turn error into value
      generic
         with function Handle (E : Error_Type) return T;
      function Recover (Self : Result) return T;

      --  Recover_With: turn error into another Result
      generic
         with function Handle (E : Error_Type) return Result;
      function Recover_With (Self : Result) return Result;

      --  =====================================================================
      --  Validation
      --  =====================================================================

      --  Ensure: validate Ok value with predicate
      generic
         with function Pred (X : T) return Boolean;
         with function To_Error (X : T) return Error_Type;
      function Ensure (Self : Result) return Result;

      --  =====================================================================
      --  Side effects
      --  =====================================================================

      --  Tap: run side effects without changing Result (for logging/debugging)
      generic
         with procedure On_Ok (V : T);
         with procedure On_Err (E : Error_Type);
      function Tap (Self : Result) return Result;

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

   end Generic_Result;

   --  ========================================================================
   --  Cross-Type Chaining: And_Then_Into
   --  ========================================================================
   --
   --  This generic function enables chaining fallible operations that return
   --  DIFFERENT Result types. This is essential for railway-oriented
   --  programming when transforming between types.

   generic
      type T is private;
      type U is private;
      with package Source_Result is new Generic_Result (T => T);
      with package Target_Result is new Generic_Result (T => U);
      with function F (X : T) return Target_Result.Result;
   function And_Then_Into
     (Self : Source_Result.Result) return Target_Result.Result;
   --  Chain fallible operations that return different Result types
   --  If Self is Error, converts to Target_Result.Error (same error info)
   --  If Self is Ok, calls F with value (F might return Error)

end TZif.Domain.Error.Result;
