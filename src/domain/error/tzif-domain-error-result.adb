pragma Ada_2022;
--  ===========================================================================
--  TZif.Domain.Error.Result - Implementation of Result monad
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Implements Generic_Result constructors (Ok, Error, From_Error),
--    predicates (Is_Ok, Is_Error), and extractors (Value, Error_Info).
--  ===========================================================================

package body TZif.Domain.Error.Result is

   package body Generic_Result is

      ----------
      -- Ok --
      ----------

      function Ok (Value : T) return Result is
      begin
         return (State => Ok_State, Success_Value => Value);
      end Ok;

      -----------
      -- Error --
      -----------

      function Error (Kind : Error_Kind; Message : String) return Result is
         use Error_Strings;
      begin
         return
           (State       => Error_State,
            Error_Value =>
              (Kind => Kind, Message => To_Bounded_String (Message)));
      end Error;

      ----------------
      -- From_Error --
      ----------------

      function From_Error (Err : Error_Type) return Result is
      begin
         return (State => Error_State, Error_Value => Err);
      end From_Error;

      --  Is_Ok and Is_Error are expression functions in the spec (for SPARK)

      -----------
      -- Value --
      -----------

      function Value (Self : Result) return T is
      begin
         return Self.Success_Value;
      end Value;

      ----------------
      -- Error_Info --
      ----------------

      function Error_Info (Self : Result) return Error_Type is
      begin
         return Self.Error_Value;
      end Error_Info;

   end Generic_Result;

end TZif.Domain.Error.Result;
