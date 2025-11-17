pragma Ada_2022;
--  ===========================================================================
--  Tzif.Domain.Error.Result
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Result implementation.
--
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

      -----------
      -- Is_Ok --
      -----------

      function Is_Ok (Self : Result) return Boolean is
      begin
         return Self.State = Ok_State;
      end Is_Ok;

      --------------
      -- Is_Error --
      --------------

      function Is_Error (Self : Result) return Boolean is
      begin
         return Self.State = Error_State;
      end Is_Error;

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

      --------------
      -- And_Then --
      --------------

      function And_Then
        (Self : Result; F : not null access function (X : T) return Result)
         return Result
      is
      begin
         if Self.State = Ok_State then
            return F (Self.Success_Value);
         else
            return Self;
         end if;
      end And_Then;

      ---------------
      -- Map_Error --
      ---------------

      function Map_Error (Self : Result) return Result is
      begin
         if Self.State = Ok_State then
            return Self;
         else
            return
              Result'
                (State => Error_State, Error_Value => G (Self.Error_Value));
         end if;
      end Map_Error;

      ------------------
      -- With_Context --
      ------------------

      function With_Context (Self : Result; Where : String) return Result is
         function Add_This (E : Error_Type) return Error_Type is
           (Add (E, Where));
         function Map_E is new Map_Error (G => Add_This);
      begin
         return Map_E (Self);
      end With_Context;

   end Generic_Result;

end TZif.Domain.Error.Result;
