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

      ------------
      -- Expect --
      ------------

      function Expect (Self : Result; Msg : String) return T is
         pragma Unreferenced (Msg);
      begin
         return Self.Success_Value;
      end Expect;

      ---------------
      -- Unwrap_Or --
      ---------------

      function Unwrap_Or (Self : Result; Default : T) return T is
      begin
         case Self.State is
            when Ok_State =>
               return Self.Success_Value;

            when Error_State =>
               return Default;
         end case;
      end Unwrap_Or;

      --------------------
      -- Unwrap_Or_With --
      --------------------

      function Unwrap_Or_With (Self : Result) return T is
      begin
         case Self.State is
            when Ok_State =>
               return Self.Success_Value;

            when Error_State =>
               return F;
         end case;
      end Unwrap_Or_With;

      ---------
      -- Map --
      ---------

      function Map (Self : Result) return Result is
      begin
         case Self.State is
            when Ok_State =>
               return Ok (F (Self.Success_Value));

            when Error_State =>
               return Self;
         end case;
      end Map;

      -----------
      -- Bimap --
      -----------

      function Bimap (Self : Result) return Result is
      begin
         case Self.State is
            when Ok_State =>
               return Ok (Map_Ok (Self.Success_Value));

            when Error_State =>
               return
                 Result'
                   (State       => Error_State,
                    Error_Value => Map_Err (Self.Error_Value));
         end case;
      end Bimap;

      --------------
      -- Fallback --
      --------------

      function Fallback (A, B : Result) return Result is
      begin
         case A.State is
            when Ok_State =>
               return A;

            when Error_State =>
               return B;
         end case;
      end Fallback;

      -------------------
      -- Fallback_With --
      -------------------

      function Fallback_With (Self : Result) return Result is
      begin
         case Self.State is
            when Ok_State =>
               return Self;

            when Error_State =>
               return F;
         end case;
      end Fallback_With;

      -------------
      -- Recover --
      -------------

      function Recover (Self : Result) return T is
      begin
         case Self.State is
            when Ok_State =>
               return Self.Success_Value;

            when Error_State =>
               return Handle (Self.Error_Value);
         end case;
      end Recover;

      ------------------
      -- Recover_With --
      ------------------

      function Recover_With (Self : Result) return Result is
      begin
         case Self.State is
            when Ok_State =>
               return Self;

            when Error_State =>
               return Handle (Self.Error_Value);
         end case;
      end Recover_With;

      ------------
      -- Ensure --
      ------------

      function Ensure (Self : Result) return Result is
      begin
         case Self.State is
            when Ok_State =>
               if Pred (Self.Success_Value) then
                  return Self;
               else
                  return
                    Result'
                      (State       => Error_State,
                       Error_Value => To_Error (Self.Success_Value));
               end if;

            when Error_State =>
               return Self;
         end case;
      end Ensure;

      ---------
      -- Tap --
      ---------

      function Tap (Self : Result) return Result is
      begin
         case Self.State is
            when Ok_State =>
               On_Ok (Self.Success_Value);

            when Error_State =>
               On_Err (Self.Error_Value);
         end case;
         return Self;
      end Tap;

   end Generic_Result;

   -------------------
   -- And_Then_Into --
   -------------------

   function And_Then_Into
     (Self : Source_Result.Result) return Target_Result.Result
   is
      use Error_Strings;
   begin
      if Source_Result.Is_Ok (Self) then
         --  Call the transformation function with the success value
         return F (Source_Result.Value (Self));
      else
         --  Convert error to target Result type (same error info)
         declare
            Err_Info : constant Error_Type := Source_Result.Error_Info (Self);
         begin
            return Target_Result.Error
              (Kind    => Err_Info.Kind,
               Message => To_String (Err_Info.Message));
         end;
      end if;
   end And_Then_Into;

end TZif.Domain.Error.Result;
