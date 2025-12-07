pragma Ada_2022;
--  ======================================================================
--  Test_Result_Combinators
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for TZif.Domain.Error.Result generic package.
--    Tests all combinator operations to achieve full coverage of
--    functional operations: And_Then, Map_Error, With_Context.
--  ======================================================================

with Ada.Text_IO; use Ada.Text_IO;
with Test_Framework;
with TZif.Domain.Error;
with TZif.Domain.Error.Result;

procedure Test_Result_Combinators is

   use TZif.Domain.Error;
   use Error_Strings;

   --  Instantiate Result with Integer for testing
   package Int_Result is new TZif.Domain.Error.Result.Generic_Result
     (T => Integer);

   --  Rename type to avoid hiding conflicts
   subtype Int_Res is Int_Result.Result;

   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;

   procedure Assert (Condition : Boolean; Test_Name : String) is
   begin
      Test_Count := Test_Count + 1;
      if Condition then
         Pass_Count := Pass_Count + 1;
         Put_Line ("  [PASS] " & Test_Name);
      else
         Put_Line ("  [FAIL] " & Test_Name);
      end if;
   end Assert;

   --  =====================================================================
   --  Helper functions for combinators
   --  =====================================================================

   --  For And_Then: chain function that doubles positive values
   function Double_Positive (X : Integer) return Int_Res is
   begin
      if X > 0 then
         return Int_Result.Ok (X * 2);
      else
         return Int_Result.Error (Validation_Error, "Value must be positive");
      end if;
   end Double_Positive;

   --  For And_Then: chain function that always fails
   function Always_Fail (X : Integer) return Int_Res is
      pragma Unreferenced (X);
   begin
      return Int_Result.Error (Internal_Error, "Always fails");
   end Always_Fail;

   --  For Map_Error: transform error by prefixing message
   function Prefix_Error (E : Error_Type) return Error_Type is
   begin
      return
        (Kind    => E.Kind,
         Message => To_Bounded_String ("Wrapped: " & To_String (E.Message)));
   end Prefix_Error;

   --  For Map_Error: transform error by changing kind
   function Upgrade_Error (E : Error_Type) return Error_Type is
   begin
      return (Kind => Internal_Error, Message => E.Message);
   end Upgrade_Error;

   --  For With_Context: add location to error message
   function Add_Location
     (E : Error_Type; Where : String) return Error_Type
   is
      New_Msg : constant String := To_String (E.Message) & " at " & Where;
   begin
      return (Kind => E.Kind, Message => To_Bounded_String (New_Msg));
   end Add_Location;

   --  Instantiate generic Map_Error functions
   function Map_Prefix is new Int_Result.Map_Error (G => Prefix_Error);
   function Map_Upgrade is new Int_Result.Map_Error (G => Upgrade_Error);

   --  Instantiate With_Context
   function Add_Context is new Int_Result.With_Context (Add => Add_Location);

begin
   Put_Line ("Test: Result Constructors and Predicates");
   --  Basic constructor tests
   declare
      Success : constant Int_Res := Int_Result.Ok (42);
      Failure : constant Int_Res :=
        Int_Result.Error (Validation_Error, "test error");
   begin
      Assert (Int_Result.Is_Ok (Success), "Ok creates successful result");
      Assert (not Int_Result.Is_Error (Success), "Ok result is not error");
      Assert (Int_Result.Value (Success) = 42, "Value extracts correct value");

      Assert (Int_Result.Is_Error (Failure), "Error creates error result");
      Assert (not Int_Result.Is_Ok (Failure), "Error result is not ok");
      Assert (Int_Result.Error_Info (Failure).Kind = Validation_Error,
              "Error_Info returns correct error kind");
   end;

   Put_Line ("Test: From_Error Constructor");
   --  Test From_Error for creating result from Error_Type
   declare
      Err     : constant Error_Type :=
        (Kind => Parse_Error, Message => To_Bounded_String ("parse failed"));
      Failure : constant Int_Res := Int_Result.From_Error (Err);
   begin
      Assert (Int_Result.Is_Error (Failure),
              "From_Error creates error result");
      Assert (Int_Result.Error_Info (Failure).Kind = Parse_Error,
              "From_Error preserves error kind");
      Assert (To_String (Int_Result.Error_Info (Failure).Message) =
                "parse failed",
              "From_Error preserves error message");
   end;

   Put_Line ("Test: And_Then - Chain Operations on Ok");
   --  And_Then on Ok with function returning Ok
   declare
      R       : constant Int_Res := Int_Result.Ok (5);
      Chained : constant Int_Res :=
        Int_Result.And_Then (R, Double_Positive'Access);
   begin
      Assert (Int_Result.Is_Ok (Chained), "And_Then on Ok returns Ok");
      Assert (Int_Result.Value (Chained) = 10,
              "And_Then applies function correctly");
   end;

   --  And_Then on Ok with function returning Error
   declare
      R       : constant Int_Res := Int_Result.Ok (-5);
      Chained : constant Int_Res :=
        Int_Result.And_Then (R, Double_Positive'Access);
   begin
      Assert (Int_Result.Is_Error (Chained),
              "And_Then on Ok can return Error from function");
      Assert (Int_Result.Error_Info (Chained).Kind = Validation_Error,
              "And_Then propagates error from function");
   end;

   --  And_Then on Ok with function that always fails
   declare
      R       : constant Int_Res := Int_Result.Ok (100);
      Chained : constant Int_Res :=
        Int_Result.And_Then (R, Always_Fail'Access);
   begin
      Assert (Int_Result.Is_Error (Chained),
              "And_Then can fail on any Ok value");
      Assert (Int_Result.Error_Info (Chained).Kind = Internal_Error,
              "And_Then returns correct error kind from function");
   end;

   Put_Line ("Test: And_Then - Short-Circuit on Error");
   --  And_Then on Error short-circuits (doesn't call function)
   declare
      R       : constant Int_Res :=
        Int_Result.Error (IO_Error, "file not found");
      Chained : constant Int_Res :=
        Int_Result.And_Then (R, Double_Positive'Access);
   begin
      Assert (Int_Result.Is_Error (Chained),
              "And_Then on Error returns Error");
      Assert (Int_Result.Error_Info (Chained).Kind = IO_Error,
              "And_Then preserves original error");
      Assert (To_String (Int_Result.Error_Info (Chained).Message) =
                "file not found",
              "And_Then preserves error message");
   end;

   Put_Line ("Test: Map_Error - Transform Error Values");
   --  Map_Error on Ok returns Ok unchanged
   declare
      R      : constant Int_Res := Int_Result.Ok (42);
      Mapped : constant Int_Res := Map_Prefix (R);
   begin
      Assert (Int_Result.Is_Ok (Mapped), "Map_Error on Ok returns Ok");
      Assert (Int_Result.Value (Mapped) = 42, "Map_Error preserves Ok value");
   end;

   --  Map_Error on Error applies transformation
   declare
      R      : constant Int_Res := Int_Result.Error (Parse_Error, "bad data");
      Mapped : constant Int_Res := Map_Prefix (R);
   begin
      Assert (Int_Result.Is_Error (Mapped),
              "Map_Error on Error returns Error");
      Assert (To_String (Int_Result.Error_Info (Mapped).Message) =
                "Wrapped: bad data",
              "Map_Error transforms error message");
      Assert (Int_Result.Error_Info (Mapped).Kind = Parse_Error,
              "Map_Error preserves error kind (when transformer does)");
   end;

   --  Map_Error can change error kind
   declare
      R      : constant Int_Res :=
        Int_Result.Error (Validation_Error, "original");
      Mapped : constant Int_Res := Map_Upgrade (R);
   begin
      Assert (Int_Result.Is_Error (Mapped), "Map_Error returns Error");
      Assert (Int_Result.Error_Info (Mapped).Kind = Internal_Error,
              "Map_Error can change error kind");
   end;

   Put_Line ("Test: With_Context - Enrich Errors with Location");
   --  With_Context on Ok returns Ok unchanged
   declare
      R        : constant Int_Res := Int_Result.Ok (100);
      Enriched : constant Int_Res := Add_Context (R, "test_location");
   begin
      Assert (Int_Result.Is_Ok (Enriched), "With_Context on Ok returns Ok");
      Assert (Int_Result.Value (Enriched) = 100,
              "With_Context preserves Ok value");
   end;

   --  With_Context on Error adds context
   declare
      R        : constant Int_Res :=
        Int_Result.Error (Not_Found_Error, "zone missing");
      Enriched : constant Int_Res := Add_Context (R, "Find_By_Id");
   begin
      Assert (Int_Result.Is_Error (Enriched),
              "With_Context on Error returns Error");
      Assert (To_String (Int_Result.Error_Info (Enriched).Message) =
                "zone missing at Find_By_Id",
              "With_Context appends location to error message");
      Assert (Int_Result.Error_Info (Enriched).Kind = Not_Found_Error,
              "With_Context preserves error kind");
   end;

   Put_Line ("Test: Chained Combinators");
   --  Chain And_Then calls
   declare
      R     : constant Int_Res := Int_Result.Ok (2);
      --  Double twice: 2 -> 4 -> 8
      Step1 : constant Int_Res :=
        Int_Result.And_Then (R, Double_Positive'Access);
      Step2 : constant Int_Res :=
        Int_Result.And_Then (Step1, Double_Positive'Access);
   begin
      Assert (Int_Result.Is_Ok (Step2), "Chained And_Then produces Ok");
      Assert (Int_Result.Value (Step2) = 8,
              "Chained And_Then computes correctly");
   end;

   --  Chain And_Then with error in middle
   declare
      R     : constant Int_Res := Int_Result.Ok (2);
      Step1 : constant Int_Res :=
        Int_Result.And_Then (R, Double_Positive'Access);
      Step2 : constant Int_Res :=
        Int_Result.And_Then (Step1, Always_Fail'Access);
      Step3 : constant Int_Res :=
        Int_Result.And_Then (Step2, Double_Positive'Access);
   begin
      Assert (Int_Result.Is_Error (Step3), "Error propagates through chain");
      Assert (Int_Result.Error_Info (Step3).Kind = Internal_Error,
              "Error kind preserved through chain");
   end;

   --  Chain Map_Error on error path
   declare
      R       : constant Int_Res :=
        Int_Result.Error (Validation_Error, "orig");
      Mapped1 : constant Int_Res := Map_Prefix (R);
      Mapped2 : constant Int_Res := Map_Upgrade (Mapped1);
   begin
      Assert (Int_Result.Is_Error (Mapped2),
              "Chained Map_Error produces Error");
      Assert (Int_Result.Error_Info (Mapped2).Kind = Internal_Error,
              "Chained Map_Error applies all transformations");
      Assert (To_String (Int_Result.Error_Info (Mapped2).Message) =
                "Wrapped: orig",
              "Chained Map_Error transforms message");
   end;

   --  Combine And_Then with context on error path
   declare
      R       : constant Int_Res := Int_Result.Ok (-1);
      Chained : constant Int_Res :=
        Int_Result.And_Then (R, Double_Positive'Access);
      Context : constant Int_Res := Add_Context (Chained, "Test_Chain");
   begin
      Assert (Int_Result.Is_Error (Context), "Combined chain produces Error");
      Assert (Int_Result.Error_Info (Context).Kind = Validation_Error,
              "Combined chain preserves error kind");
   end;

   Put_Line ("Test: Bimap - Transform Both Ok and Error");
   --  Bimap on Ok transforms value
   declare
      function Double (X : Integer) return Integer is (X * 2);

      function Change_Kind (E : Error_Type) return Error_Type is
        ((Kind => IO_Error, Message => E.Message));

      function Transform is new Int_Result.Bimap
        (Map_Ok  => Double,
         Map_Err => Change_Kind);

      R      : constant Int_Res := Int_Result.Ok (21);
      Mapped : constant Int_Res := Transform (R);
   begin
      Assert (Int_Result.Is_Ok (Mapped), "Bimap on Ok returns Ok");
      Assert (Int_Result.Value (Mapped) = 42,
              "Bimap transforms Ok value correctly");
   end;

   --  Bimap on Error transforms error
   declare
      function Double (X : Integer) return Integer is (X * 2);

      function Change_Kind (E : Error_Type) return Error_Type is
        ((Kind => IO_Error, Message => E.Message));

      function Transform is new Int_Result.Bimap
        (Map_Ok  => Double,
         Map_Err => Change_Kind);

      R      : constant Int_Res := Int_Result.Error (Validation_Error, "test");
      Mapped : constant Int_Res := Transform (R);
   begin
      Assert (Int_Result.Is_Error (Mapped), "Bimap on Error returns Error");
      Assert (Int_Result.Error_Info (Mapped).Kind = IO_Error,
              "Bimap transforms error kind");
   end;

   Put_Line ("Test: Ensure - Validate Ok Values");
   --  Ensure on Ok with predicate true keeps Ok
   declare
      function Is_Positive (X : Integer) return Boolean is (X > 0);

      function To_Validation_Error (X : Integer) return Error_Type is
         pragma Unreferenced (X);
      begin
         return
           (Kind    => Validation_Error,
            Message => To_Bounded_String ("Not positive"));
      end To_Validation_Error;

      function Validate is new Int_Result.Ensure
        (Pred     => Is_Positive,
         To_Error => To_Validation_Error);

      R      : constant Int_Res := Int_Result.Ok (10);
      Result : constant Int_Res := Validate (R);
   begin
      Assert (Int_Result.Is_Ok (Result),
              "Ensure keeps Ok if predicate holds");
      Assert (Int_Result.Value (Result) = 10,
              "Ensure preserves Ok value");
   end;

   --  Ensure on Ok with predicate false converts to Error
   declare
      function Is_Positive (X : Integer) return Boolean is (X > 0);

      function To_Validation_Error (X : Integer) return Error_Type is
         pragma Unreferenced (X);
      begin
         return
           (Kind    => Validation_Error,
            Message => To_Bounded_String ("Not positive"));
      end To_Validation_Error;

      function Validate is new Int_Result.Ensure
        (Pred     => Is_Positive,
         To_Error => To_Validation_Error);

      R      : constant Int_Res := Int_Result.Ok (-5);
      Result : constant Int_Res := Validate (R);
   begin
      Assert (Int_Result.Is_Error (Result),
              "Ensure converts to Error if predicate fails");
      Assert (Int_Result.Error_Info (Result).Kind = Validation_Error,
              "Ensure creates correct error kind");
   end;

   --  Ensure on Error leaves Error unchanged
   declare
      function Is_Positive (X : Integer) return Boolean is (X > 0);

      function To_Validation_Error (X : Integer) return Error_Type is
         pragma Unreferenced (X);
      begin
         return
           (Kind    => Validation_Error,
            Message => To_Bounded_String ("Not positive"));
      end To_Validation_Error;

      function Validate is new Int_Result.Ensure
        (Pred     => Is_Positive,
         To_Error => To_Validation_Error);

      R      : constant Int_Res := Int_Result.Error (IO_Error, "original");
      Result : constant Int_Res := Validate (R);
   begin
      Assert (Int_Result.Is_Error (Result), "Ensure leaves Error unchanged");
      Assert (Int_Result.Error_Info (Result).Kind = IO_Error,
              "Ensure preserves original error kind");
   end;

   Put_Line ("Test: All Error Kinds");
   --  Test each error kind can be created and retrieved
   declare
      E1 : constant Int_Res := Int_Result.Error (Validation_Error, "val");
      E2 : constant Int_Res := Int_Result.Error (Parse_Error, "parse");
      E3 : constant Int_Res := Int_Result.Error (Not_Found_Error, "not found");
      E4 : constant Int_Res := Int_Result.Error (IO_Error, "io");
      E5 : constant Int_Res := Int_Result.Error (Resource_Error, "resource");
      E6 : constant Int_Res := Int_Result.Error (Internal_Error, "internal");
   begin
      Assert (Int_Result.Error_Info (E1).Kind = Validation_Error,
              "Validation_Error kind correct");
      Assert (Int_Result.Error_Info (E2).Kind = Parse_Error,
              "Parse_Error kind correct");
      Assert (Int_Result.Error_Info (E3).Kind = Not_Found_Error,
              "Not_Found_Error kind correct");
      Assert (Int_Result.Error_Info (E4).Kind = IO_Error,
              "IO_Error kind correct");
      Assert (Int_Result.Error_Info (E5).Kind = Resource_Error,
              "Resource_Error kind correct");
      Assert (Int_Result.Error_Info (E6).Kind = Internal_Error,
              "Internal_Error kind correct");
   end;

   Put_Line ("Test: Unwrap_Or - Extract or Default");
   --  Unwrap_Or on Ok returns value
   declare
      R      : constant Int_Res := Int_Result.Ok (42);
      Result : constant Integer := Int_Result.Unwrap_Or (R, Default => 0);
   begin
      Assert (Result = 42, "Unwrap_Or on Ok returns value");
   end;

   --  Unwrap_Or on Error returns default
   declare
      R      : constant Int_Res := Int_Result.Error (IO_Error, "failed");
      Result : constant Integer := Int_Result.Unwrap_Or (R, Default => -1);
   begin
      Assert (Result = -1, "Unwrap_Or on Error returns default");
   end;

   Put_Line ("Test: Unwrap_Or_With - Lazy Default");
   --  Unwrap_Or_With on Ok returns value
   declare
      function Compute_Default return Integer is (999);
      function Extract is new Int_Result.Unwrap_Or_With (F => Compute_Default);
      R      : constant Int_Res := Int_Result.Ok (77);
      Result : constant Integer := Extract (R);
   begin
      Assert (Result = 77, "Unwrap_Or_With on Ok returns value");
   end;

   --  Unwrap_Or_With on Error calls default function
   declare
      function Compute_Default return Integer is (999);
      function Extract is new Int_Result.Unwrap_Or_With (F => Compute_Default);
      R      : constant Int_Res := Int_Result.Error (IO_Error, "failed");
      Result : constant Integer := Extract (R);
   begin
      Assert (Result = 999, "Unwrap_Or_With on Error returns default");
   end;

   Put_Line ("Test: Map - Transform Ok Value");
   --  Map on Ok transforms value
   declare
      function Triple (X : Integer) return Integer is (X * 3);
      function Do_Map is new Int_Result.Map (F => Triple);
      R      : constant Int_Res := Int_Result.Ok (10);
      Result : constant Int_Res := Do_Map (R);
   begin
      Assert (Int_Result.Is_Ok (Result), "Map on Ok returns Ok");
      Assert (Int_Result.Value (Result) = 30, "Map transforms value");
   end;

   --  Map on Error returns Error unchanged
   declare
      function Triple (X : Integer) return Integer is (X * 3);
      function Do_Map is new Int_Result.Map (F => Triple);
      R      : constant Int_Res := Int_Result.Error (IO_Error, "err");
      Result : constant Int_Res := Do_Map (R);
   begin
      Assert (Int_Result.Is_Error (Result), "Map on Error returns Error");
      Assert (Int_Result.Error_Info (Result).Kind = IO_Error,
              "Map preserves error kind");
   end;

   Put_Line ("Test: Fallback - Eager Alternative");
   --  Fallback with Ok first returns first
   declare
      A      : constant Int_Res := Int_Result.Ok (1);
      B      : constant Int_Res := Int_Result.Ok (2);
      Result : constant Int_Res := Int_Result.Fallback (A, B);
   begin
      Assert (Int_Result.Is_Ok (Result), "Fallback Ok|Ok returns Ok");
      Assert (Int_Result.Value (Result) = 1, "Fallback returns first Ok");
   end;

   --  Fallback with Error first returns second
   declare
      A      : constant Int_Res := Int_Result.Error (IO_Error, "first err");
      B      : constant Int_Res := Int_Result.Ok (2);
      Result : constant Int_Res := Int_Result.Fallback (A, B);
   begin
      Assert (Int_Result.Is_Ok (Result), "Fallback Err|Ok returns Ok");
      Assert (Int_Result.Value (Result) = 2, "Fallback returns second Ok");
   end;

   --  Fallback with both Error returns second Error
   declare
      A      : constant Int_Res := Int_Result.Error (IO_Error, "first");
      B      : constant Int_Res := Int_Result.Error (Parse_Error, "second");
      Result : constant Int_Res := Int_Result.Fallback (A, B);
   begin
      Assert (Int_Result.Is_Error (Result), "Fallback Err|Err returns Error");
      Assert (Int_Result.Error_Info (Result).Kind = Parse_Error,
              "Fallback returns second error kind");
   end;

   Put_Line ("Test: Fallback_With - Lazy Alternative");
   --  Fallback_With on Ok doesn't call fallback function
   declare
      function Compute_Fallback return Int_Res is
        (Int_Result.Ok (999));
      function Try_Fallback is new Int_Result.Fallback_With
        (F => Compute_Fallback);
      R      : constant Int_Res := Int_Result.Ok (1);
      Result : constant Int_Res := Try_Fallback (R);
   begin
      Assert (Int_Result.Is_Ok (Result), "Fallback_With on Ok returns Ok");
      Assert (Int_Result.Value (Result) = 1,
              "Fallback_With on Ok returns original value");
   end;

   --  Fallback_With on Error calls fallback function
   declare
      function Compute_Fallback return Int_Res is
        (Int_Result.Ok (999));
      function Try_Fallback is new Int_Result.Fallback_With
        (F => Compute_Fallback);
      R      : constant Int_Res := Int_Result.Error (IO_Error, "failed");
      Result : constant Int_Res := Try_Fallback (R);
   begin
      Assert (Int_Result.Is_Ok (Result),
              "Fallback_With on Error can return Ok");
      Assert (Int_Result.Value (Result) = 999,
              "Fallback_With returns fallback value");
   end;

   Put_Line ("Test: Recover - Turn Error into Value");
   --  Recover on Ok returns value
   declare
      function Handle_Error (E : Error_Type) return Integer is
         pragma Unreferenced (E);
      begin
         return -1;
      end Handle_Error;

      function Do_Recover is new Int_Result.Recover (Handle => Handle_Error);
      R      : constant Int_Res := Int_Result.Ok (42);
      Result : constant Integer := Do_Recover (R);
   begin
      Assert (Result = 42, "Recover on Ok returns original value");
   end;

   --  Recover on Error returns handler result
   declare
      function Handle_Error (E : Error_Type) return Integer is
         pragma Unreferenced (E);
      begin
         return -1;
      end Handle_Error;

      function Do_Recover is new Int_Result.Recover (Handle => Handle_Error);
      R      : constant Int_Res := Int_Result.Error (IO_Error, "failed");
      Result : constant Integer := Do_Recover (R);
   begin
      Assert (Result = -1, "Recover on Error returns handler value");
   end;

   Put_Line ("Test: Recover_With - Turn Error into Result");
   --  Recover_With on Ok returns Ok
   declare
      function Handle_Error (E : Error_Type) return Int_Res is
         pragma Unreferenced (E);
      begin
         return Int_Result.Ok (-1);
      end Handle_Error;

      function Do_Recover is new Int_Result.Recover_With
        (Handle => Handle_Error);
      R      : constant Int_Res := Int_Result.Ok (42);
      Result : constant Int_Res := Do_Recover (R);
   begin
      Assert (Int_Result.Is_Ok (Result), "Recover_With on Ok returns Ok");
      Assert (Int_Result.Value (Result) = 42,
              "Recover_With preserves original value");
   end;

   --  Recover_With on Error returns handler result
   declare
      function Handle_Error (E : Error_Type) return Int_Res is
         pragma Unreferenced (E);
      begin
         return Int_Result.Ok (-1);
      end Handle_Error;

      function Do_Recover is new Int_Result.Recover_With
        (Handle => Handle_Error);
      R      : constant Int_Res := Int_Result.Error (IO_Error, "failed");
      Result : constant Int_Res := Do_Recover (R);
   begin
      Assert (Int_Result.Is_Ok (Result), "Recover_With on Error returns Ok");
      Assert (Int_Result.Value (Result) = -1,
              "Recover_With returns handler result");
   end;

   Put_Line ("Test: Tap - Side Effects without Changing Result");
   --  Tap on Ok calls On_Ok procedure
   declare
      Ok_Called  : Boolean := False;
      Err_Called : Boolean := False;

      procedure On_Ok_Call (V : Integer) is
         pragma Unreferenced (V);
      begin
         Ok_Called := True;
      end On_Ok_Call;

      procedure On_Err_Call (E : Error_Type) is
         pragma Unreferenced (E);
      begin
         Err_Called := True;
      end On_Err_Call;

      function Do_Tap is new Int_Result.Tap
        (On_Ok  => On_Ok_Call,
         On_Err => On_Err_Call);

      R      : constant Int_Res := Int_Result.Ok (42);
      Result : constant Int_Res := Do_Tap (R);
   begin
      Assert (Int_Result.Is_Ok (Result), "Tap returns same Result");
      Assert (Int_Result.Value (Result) = 42, "Tap preserves value");
      Assert (Ok_Called, "Tap calls On_Ok for Ok result");
      Assert (not Err_Called, "Tap does not call On_Err for Ok result");
   end;

   --  Tap on Error calls On_Err procedure
   declare
      Ok_Called  : Boolean := False;
      Err_Called : Boolean := False;

      procedure On_Ok_Call (V : Integer) is
         pragma Unreferenced (V);
      begin
         Ok_Called := True;
      end On_Ok_Call;

      procedure On_Err_Call (E : Error_Type) is
         pragma Unreferenced (E);
      begin
         Err_Called := True;
      end On_Err_Call;

      function Do_Tap is new Int_Result.Tap
        (On_Ok  => On_Ok_Call,
         On_Err => On_Err_Call);

      R      : constant Int_Res := Int_Result.Error (IO_Error, "failed");
      Result : constant Int_Res := Do_Tap (R);
   begin
      Assert (Int_Result.Is_Error (Result), "Tap returns same Error");
      Assert (not Ok_Called, "Tap does not call On_Ok for Error result");
      Assert (Err_Called, "Tap calls On_Err for Error result");
   end;

   --  Register results with test framework
   Test_Framework.Register_Results (Test_Count, Pass_Count);

end Test_Result_Combinators;
