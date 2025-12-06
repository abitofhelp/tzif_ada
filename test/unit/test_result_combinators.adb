pragma Ada_2022;
--  ======================================================================
--  Test_Result_Combinators
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Unit tests for TZif.Domain.Error.Result generic package.
--    Tests all combinator operations to achieve full coverage of
--    functional operations: And_Then, Map_Error, With_Context.
--
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

   --  Register results with test framework
   Test_Framework.Register_Results (Test_Count, Pass_Count);

end Test_Result_Combinators;
