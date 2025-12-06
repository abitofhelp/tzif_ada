pragma Ada_2022;
--  ======================================================================
--  Test_Option_Combinators
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for TZif.Domain.Types.Option generic package.
--    Tests all combinator operations to achieve full coverage of
--    functional operations: Map, And_Then, Filter, Or_Else, etc.
--  ======================================================================

with Ada.Text_IO; use Ada.Text_IO;
with Test_Framework;
with TZif.Domain.Types.Option;

procedure Test_Option_Combinators is

   --  Instantiate Option with Integer for testing
   package Int_Option is new TZif.Domain.Types.Option (T => Integer);
   use Int_Option;

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
   --  Helper functions for generic instantiations
   --  =====================================================================

   --  For Map: doubles the value
   function Double (X : Integer) return Integer is (X * 2);

   --  For And_Then: returns Some if positive, None otherwise
   function Positive_Only (X : Integer) return Option is
     (if X > 0 then New_Some (X) else None);

   --  For Filter: predicate that checks if value is even
   function Is_Even (X : Integer) return Boolean is (X mod 2 = 0);

   --  For Filter: predicate that checks if value is positive
   function Is_Positive (X : Integer) return Boolean is (X > 0);

   --  For Unwrap_Or_With / Or_Else_With: lazy fallback provider
   function Get_Default return Integer is (999);

   function Get_Fallback_Option return Option is (New_Some (888));

   --  Instantiate generic functions
   function Map_Double is new Map (F => Double);
   function Bind_Positive is new And_Then (F => Positive_Only);
   function Filter_Even is new Filter (Pred => Is_Even);
   function Filter_Positive is new Filter (Pred => Is_Positive);
   function Unwrap_Default is new Unwrap_Or_With (F => Get_Default);
   function Fallback_Option is new Or_Else_With (F => Get_Fallback_Option);

begin
   Put_Line ("Test: Option Constructors and Predicates");
   --  Basic constructor tests
   declare
      S : constant Option := New_Some (42);
      N : constant Option := None;
   begin
      Assert (Is_Some (S), "New_Some creates Some");
      Assert (not Is_None (S), "Some is not None");
      Assert (Value (S) = 42, "Value extracts correct value");

      Assert (Is_None (N), "None creates None");
      Assert (not Is_Some (N), "None is not Some");
   end;

   Put_Line ("Test: Unwrap_Or - Eager Default");
   --  Unwrap_Or with Some returns value
   declare
      S : constant Option := New_Some (10);
   begin
      Assert (Unwrap_Or (S, 0) = 10, "Unwrap_Or on Some returns value");
   end;

   --  Unwrap_Or with None returns default
   declare
      N : constant Option := None;
   begin
      Assert (Unwrap_Or (N, 42) = 42, "Unwrap_Or on None returns default");
   end;

   Put_Line ("Test: Unwrap_Or_With - Lazy Default");
   --  Unwrap_Or_With with Some returns value (doesn't call F)
   declare
      S : constant Option := New_Some (10);
   begin
      Assert (Unwrap_Default (S) = 10,
              "Unwrap_Or_With on Some returns value");
   end;

   --  Unwrap_Or_With with None calls F
   declare
      N : constant Option := None;
   begin
      Assert (Unwrap_Default (N) = 999,
              "Unwrap_Or_With on None calls fallback function");
   end;

   Put_Line ("Test: Map - Transform Some Values");
   --  Map on Some applies function
   declare
      S      : constant Option := New_Some (5);
      Result : constant Option := Map_Double (S);
   begin
      Assert (Is_Some (Result), "Map on Some returns Some");
      Assert (Value (Result) = 10, "Map applies function correctly");
   end;

   --  Map on None returns None
   declare
      N      : constant Option := None;
      Result : constant Option := Map_Double (N);
   begin
      Assert (Is_None (Result), "Map on None returns None");
   end;

   Put_Line ("Test: And_Then (Monadic Bind) - Chain Operations");
   --  And_Then on Some with function returning Some
   declare
      S      : constant Option := New_Some (5);
      Result : constant Option := Bind_Positive (S);
   begin
      Assert (Is_Some (Result), "And_Then on positive Some returns Some");
      Assert (Value (Result) = 5, "And_Then preserves value");
   end;

   --  And_Then on Some with function returning None
   declare
      S      : constant Option := New_Some (-5);
      Result : constant Option := Bind_Positive (S);
   begin
      Assert (Is_None (Result),
              "And_Then on negative Some returns None (function result)");
   end;

   --  And_Then on None returns None (short-circuits)
   declare
      N      : constant Option := None;
      Result : constant Option := Bind_Positive (N);
   begin
      Assert (Is_None (Result), "And_Then on None returns None");
   end;

   Put_Line ("Test: Filter - Conditional Retention");
   --  Filter on Some where predicate is true
   declare
      S      : constant Option := New_Some (4);
      Result : constant Option := Filter_Even (S);
   begin
      Assert (Is_Some (Result), "Filter keeps Some when predicate true");
      Assert (Value (Result) = 4, "Filter preserves value");
   end;

   --  Filter on Some where predicate is false
   declare
      S      : constant Option := New_Some (5);
      Result : constant Option := Filter_Even (S);
   begin
      Assert (Is_None (Result), "Filter returns None when predicate false");
   end;

   --  Filter on None returns None
   declare
      N      : constant Option := None;
      Result : constant Option := Filter_Even (N);
   begin
      Assert (Is_None (Result), "Filter on None returns None");
   end;

   --  Additional filter test with different predicate
   declare
      Pos    : constant Option := New_Some (10);
      Neg    : constant Option := New_Some (-10);
   begin
      Assert (Is_Some (Filter_Positive (Pos)),
              "Filter_Positive keeps positive values");
      Assert (Is_None (Filter_Positive (Neg)),
              "Filter_Positive rejects negative values");
   end;

   Put_Line ("Test: Or_Else - Eager Fallback");
   --  Or_Else: Some | _ returns first
   declare
      A      : constant Option := New_Some (1);
      B      : constant Option := New_Some (2);
      Result : constant Option := Or_Else (A, B);
   begin
      Assert (Is_Some (Result), "Or_Else with Some first returns Some");
      Assert (Value (Result) = 1, "Or_Else returns first Some value");
   end;

   --  Or_Else: Some | None returns first
   declare
      A      : constant Option := New_Some (1);
      B      : constant Option := None;
      Result : constant Option := Or_Else (A, B);
   begin
      Assert (Is_Some (Result), "Or_Else Some|None returns Some");
      Assert (Value (Result) = 1, "Or_Else returns first value");
   end;

   --  Or_Else: None | Some returns second
   declare
      A      : constant Option := None;
      B      : constant Option := New_Some (2);
      Result : constant Option := Or_Else (A, B);
   begin
      Assert (Is_Some (Result), "Or_Else None|Some returns Some");
      Assert (Value (Result) = 2, "Or_Else returns fallback value");
   end;

   --  Or_Else: None | None returns None
   declare
      A      : constant Option := None;
      B      : constant Option := None;
      Result : constant Option := Or_Else (A, B);
   begin
      Assert (Is_None (Result), "Or_Else None|None returns None");
   end;

   --  Test Fallback alias
   declare
      A      : constant Option := None;
      B      : constant Option := New_Some (99);
      Result : constant Option := Fallback (A, B);
   begin
      Assert (Is_Some (Result), "Fallback alias works correctly");
      Assert (Value (Result) = 99, "Fallback returns correct value");
   end;

   Put_Line ("Test: Or_Else_With - Lazy Fallback");
   --  Or_Else_With on Some doesn't call fallback function
   declare
      S      : constant Option := New_Some (1);
      Result : constant Option := Fallback_Option (S);
   begin
      Assert (Is_Some (Result), "Or_Else_With on Some returns Some");
      Assert (Value (Result) = 1,
              "Or_Else_With on Some preserves original value");
   end;

   --  Or_Else_With on None calls fallback function
   declare
      N      : constant Option := None;
      Result : constant Option := Fallback_Option (N);
   begin
      Assert (Is_Some (Result), "Or_Else_With on None returns fallback");
      Assert (Value (Result) = 888,
              "Or_Else_With on None returns fallback value");
   end;

   Put_Line ("Test: Chained Operations");
   --  Chain multiple operations: Map then Filter
   declare
      S       : constant Option := New_Some (3);
      Doubled : constant Option := Map_Double (S);
      Filtered : constant Option := Filter_Even (Doubled);
   begin
      Assert (Is_Some (Filtered), "Chained Map->Filter works on Some");
      Assert (Value (Filtered) = 6,
              "Chained operations produce correct value");
   end;

   --  Chain starting from None
   declare
      N       : constant Option := None;
      Doubled : constant Option := Map_Double (N);
      Filtered : constant Option := Filter_Even (Doubled);
   begin
      Assert (Is_None (Filtered), "Chained operations propagate None");
   end;

   --  Chain with Or_Else recovery
   declare
      N        : constant Option := None;
      Mapped   : constant Option := Map_Double (N);
      Recovered : constant Option := Or_Else (Mapped, New_Some (100));
   begin
      Assert (Is_Some (Recovered), "Or_Else recovers from None in chain");
      Assert (Value (Recovered) = 100, "Recovery provides fallback value");
   end;

   --  Register results with test framework
   Test_Framework.Register_Results (Test_Count, Pass_Count);

end Test_Option_Combinators;
