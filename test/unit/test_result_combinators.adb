pragma Ada_2022;
--  ======================================================================
--  Test_Result_Combinators
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for TZif.Domain.Error.Result generic package.
--    Tests essential operations: constructors, predicates, extractors.
--
--  Note:
--    Advanced combinators (Map, And_Then, Fallback, Recover, etc.) are
--    available in Functional.Result and tested in the functional crate.
--    This domain Result provides only minimal essential operations for
--    SPARK compatibility.
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

   Put_Line ("Test: Value Extraction Edge Cases");
   --  Test value extraction with different values
   declare
      R_Zero     : constant Int_Res := Int_Result.Ok (0);
      R_Negative : constant Int_Res := Int_Result.Ok (-100);
      R_Max      : constant Int_Res := Int_Result.Ok (Integer'Last);
      R_Min      : constant Int_Res := Int_Result.Ok (Integer'First);
   begin
      Assert (Int_Result.Value (R_Zero) = 0, "Value extracts zero");
      Assert (Int_Result.Value (R_Negative) = -100, "Value extracts negative");
      Assert (Int_Result.Value (R_Max) = Integer'Last, "Value extracts max");
      Assert (Int_Result.Value (R_Min) = Integer'First, "Value extracts min");
   end;

   Put_Line ("Test: Error Message Extraction");
   --  Test error message preservation
   declare
      Long_Msg : constant String := "This is a longer error message with info";
      R        : constant Int_Res := Int_Result.Error (IO_Error, Long_Msg);
   begin
      Assert (To_String (Int_Result.Error_Info (R).Message) = Long_Msg,
              "Long error message preserved");
   end;

   --  Register results with test framework
   Test_Framework.Register_Results (Test_Count, Pass_Count);

end Test_Result_Combinators;
