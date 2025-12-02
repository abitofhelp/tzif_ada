pragma Ada_2022;
pragma Unevaluated_Use_Of_Old (Allow);
--  ======================================================================
--  Test_Bounded_Vector
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Unit tests for TZif.Domain.Bounded_Vector generic package.
--    Tests all public operations including Option/Result error handling.
--
--  ======================================================================

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Test_Framework;
with TZif.Domain.Bounded_Vector;

procedure Test_Bounded_Vector is

   --  Instantiate bounded vector with Integer elements and small capacity
   --  Small capacity (5) makes it easy to test boundary conditions
   package Int_Vectors is new TZif.Domain.Bounded_Vector
     (Element_Type => Integer, Capacity => 5);

   use Int_Vectors;

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
   Put_Line ("Test: Empty Vector Creation");
   --  Test Empty_Vector
   declare
      V : constant Vector := Empty_Vector;
   begin
      Assert (Is_Empty (V), "Empty_Vector is empty");
      Assert (not Is_Full (V), "Empty_Vector is not full");
      Assert (Length (V) = 0, "Empty_Vector has length 0");
      Assert (Last_Index (V) = 0, "Empty_Vector has last_index 0");
      Assert (Max_Capacity = 5, "Max_Capacity returns correct value");
      Assert (First_Index = 1, "First_Index is 1");
   end;

   Put_Line ("Test: Append Operations (Result-based)");
   --  Test Append with Result
   declare
      V : Vector := Empty_Vector;
      R : Unit_Result;
   begin
      --  Append first element
      Append (V, 10, R);
      Assert (Is_Ok (R), "First append succeeds");
      Assert (Length (V) = 1, "Length is 1 after first append");
      Assert (not Is_Empty (V), "Vector is not empty after append");

      --  Append more elements
      Append (V, 20, R);
      Assert (Is_Ok (R), "Second append succeeds");
      Append (V, 30, R);
      Assert (Is_Ok (R), "Third append succeeds");
      Append (V, 40, R);
      Assert (Is_Ok (R), "Fourth append succeeds");
      Append (V, 50, R);
      Assert (Is_Ok (R), "Fifth append succeeds");

      Assert (Length (V) = 5, "Length is 5 after filling");
      Assert (Is_Full (V), "Vector is full at capacity");

      --  Append when full should fail
      Append (V, 60, R);
      Assert (Is_Err (R), "Append when full returns error");
      Assert (Error (R).Kind = Vector_Full, "Error kind is Vector_Full");
      Assert (Length (V) = 5, "Length unchanged after failed append");
   end;

   Put_Line ("Test: Element Access (Option-based)");
   --  Test Element, First_Element, Last_Element
   declare
      V   : Vector := Empty_Vector;
      R   : Unit_Result;
      Opt : Element_Option;
   begin
      --  Access on empty vector returns None
      Opt := First_Element (V);
      Assert (Is_None (Opt), "First_Element on empty returns None");

      Opt := Last_Element (V);
      Assert (Is_None (Opt), "Last_Element on empty returns None");

      Opt := Element (V, 1);
      Assert (Is_None (Opt), "Element(1) on empty returns None");

      --  Add some elements
      Append (V, 100, R);
      Append (V, 200, R);
      Append (V, 300, R);

      --  Valid access returns Some
      Opt := First_Element (V);
      Assert (Is_Some (Opt), "First_Element returns Some");
      Assert (Value (Opt) = 100, "First element is 100");

      Opt := Last_Element (V);
      Assert (Is_Some (Opt), "Last_Element returns Some");
      Assert (Value (Opt) = 300, "Last element is 300");

      Opt := Element (V, 2);
      Assert (Is_Some (Opt), "Element(2) returns Some");
      Assert (Value (Opt) = 200, "Element at index 2 is 200");

      --  Out of bounds returns None
      Opt := Element (V, 4);
      Assert (Is_None (Opt), "Element(4) returns None (out of bounds)");

      Opt := Element (V, 5);
      Assert (Is_None (Opt), "Element(5) returns None (out of bounds)");
   end;

   Put_Line ("Test: Unchecked Element Access");
   --  Test Unchecked_Element, Unchecked_First, Unchecked_Last
   declare
      V : Vector := Empty_Vector;
      R : Unit_Result;
   begin
      Append (V, 11, R);
      Append (V, 22, R);
      Append (V, 33, R);

      Assert (Unchecked_First (V) = 11, "Unchecked_First returns first");
      Assert (Unchecked_Last (V) = 33, "Unchecked_Last returns last");
      Assert
        (Unchecked_Element (V, 2) = 22, "Unchecked_Element correct");
   end;

   Put_Line ("Test: Delete_Last Operation");
   --  Test Delete_Last with Result
   declare
      V : Vector := Empty_Vector;
      R : Unit_Result;
   begin
      --  Delete from empty should fail
      Delete_Last (V, R);
      Assert (Is_Err (R), "Delete_Last on empty returns error");
      Assert (Error (R).Kind = Vector_Empty, "Error kind is Vector_Empty");

      --  Add elements then delete
      Append (V, 1, R);
      Append (V, 2, R);
      Append (V, 3, R);
      Assert (Length (V) = 3, "Vector has 3 elements");

      Delete_Last (V, R);
      Assert (Is_Ok (R), "Delete_Last succeeds");
      Assert (Length (V) = 2, "Length is 2 after delete");
      Assert (Unchecked_Last (V) = 2, "Last element is now 2");

      Delete_Last (V, R);
      Delete_Last (V, R);
      Assert (Is_Empty (V), "Vector is empty after all deletes");

      Delete_Last (V, R);
      Assert (Is_Err (R), "Delete_Last on empty after clearing returns error");
   end;

   Put_Line ("Test: Replace_Element Operation");
   --  Test Replace_Element with Result
   declare
      V : Vector := Empty_Vector;
      R : Unit_Result;
   begin
      Append (V, 10, R);
      Append (V, 20, R);
      Append (V, 30, R);

      --  Valid replace
      Replace_Element (V, 2, 99, R);
      Assert (Is_Ok (R), "Replace_Element at valid index succeeds");
      Assert (Unchecked_Element (V, 2) = 99, "Element at index 2 is now 99");
      Assert (Length (V) = 3, "Length unchanged after replace");

      --  Replace at boundary
      Replace_Element (V, 1, 88, R);
      Assert (Is_Ok (R), "Replace at index 1 succeeds");
      Assert (Unchecked_First (V) = 88, "First element is now 88");

      Replace_Element (V, 3, 77, R);
      Assert (Is_Ok (R), "Replace at last index succeeds");
      Assert (Unchecked_Last (V) = 77, "Last element is now 77");

      --  Out of bounds replace should fail
      Replace_Element (V, 4, 66, R);
      Assert (Is_Err (R), "Replace at index 4 returns error (out of bounds)");
      Assert
        (Error (R).Kind = Index_Out_Of_Bounds,
         "Error kind is Index_Out_Of_Bounds");
   end;

   Put_Line ("Test: Clear Operation");
   --  Test Clear
   declare
      V : Vector := Empty_Vector;
      R : Unit_Result;
   begin
      Append (V, 1, R);
      Append (V, 2, R);
      Append (V, 3, R);
      Assert (Length (V) = 3, "Vector has 3 elements before clear");

      Clear (V);
      Assert (Is_Empty (V), "Vector is empty after clear");
      Assert (Length (V) = 0, "Length is 0 after clear");
      Assert (Last_Index (V) = 0, "Last_Index is 0 after clear");

      --  Clear on empty is OK
      Clear (V);
      Assert (Is_Empty (V), "Clear on empty vector is idempotent");
   end;

   Put_Line ("Test: Unchecked Operations");
   --  Test Unchecked_Append, Unchecked_Delete_Last, Unchecked_Replace
   declare
      V : Vector := Empty_Vector;
   begin
      --  Unchecked_Append
      Unchecked_Append (V, 111);
      Unchecked_Append (V, 222);
      Assert (Length (V) = 2, "Unchecked_Append adds elements");
      Assert (Unchecked_Last (V) = 222, "Last element after unchecked append");

      --  Unchecked_Replace
      Unchecked_Replace (V, 1, 999);
      Assert (Unchecked_First (V) = 999, "Unchecked_Replace modifies element");

      --  Unchecked_Delete_Last
      Unchecked_Delete_Last (V);
      Assert (Length (V) = 1, "Unchecked_Delete_Last removes element");
      Assert (Unchecked_Last (V) = 999, "Last element after delete");
   end;

   Put_Line ("Test: Index Types and Boundaries");
   --  Test Index_Type, Count_Type, Extended_Index
   declare
      V : Vector := Empty_Vector;
      R : Unit_Result;
      I : Extended_Index;
      C : Count_Type;
   begin
      I := Last_Index (V);
      Assert (I = 0, "Last_Index on empty is Extended_Index 0");

      Append (V, 42, R);
      I := Last_Index (V);
      Assert (I = 1, "Last_Index after one append is 1");

      C := Length (V);
      Assert (C = 1, "Length returns Count_Type");

      --  Fill to capacity
      Append (V, 1, R);
      Append (V, 2, R);
      Append (V, 3, R);
      Append (V, 4, R);

      Assert (Length (V) = 5, "Vector at capacity");
      Assert (Last_Index (V) = 5, "Last_Index at capacity is 5");
   end;

   Put_Line ("Test: Option New_Some and None Constructors");
   --  Test explicit Option construction (matches Functional.Option pattern)
   declare
      Opt_None : constant Element_Option := None;
      Opt_Some : constant Element_Option := New_Some (42);
   begin
      Assert (Is_None (Opt_None), "None constructor creates K_None");
      Assert (not Is_Some (Opt_None), "None is not K_Some");

      Assert (Is_Some (Opt_Some), "New_Some constructor creates K_Some");
      Assert (not Is_None (Opt_Some), "New_Some is not K_None");
      Assert (Value (Opt_Some) = 42, "Value returns value from New_Some");
   end;

   Put_Line ("Test: Result Ok and Err Constructors");
   --  Test explicit Result construction (matches Functional.Result pattern)
   declare
      R_Ok  : constant Unit_Result := Ok;
      R_Err : constant Unit_Result := Err (Vector_Full);
   begin
      Assert (Is_Ok (R_Ok), "Ok constructor creates K_Ok result");
      Assert (not Is_Err (R_Ok), "Ok is not K_Err");

      Assert (Is_Err (R_Err), "Err constructor creates K_Err result");
      Assert (not Is_Ok (R_Err), "Err is not K_Ok");
      Assert (Error (R_Err).Kind = Vector_Full, "Error returns error value");
   end;

   --  Summary
   Put_Line ("====================================================");
   Put_Line
     ("  Results:" & Pass_Count'Image & " /" & Test_Count'Image & " passed");
   if Pass_Count = Test_Count then
      Put_Line ("  Status: ALL TESTS PASSED");
   else
      Put_Line ("  Status: FAILURES DETECTED");
   end if;
   Put_Line ("====================================================");

   Test_Framework.Register_Results (Test_Count, Pass_Count);

   if Pass_Count /= Test_Count then
      Ada.Command_Line.Set_Exit_Status (1);
   end if;

end Test_Bounded_Vector;
