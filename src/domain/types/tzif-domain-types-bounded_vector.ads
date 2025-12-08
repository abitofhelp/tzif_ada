pragma Ada_2022;
pragma Unevaluated_Use_Of_Old (Allow);
--  ===========================================================================
--  TZif.Domain.Types.Bounded_Vector
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    SPARK-friendly bounded vector container with functional error handling.
--    Provides a fixed-capacity, stack-allocated vector that never uses the
--    heap, making it suitable for embedded systems and safety-critical code.
--
--  Design:
--    - API modeled after Ada.Containers.Bounded_Vectors
--    - Fallible operations return Result types (no exceptions)
--    - Element access returns Option types (explicit absence handling)
--    - Self-contained error types (zero external dependencies)
--    - SPARK_Mode enabled for formal verification
--    - No dynamic memory allocation
--
--  Key Types:
--    Vector          - The bounded vector container
--    Element_Option  - Option monad for element access
--    Unit_Result     - Result monad for modification operations
--    Error_Kind      - Enumeration of possible error conditions
--
--  Usage:
--    package Transition_Vectors is new TZif.Domain.Types.Bounded_Vector
--      (Element_Type => Transition_Type, Capacity => 2000);
--
--    V : Transition_Vectors.Vector := Transition_Vectors.Empty_Vector;
--    R : Transition_Vectors.Unit_Result;
--
--    Transition_Vectors.Append (V, My_Transition, R);
--    if R.Is_Ok then
--       -- Success
--    else
--       -- Handle R.Error
--    end if;
--
--  Dependencies:
--    None (self-contained for domain layer purity)
--
--  ===========================================================================

generic
   type Element_Type is private;
   Capacity : Positive;
   --  Default_Value provides initialization for internal storage.
   --  Required for SPARK compatibility (no box notation allowed).
   with function Default_Value return Element_Type;
package TZif.Domain.Types.Bounded_Vector with
  SPARK_Mode => On,
  Pure
is

   --  ========================================================================
   --  Index and Count Types
   --  ========================================================================

   --  Valid indices for element access (1-based)
   subtype Index_Type is Positive range 1 .. Capacity;

   --  Count of elements (0 to Capacity)
   subtype Count_Type is Natural range 0 .. Capacity;

   --  Extended index including "no index" value (0)
   subtype Extended_Index is Natural range 0 .. Capacity;

   --  ========================================================================
   --  Error Types (Self-Contained)
   --  ========================================================================

   --  Categories of errors that can occur during vector operations
   type Error_Kind is
     (Index_Out_Of_Bounds,  --  Index exceeds current length
      Vector_Full,          --  Cannot append: at capacity
      Vector_Empty);        --  Cannot access/remove: no elements

   --  Error record with kind identifier
   type Error_Type is record
      Kind : Error_Kind;
   end record;

   --  ========================================================================
   --  Option Type for Element Access (matches Functional.Option pattern)
   --  ========================================================================
   --  Used for operations that may not return a value (element lookup).
   --  Caller must check Kind before accessing Value.
   --  ========================================================================

   type Option_Kind is (K_Some, K_None);

   type Element_Option (Kind : Option_Kind := K_None) is record
      case Kind is
         when K_Some =>
            Value : Element_Type;
         when K_None =>
            null;
      end case;
   end record;

   --  Option constructors
   function New_Some (V : Element_Type) return Element_Option is
     ((Kind => K_Some, Value => V)) with
     Inline;

   function None return Element_Option is
     ((Kind => K_None)) with
     Inline;

   --  Option predicates
   function Is_Some (O : Element_Option) return Boolean is
     (O.Kind = K_Some) with
     Inline;

   function Is_None (O : Element_Option) return Boolean is
     (O.Kind = K_None) with
     Inline;

   --  Option value extraction (caller must check Is_Some first)
   function Value (O : Element_Option) return Element_Type is
     (O.Value) with
     Pre => O.Kind = K_Some, Inline;

   --  ========================================================================
   --  Result Type for Modification Operations (matches Functional.Result)
   --  ========================================================================
   --  Used for operations that can fail (append, delete, replace).
   --  Railway-oriented programming: operations return success or error.
   --  ========================================================================

   type Result_Kind is (K_Ok, K_Err);

   type Unit_Result (Kind : Result_Kind := K_Err) is record
      case Kind is
         when K_Ok =>
            null;
         when K_Err =>
            Err_Value : Error_Type;
      end case;
   end record;

   --  Result constructors
   function Ok return Unit_Result is
     ((Kind => K_Ok)) with
     Inline;

   function Err (E : Error_Kind) return Unit_Result is
     ((Kind => K_Err, Err_Value => (Kind => E))) with
     Inline;

   --  Result predicates
   function Is_Ok (R : Unit_Result) return Boolean is
     (R.Kind = K_Ok) with
     Inline;

   function Is_Err (R : Unit_Result) return Boolean is
     (R.Kind = K_Err) with
     Inline;

   --  Error extraction (caller must check Is_Err first)
   function Error (R : Unit_Result) return Error_Type is
     (R.Err_Value) with
     Pre => R.Kind = K_Err, Inline;

   --  ========================================================================
   --  Vector Type
   --  ========================================================================

   type Vector is private with
     Default_Initial_Condition => Is_Empty (Vector);

   --  ========================================================================
   --  Constants
   --  ========================================================================

   function Empty_Vector return Vector with
     Post => Is_Empty (Empty_Vector'Result);

   --  ========================================================================
   --  Query Functions (Always Succeed)
   --  ========================================================================

   --  Returns the number of elements in the vector
   function Length (V : Vector) return Count_Type with
     Inline;

   --  Returns the maximum capacity of the vector
   function Max_Capacity return Positive is (Capacity) with
     Inline;

   --  Returns True if the vector contains no elements
   function Is_Empty (V : Vector) return Boolean with
     Post => Is_Empty'Result = (Length (V) = 0),
     Inline;

   --  Returns True if the vector is at capacity
   function Is_Full (V : Vector) return Boolean with
     Post => Is_Full'Result = (Length (V) = Capacity),
     Inline;

   --  Returns the first valid index (always 1)
   function First_Index return Index_Type is (1) with
     Inline;

   --  Returns the last valid index, or 0 if empty
   function Last_Index (V : Vector) return Extended_Index with
     Post => Last_Index'Result = Length (V),
     Inline;

   --  ========================================================================
   --  Element Access (Return Option)
   --  ========================================================================

   --  Returns the element at the given index, or None if out of bounds
   function Element (V : Vector; Index : Index_Type) return Element_Option with
     Post => (if Index <= Length (V) then Is_Some (Element'Result)
              else Is_None (Element'Result));

   --  Returns the first element, or None if empty
   function First_Element (V : Vector) return Element_Option with
     Post => (if not Is_Empty (V) then Is_Some (First_Element'Result)
              else Is_None (First_Element'Result));

   --  Returns the last element, or None if empty
   function Last_Element (V : Vector) return Element_Option with
     Post => (if not Is_Empty (V) then Is_Some (Last_Element'Result)
              else Is_None (Last_Element'Result));

   --  ========================================================================
   --  Direct Element Access (With Precondition)
   --  ========================================================================
   --  These functions provide direct access without Option wrapping.
   --  Use when caller has already verified bounds via Length check.
   --  ========================================================================

   --  Returns element at index (caller must ensure valid index)
   function Unchecked_Element
     (V : Vector; Index : Index_Type) return Element_Type with
     Pre => Index <= Length (V);

   --  Returns first element (caller must ensure not empty)
   function Unchecked_First (V : Vector) return Element_Type with
     Pre => not Is_Empty (V);

   --  Returns last element (caller must ensure not empty)
   function Unchecked_Last (V : Vector) return Element_Type with
     Pre => not Is_Empty (V);

   --  ========================================================================
   --  Modification Operations (Return Result)
   --  ========================================================================

   --  Appends element to end of vector
   --  Returns Err(Vector_Full) if at capacity
   procedure Append
     (V      : in out Vector;
      E      : Element_Type;
      Result : out Unit_Result) with
     Post => (if Is_Ok (Result) then Length (V) = Length (V)'Old + 1
              else Length (V) = Length (V)'Old);

   --  Removes the last element
   --  Returns Err(Vector_Empty) if empty
   procedure Delete_Last
     (V      : in out Vector;
      Result : out Unit_Result) with
     Post => (if Is_Ok (Result) then Length (V) = Length (V)'Old - 1
              else Length (V) = Length (V)'Old);

   --  Replaces element at given index
   --  Returns Err(Index_Out_Of_Bounds) if index > Length
   procedure Replace_Element
     (V      : in out Vector;
      Index  : Index_Type;
      E      : Element_Type;
      Result : out Unit_Result) with
     Post => Length (V) = Length (V)'Old;

   --  Removes all elements
   procedure Clear (V : in out Vector) with
     Post => Is_Empty (V);

   --  ========================================================================
   --  Convenience Procedures (With Precondition, No Result)
   --  ========================================================================
   --  Use when caller has already verified preconditions.
   --  ========================================================================

   --  Appends element (caller must ensure not full)
   procedure Unchecked_Append (V : in out Vector; E : Element_Type) with
     Pre  => not Is_Full (V),
     Post => Length (V) = Length (V)'Old + 1;

   --  Removes last element (caller must ensure not empty)
   procedure Unchecked_Delete_Last (V : in out Vector) with
     Pre  => not Is_Empty (V),
     Post => Length (V) = Length (V)'Old - 1;

   --  Replaces element (caller must ensure valid index)
   procedure Unchecked_Replace
     (V     : in out Vector;
      Index : Index_Type;
      E     : Element_Type) with
     Pre  => Index <= Length (V),
     Post => Length (V) = Length (V)'Old;

   --  ========================================================================
   --  Sorting (Nested Generic)
   --  ========================================================================
   --  Instantiate with a comparison function to get a sort procedure.
   --  Uses insertion sort for SPARK compatibility (no recursion, bounded).
   --
   --  Example:
   --    function Less_Than (A, B : My_Type) return Boolean is (A < B);
   --    procedure Sort is new My_Vectors.Generic_Sort ("<" => Less_Than);
   --    Sort (My_Vector);
   --  ========================================================================

   generic
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   procedure Generic_Sort (V : in out Vector) with
     Post => Length (V) = Length (V)'Old;

   --  ========================================================================
   --  Reverse Operation
   --  ========================================================================

   --  Reverses the order of elements in place
   procedure Reverse_Elements (V : in out Vector) with
     Post => Length (V) = Length (V)'Old;

   --  ========================================================================
   --  Swap Operation (used by sorting and reverse)
   --  ========================================================================

   --  Swaps two elements at given indices
   procedure Swap
     (V : in out Vector;
      I : Index_Type;
      J : Index_Type) with
     Pre  => I <= Length (V) and then J <= Length (V),
     Post => Length (V) = Length (V)'Old;

private

   --  Internal array type for element storage
   type Element_Array is array (Index_Type) of Element_Type;

   --  Vector record with invariant ensuring Last never exceeds Capacity
   type Vector is record
      Data : Element_Array;
      Last : Count_Type := 0;
   end record with
     Type_Invariant => Last <= Capacity;

end TZif.Domain.Types.Bounded_Vector;
