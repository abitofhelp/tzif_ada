pragma Ada_2022;
--  ===========================================================================
--  TZif.Domain.Types.Bounded_Vector (Body)
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  ===========================================================================

package body TZif.Domain.Types.Bounded_Vector with
  SPARK_Mode => On
is

   --  ========================================================================
   --  Constants
   --  ========================================================================

   function Empty_Vector return Vector is
     ((Data => [others => Default_Value], Last => 0));

   --  ========================================================================
   --  Query Functions
   --  ========================================================================

   function Length (V : Vector) return Count_Type is
     (V.Last);

   function Is_Empty (V : Vector) return Boolean is
     (V.Last = 0);

   function Is_Full (V : Vector) return Boolean is
     (V.Last = Capacity);

   function Last_Index (V : Vector) return Extended_Index is
     (V.Last);

   --  ========================================================================
   --  Element Access (Return Option)
   --  ========================================================================

   function Element (V : Vector; Index : Index_Type) return Element_Option is
   begin
      if Index <= V.Last then
         return (Kind => K_Some, Value => V.Data (Index));
      else
         return (Kind => K_None);
      end if;
   end Element;

   function First_Element (V : Vector) return Element_Option is
   begin
      if V.Last > 0 then
         return (Kind => K_Some, Value => V.Data (1));
      else
         return (Kind => K_None);
      end if;
   end First_Element;

   function Last_Element (V : Vector) return Element_Option is
   begin
      if V.Last > 0 then
         return (Kind => K_Some, Value => V.Data (V.Last));
      else
         return (Kind => K_None);
      end if;
   end Last_Element;

   --  ========================================================================
   --  Direct Element Access (With Precondition)
   --  ========================================================================

   function Unchecked_Element
     (V : Vector; Index : Index_Type) return Element_Type
   is
   begin
      return V.Data (Index);
   end Unchecked_Element;

   function Unchecked_First (V : Vector) return Element_Type is
   begin
      return V.Data (1);
   end Unchecked_First;

   function Unchecked_Last (V : Vector) return Element_Type is
   begin
      return V.Data (V.Last);
   end Unchecked_Last;

   --  ========================================================================
   --  Modification Operations (Return Result)
   --  ========================================================================

   procedure Append
     (V      : in out Vector;
      E      : Element_Type;
      Result : out Unit_Result)
   is
   begin
      if V.Last >= Capacity then
         pragma Annotate
           (GNATprove, Intentional,
            "discriminant check might fail",
            "Assigning Err to mutable discriminant out parameter is safe");
         Result := Err (Vector_Full);
      else
         V.Last := V.Last + 1;
         V.Data (V.Last) := E;
         pragma Annotate
           (GNATprove, Intentional,
            "discriminant check might fail",
            "Assigning Ok to mutable discriminant out parameter is safe");
         Result := Ok;
      end if;
   end Append;

   procedure Delete_Last
     (V      : in out Vector;
      Result : out Unit_Result)
   is
   begin
      if V.Last = 0 then
         pragma Annotate
           (GNATprove, Intentional,
            "discriminant check might fail",
            "Assigning Err to mutable discriminant out parameter is safe");
         Result := Err (Vector_Empty);
      else
         V.Last := V.Last - 1;
         pragma Annotate
           (GNATprove, Intentional,
            "discriminant check might fail",
            "Assigning Ok to mutable discriminant out parameter is safe");
         Result := Ok;
      end if;
   end Delete_Last;

   procedure Replace_Element
     (V      : in out Vector;
      Index  : Index_Type;
      E      : Element_Type;
      Result : out Unit_Result)
   is
   begin
      if Index > V.Last then
         pragma Annotate
           (GNATprove, Intentional,
            "discriminant check might fail",
            "Assigning Err to mutable discriminant out parameter is safe");
         Result := Err (Index_Out_Of_Bounds);
      else
         V.Data (Index) := E;
         pragma Annotate
           (GNATprove, Intentional,
            "discriminant check might fail",
            "Assigning Ok to mutable discriminant out parameter is safe");
         Result := Ok;
      end if;
   end Replace_Element;

   procedure Clear (V : in out Vector) is
   begin
      V.Last := 0;
   end Clear;

   --  ========================================================================
   --  Convenience Procedures (With Precondition, No Result)
   --  ========================================================================

   procedure Unchecked_Append (V : in out Vector; E : Element_Type) is
   begin
      V.Last := V.Last + 1;
      V.Data (V.Last) := E;
   end Unchecked_Append;

   procedure Unchecked_Delete_Last (V : in out Vector) is
   begin
      V.Last := V.Last - 1;
   end Unchecked_Delete_Last;

   procedure Unchecked_Replace
     (V     : in out Vector;
      Index : Index_Type;
      E     : Element_Type)
   is
   begin
      V.Data (Index) := E;
   end Unchecked_Replace;

   --  ========================================================================
   --  Swap Operation
   --  ========================================================================

   procedure Swap
     (V : in out Vector;
      I : Index_Type;
      J : Index_Type)
   is
      Temp : constant Element_Type := V.Data (I);
   begin
      V.Data (I) := V.Data (J);
      V.Data (J) := Temp;
   end Swap;

   --  ========================================================================
   --  Reverse Operation
   --  ========================================================================

   procedure Reverse_Elements (V : in out Vector) is
      Left  : Index_Type := 1;
      Right : Extended_Index := V.Last;
   begin
      --  Swap elements from ends toward middle
      while Left < Right loop
         pragma Loop_Invariant (Left <= V.Last);
         pragma Loop_Invariant (Right <= V.Last);
         pragma Loop_Invariant (V.Last = V.Last'Loop_Entry);

         Swap (V, Left, Right);
         Left := Left + 1;
         Right := Right - 1;
      end loop;
   end Reverse_Elements;

   --  ========================================================================
   --  Sorting (Insertion Sort - SPARK-compatible)
   --  ========================================================================
   --  Insertion sort is chosen for SPARK compatibility:
   --  - No recursion (bounded stack usage)
   --  - Simple loop structure with provable termination
   --  - O(n²) worst case, O(n) best case (nearly sorted data)
   --  - Stable sort (equal elements maintain relative order)
   --  ========================================================================

   procedure Generic_Sort (V : in out Vector) is
      J    : Extended_Index;
      Key  : Element_Type;
   begin
      --  Empty or single-element vectors are already sorted
      if V.Last <= 1 then
         return;
      end if;

      --  Insertion sort: build sorted sequence from left to right
      for I in 2 .. V.Last loop
         pragma Loop_Invariant (V.Last = V.Last'Loop_Entry);

         Key := V.Data (I);
         J := I - 1;

         --  Shift larger elements right (use "<" since we only have that)
         while J >= 1 and then Key < V.Data (J) loop
            pragma Loop_Invariant (J < I);
            pragma Loop_Invariant (V.Last = V.Last'Loop_Entry);

            V.Data (J + 1) := V.Data (J);
            J := J - 1;
         end loop;

         --  Insert key at correct position
         V.Data (J + 1) := Key;
      end loop;
   end Generic_Sort;

end TZif.Domain.Types.Bounded_Vector;
