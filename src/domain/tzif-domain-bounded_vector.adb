pragma Ada_2022;
--  ===========================================================================
--  TZif.Domain.Bounded_Vector (Body)
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  ===========================================================================

package body TZif.Domain.Bounded_Vector with
  SPARK_Mode => On
is

   --  ========================================================================
   --  Constants
   --  ========================================================================

   function Empty_Vector return Vector is
     ((Data => [others => <>], Last => 0));

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
         Result := Err (Vector_Full);
      else
         V.Last := V.Last + 1;
         V.Data (V.Last) := E;
         Result := Ok;
      end if;
   end Append;

   procedure Delete_Last
     (V      : in out Vector;
      Result : out Unit_Result)
   is
   begin
      if V.Last = 0 then
         Result := Err (Vector_Empty);
      else
         V.Last := V.Last - 1;
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
         Result := Err (Index_Out_Of_Bounds);
      else
         V.Data (Index) := E;
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

end TZif.Domain.Bounded_Vector;
