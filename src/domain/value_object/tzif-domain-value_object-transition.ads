pragma Ada_2022;
--  ===========================================================================
--  TZif.Domain.Value_Object.Transition
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Transition value object - immutable domain data.
--
--  Responsibilities:
--    - Define Transition type and operations
--    - Provide constructors and accessors
--
--  Key Types:
--    Transition_Type
--
--  Dependencies:
--    TZif.Domain.Value_Object.Epoch_Seconds
--    Pure
--
--  ===========================================================================

with TZif.Domain.Value_Object.Epoch_Seconds;

package TZif.Domain.Value_Object.Transition with
  Pure
is

   use TZif.Domain.Value_Object.Epoch_Seconds;

   --  ========================================================================
   --  Transition Type
   --  ========================================================================

   type Transition_Type is record
      --  Epoch time when this transition occurs
      Time : Epoch_Seconds_Type;

      --  Index into timezone types array (0-based)
      Type_Index : Natural;
   end record;

   --  ========================================================================
   --  Comparison Functions
   --  ========================================================================

   --  Compare two transitions by time (for sorting)
   function "<" (Left, Right : Transition_Type) return Boolean is
     (Left.Time < Right.Time);

   function "<=" (Left, Right : Transition_Type) return Boolean is
     (Left.Time <= Right.Time);

   function ">" (Left, Right : Transition_Type) return Boolean is
     (Left.Time > Right.Time);

   function ">=" (Left, Right : Transition_Type) return Boolean is
     (Left.Time >= Right.Time);

   --  ========================================================================
   --  Query Functions
   --  ========================================================================

   --  Check if transition occurs before given time
   function Occurs_Before
     (Transition : Transition_Type; Time : Epoch_Seconds_Type)
      return Boolean is
     (Transition.Time < Time);

   --  Check if transition occurs after given time
   function Occurs_After
     (Transition : Transition_Type; Time : Epoch_Seconds_Type)
      return Boolean is
     (Transition.Time > Time);

   --  Check if transition occurs at given time
   function Occurs_At
     (Transition : Transition_Type; Time : Epoch_Seconds_Type)
      return Boolean is
     (Transition.Time = Time);

end TZif.Domain.Value_Object.Transition;
