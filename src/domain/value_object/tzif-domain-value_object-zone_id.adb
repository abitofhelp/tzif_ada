pragma Ada_2022;
--  ===========================================================================
--  TZif.Domain.Value_Object.Zone_Id
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Zone Id value object - immutable domain data.
--
--  ===========================================================================

with Ada.Strings;

package body TZif.Domain.Value_Object.Zone_Id is

   --  ========================================================================
   --  Constructor Functions
   --  ========================================================================

   function Make_Zone_Id (Id : String) return Zone_Id_Type is
   begin
      --  May propagate Ada.Strings.Length_Error if exceeds max length
      return Make_Unchecked (Id);
   end Make_Zone_Id;

   function Make_Zone_Id_Truncate (Id : String) return Zone_Id_Type is
   begin
      --  Truncate on the right to fit within bounded length
      return
        Zone_Id_Type'
          (ID =>
             Zone_Id_Strings.To_Bounded_String
               (Id, Drop => Ada.Strings.Right));
   end Make_Zone_Id_Truncate;

   --  ========================================================================
   --  Query Functions
   --  ========================================================================

   function To_String (Id : Zone_Id_Type) return String is
   begin
      return Zone_Id_Strings.To_String (Id.ID);
   end To_String;

   function Length (Id : Zone_Id_Type) return Natural is
   begin
      return Zone_Id_Strings.Length (Id.ID);
   end Length;

   function Is_Empty (Id : Zone_Id_Type) return Boolean is
   begin
      return Length (Id) = 0;
   end Is_Empty;

   function Matches (Id : Zone_Id_Type; Value : String) return Boolean is
   begin
      return To_String (Id) = Value;
   end Matches;

   --  ========================================================================
   --  Comparison Functions
   --  ========================================================================

   overriding function "=" (Left, Right : Zone_Id_Type) return Boolean is
   begin
      return Zone_Id_Strings."=" (Left.ID, Right.ID);
   end "=";

   function "<" (Left, Right : Zone_Id_Type) return Boolean is
   begin
      return Zone_Id_Strings."<" (Left.ID, Right.ID);
   end "<";

   --  ========================================================================
   --  Private Constructor
   --  ========================================================================

   function Make_Unchecked (Id : String) return Zone_Id_Type is
   begin
      return Zone_Id_Type'(ID => Zone_Id_Strings.To_Bounded_String (Id));
   end Make_Unchecked;

end TZif.Domain.Value_Object.Zone_Id;
