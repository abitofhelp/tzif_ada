pragma Ada_2022;
--  ======================================================================
--  Test_Spies.Find_By_Regex_Spy
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Unit tests for Spies-Find By Regex Spy functionality.
--
--  ======================================================================

with Ada.Containers.Vectors;

package body Test_Spies.Find_By_Regex_Spy is
   use TZif.Application.Port.Inbound.Find_By_Regex.Zone_Name_Strings;

   package Name_Vec is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Zone_Name_String,
      "="          => Zone_Name_Strings."=");

   protected State is
      procedure Inc (Name : Zone_Name_String);
      procedure Reset;
      function Count return Natural;
      function Snapshot return Name_Vec.Vector;
   private
      C : Natural := 0;
      V : Name_Vec.Vector;
   end State;

   protected body State is
      procedure Inc (Name : Zone_Name_String) is
      begin
         C := C + 1;
         V.Append (Name);
      end Inc;

      procedure Reset is
      begin
         C := 0;
         V.Clear;
      end Reset;

      function Count return Natural is (C);

      function Snapshot return Name_Vec.Vector is (V);
   end State;

   procedure Collect (Name : Zone_Name_String) is
   begin
      State.Inc (Name);
   end Collect;

   procedure Reset is
   begin
      State.Reset;
   end Reset;

   function Count return Natural is
   begin
      return State.Count;
   end Count;

   function Names return Zone_Name_Array is
      Vc : constant Name_Vec.Vector := State.Snapshot;
      Len : constant Natural := Natural (Vc.Length);
   begin
      if Len = 0 then
         return [1 .. 0 => <>];
      else
         declare
            Arr : Zone_Name_Array (1 .. Len);
         begin
            for I in 1 .. Len loop
               Arr (I) := Vc.Element (I);
            end loop;
            return Arr;
         end;
      end if;
   end Names;
end Test_Spies.Find_By_Regex_Spy;
