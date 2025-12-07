pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.Paths.Canonical
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Canonical implementation.
--
--  ===========================================================================

with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body TZif.Infrastructure.Paths.Canonical is

   function Canonicalize
     (S           : String; Case_Insensitive : Boolean := False;
      Slash_Style : Character := '/') return String
   is
      use Ada.Directories;
      use Ada.Characters.Handling;

      --  Step 1: Get absolute path (falls back to input on error)
      function Get_Absolute return String is
      begin
         return Full_Name (S);
      exception
         when others =>
            return S;
      end Get_Absolute;

      Absolute_Path : constant String := Get_Absolute;

      --  Step 2: Unify separators + collapse duplicates
      B           : Unbounded_String := To_Unbounded_String ("");
      Prev_Is_Sep : Boolean          := False;

      function Is_Sep (C : Character) return Boolean is
      begin
         return C = '/' or else C = '\';
      end Is_Sep;

   begin
      --  Process each character
      for C of Absolute_Path loop
         if Is_Sep (C) then
            if not Prev_Is_Sep then
               Append (B, Slash_Style);
               Prev_Is_Sep := True;
            end if;
         else
            Append (B, C);
            Prev_Is_Sep := False;
         end if;
      end loop;

      --  Step 3: Drop trailing separator unless root-only
      declare
         R : String := To_String (B);
      begin
         if R'Length > 1 and then R (R'Last) = Slash_Style then
            --  Keep leading '//' (UNC) or single root '/' as-is
            if not
              (R'Length >= 2 and then R (R'First) = Slash_Style
               and then R (R'First + 1) = Slash_Style)
            then
               R := R (R'First .. R'Last - 1);
            end if;
         end if;

         --  Step 4: Apply case folding if requested
         if Case_Insensitive then
            return To_Lower (R);
         else
            return R;
         end if;
      end;
   end Canonicalize;

end TZif.Infrastructure.Paths.Canonical;
