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
--  Implementation Notes:
--    Uses Functional.Try to handle potential exceptions from Ada.Directories.
--
--  ===========================================================================

with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Functional.Option;
with Functional.Try;

package body TZif.Infrastructure.Paths.Canonical is

   --  We need a bounded string for Option since String is indefinite
   Max_Path_Length : constant := 4096;
   subtype Path_String is String (1 .. Max_Path_Length);

   package Path_Option is new Functional.Option (T => Path_String);

   function Canonicalize
     (S           : String; Case_Insensitive : Boolean := False;
      Slash_Style : Character := '/') return String
   is
      use Ada.Directories;
      use Ada.Characters.Handling;

      --  Padded path for Option
      function Pad_Path (P : String) return Path_String is
         Result : Path_String := [others => ' '];
      begin
         Result (1 .. P'Length) := P;
         return Result;
      end Pad_Path;

      --  Step 1: Get absolute path using Functional.Try
      function Raw_Full_Name return Path_String is
      begin
         return Pad_Path (Full_Name (S));
      end Raw_Full_Name;

      function Try_Full_Name is new Functional.Try.Try_To_Functional_Option
        (T          => Path_String,
         Option_Pkg => Path_Option,
         Action     => Raw_Full_Name);

      --  Try to get full name, fall back to input on failure
      function Get_Absolute_Path return String is
         Opt : constant Path_Option.Option := Try_Full_Name;
      begin
         if Path_Option.Is_Some (Opt) then
            declare
               Padded : constant Path_String := Path_Option.Value (Opt);
               --  Find actual length (trim trailing spaces)
               Actual_Len : Natural := Max_Path_Length;
            begin
               while Actual_Len > 0 and then Padded (Actual_Len) = ' ' loop
                  Actual_Len := Actual_Len - 1;
               end loop;
               return Padded (1 .. Actual_Len);
            end;
         else
            return S;
         end if;
      end Get_Absolute_Path;

      Absolute_Path : constant String := Get_Absolute_Path;

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
