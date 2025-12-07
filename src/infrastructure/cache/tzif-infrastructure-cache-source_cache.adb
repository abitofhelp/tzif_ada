pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.Cache.Source_Cache
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Source Cache for performance optimization.
--
--  ===========================================================================

with GNAT.SHA256;
with Interfaces; use Interfaces;
with TZif.Infrastructure.Cache.Path_Canonical;

package body TZif.Infrastructure.Cache.Source_Cache is

   --  ========================================================================
   --  Source Entry Constructor
   --  ========================================================================

   function Make_Source_Entry
     (Source : Source_Info_Type; Canonical_Path : Path_String_Type)
      return Source_Entry
   is
      --  Hash the canonical path for semantic identity
      Canonical_Str : constant String := To_String (Canonical_Path);
      SHA256_Hash   : constant SHA256_Digest_Type :=
        GNAT.SHA256.Digest (Canonical_Str);
   begin
      return
        (Path           => Get_Path (Source), Path_SHA256 => SHA256_Hash,
         Canonical_Path => Canonical_Path, Version => Get_Version (Source),
         Zone_Count     => Get_Zone_Count (Source), ULID => Get_ULID (Source));
   end Make_Source_Entry;

   --  ========================================================================
   --  FNV-1a Fold of SHA256 Digest
   --  ========================================================================

   function Hash_Path (Path : Path_String_Type) return Ada.Containers.Hash_Type
   is
      --  Canonicalize path before hashing (normalize separators, case, etc.)
      Canonical     : constant Path_String_Type :=
        Path_Canonical.Canonicalize (Path);
      Path_Str      : constant String           := To_String (Canonical);
      SHA256_Digest : constant String := GNAT.SHA256.Digest (Path_Str);
      Acc : Unsigned_32 := 2_166_136_261;  -- FNV-1a offset basis (32-bit)
   begin
      --  Fold entire 64-character hex digest into 32 bits using FNV-1a
      for C of SHA256_Digest loop
         Acc := Acc xor Unsigned_32 (Character'Pos (C));
         Acc := Acc * 16_777_619;  -- FNV-1a prime (32-bit)
      end loop;

      return Ada.Containers.Hash_Type (Acc);
   end Hash_Path;

   --  ========================================================================
   --  Path Equality
   --  ========================================================================

   function Path_Equal (Left, Right : Path_String_Type) return Boolean is
      --  Compare canonical forms to handle path variations
      Left_Canonical  : constant Path_String_Type :=
        Path_Canonical.Canonicalize (Left);
      Right_Canonical : constant Path_String_Type :=
        Path_Canonical.Canonicalize (Right);
   begin
      return To_String (Left_Canonical) = To_String (Right_Canonical);
   end Path_Equal;

   --  ========================================================================
   --  Protected Source Cache Body
   --  ========================================================================

   protected body Source_Cache_Type is

      --  ===================================================================
      --  Lock-Free Read Operations
      --  ===================================================================

      function Contains (Path : Path_String_Type) return Boolean is
      begin
         return Sources.Contains (Path);
      end Contains;

      function Get (Path : Path_String_Type) return Source_Option_Type is
      begin
         if Sources.Contains (Path) then
            return Source_Option.New_Some (Sources.Element (Path));
         else
            return Source_Option.None;
         end if;
      end Get;

      function Get_All return Source_Map_Type is
      begin
         return Sources;
      end Get_All;

      function Size return Natural is
      begin
         return Natural (Sources.Length);
      end Size;

      function Is_Empty return Boolean is
      begin
         return Sources.Is_Empty;
      end Is_Empty;

      --  ===================================================================
      --  Synchronized Write Operations
      --  ===================================================================

      procedure Insert (Path : Path_String_Type; Source : Source_Entry) is
      begin
         Sources.Include (Path, Source);
      end Insert;

      procedure Remove (Path : Path_String_Type) is
      begin
         if Sources.Contains (Path) then
            Sources.Delete (Path);
         end if;
      end Remove;

      procedure Clear is
      begin
         Sources.Clear;
      end Clear;

   end Source_Cache_Type;

end TZif.Infrastructure.Cache.Source_Cache;
