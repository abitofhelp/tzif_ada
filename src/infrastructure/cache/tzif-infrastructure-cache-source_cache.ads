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
--  Key Types:
--    SHA256_Digest_Type
--    Source_Entry
--    Source_Map_Type
--    Source_Option_Type
--    Source_Cache_Type
--
--  Dependencies:
--    TZif.Domain.Value_Object.Source_Info
--    Functional.Option
--    Elaborate_Body
--
--  ===========================================================================

with Ada.Containers.Hashed_Maps;
with TZif.Domain.Value_Object.Source_Info;
with Functional.Option;

--  Cannot use Preelaborate due to Functional.Option dependency

package TZif.Infrastructure.Cache.Source_Cache with
  Elaborate_Body
is

   use TZif.Domain.Value_Object.Source_Info;

   --  ========================================================================
   --  Source Entry Type (Extended from Source_Info)
   --  ========================================================================

   subtype SHA256_Digest_Type is String (1 .. 64);  -- Full 256-bit hash (hex)

   type Source_Entry is record
      Path           : Path_String_Type;         -- Primary key
      Path_SHA256    : SHA256_Digest_Type;       -- Full hash for integrity
      Canonical_Path : Path_String_Type;         -- Resolved symlinks
      Version        : Version_String_Type;
      Zone_Count     : Natural;
      ULID           : ULID_Type;                -- For logging/tracking
   end record;

   --  Constructor from Source_Info
   function Make_Source_Entry
     (Source : Source_Info_Type; Canonical_Path : Path_String_Type)
      return Source_Entry;

   --  ========================================================================
   --  Path Hash Function (SHA256 folded via FNV-1a)
   --  ========================================================================

   function Hash_Path
     (Path : Path_String_Type) return Ada.Containers.Hash_Type;
   --  Computes SHA256 of path, folds to 32-bit via FNV-1a
   --  Provides content-based hashing with maximum entropy preservation

   --  ========================================================================
   --  Source Map Type
   --  ========================================================================

   function Path_Equal (Left, Right : Path_String_Type) return Boolean;

   package Source_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Path_String_Type, Element_Type => Source_Entry,
      Hash     => Hash_Path, Equivalent_Keys => Path_Equal);

   subtype Source_Map_Type is Source_Maps.Map;

   --  ========================================================================
   --  Option[Source_Entry] for cache misses
   --  ========================================================================

   package Source_Option is new Functional.Option (T => Source_Entry);
   subtype Source_Option_Type is Source_Option.Option;

   --  ========================================================================
   --  Protected Source Cache
   --  ========================================================================

   protected type Source_Cache_Type is

      --  ===================================================================
      --  Lock-Free Read Operations (No Task Suspension)
      --  ===================================================================

      --  Check if source exists by path
      function Contains (Path : Path_String_Type) return Boolean;

      --  Get source entry by path (returns None if not found)
      function Get (Path : Path_String_Type) return Source_Option_Type with
        Post =>
         (if Contains (Path) then Source_Option.Is_Some (Get'Result)
          else Source_Option.Is_None (Get'Result));

      --  Get all source entries as a map
      function Get_All return Source_Map_Type;

      --  Get number of sources in cache
      function Size return Natural;

      --  Check if cache is empty
      function Is_Empty return Boolean;

      --  ===================================================================
      --  Synchronized Write Operations (Serialized Access)
      --  ===================================================================

      --  Insert or update source entry
      procedure Insert (Path : Path_String_Type; Source : Source_Entry) with
        Post => Contains (Path);

      --  Remove source entry by path
      procedure Remove (Path : Path_String_Type) with
        Post => not Contains (Path);

      --  Clear all source entries
      procedure Clear with
        Post => Is_Empty;

   private
      Sources : Source_Map_Type;
   end Source_Cache_Type;

end TZif.Infrastructure.Cache.Source_Cache;
