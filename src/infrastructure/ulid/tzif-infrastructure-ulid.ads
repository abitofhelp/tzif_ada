pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.ULID
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    ULID generator instantiation with System.Random_Numbers (high quality).
--
--  Architecture:
--    Concrete instantiation of generic ULID.Generator with GNAT's
--    System.Random_Numbers for cryptographically strong random generation.
--
--  Design:
--    - Uses GNAT-internal System.Random_Numbers (Mersenne Twister)
--    - Thread-safe via protected type from generic
--    - Higher quality than Ada.Numerics.Discrete_Random
--    - Appropriate for production UUID/ULID generation
--
--  SPARK Compatibility:
--    - This package is SPARK_Mode => Off (uses non-standard RNG)
--    - Callers can still verify contracts from Generator spec
--    - Alternative: Users can instantiate with SPARK-compatible RNG
--
--  Dependencies:
--    TZif.Infrastructure.ULID.Generator (generic)
--    TZif.Domain.Value_Object.Source_Info (ULID_Type)
--    System.Random_Numbers (GNAT-internal)
--
--  ===========================================================================

pragma Warnings (Off, """System.Random_Numbers"" is an internal GNAT unit");
with System.Random_Numbers;
pragma Warnings (On, """System.Random_Numbers"" is an internal GNAT unit");

with Interfaces;
with TZif.Infrastructure.ULID_Generic;
with TZif.Domain.Value_Object.Source_Info;

package TZif.Infrastructure.ULID
  with SPARK_Mode => Off,  --  Uses GNAT-internal non-standard RNG
       Elaborate_Body
is

   use TZif.Domain.Value_Object.Source_Info;
   use Interfaces;

   --  ========================================================================
   --  System.Random_Numbers Adapter
   --  ========================================================================
   --
   --  Adapts GNAT's System.Random_Numbers to the generic RNG interface.
   --  Provides higher quality randomness than Ada.Numerics.Discrete_Random.
   --
   --  ========================================================================

   subtype Random_State is System.Random_Numbers.Generator;

   --  Reset the RNG
   procedure Reset_RNG (Gen : Random_State) renames
     System.Random_Numbers.Reset;

   --  Generate a random byte using System.Random_Numbers
   function Random_Byte_Impl (Gen : in out Random_State)
     return Unsigned_8 is
     (Unsigned_8 (Unsigned_32'(System.Random_Numbers.Random (Gen)) mod 256));

   --  ========================================================================
   --  ULID Generator Instantiation
   --  ========================================================================
   --
   --  Instantiate the generic with System.Random_Numbers adapter.
   --  This is the production ULID generator used throughout TZif.
   --
   --  ========================================================================

   package ULID_Gen is new TZif.Infrastructure.ULID_Generic
     (Random_State => Random_State,
      Reset        => Reset_RNG,
      Random_Byte  => Random_Byte_Impl);

   --  ========================================================================
   --  Public API (Convenience Renamings)
   --  ========================================================================
   --
   --  Provides clean API without needing to reference ULID_Gen package.
   --  Maintains backward compatibility with existing TZif code.
   --
   --  ========================================================================

   --  Thread-safe ULID generator
   subtype ULID_Generator is ULID_Gen.ULID_Generator_Type;

   --  Generate a new ULID
   function Generate return ULID_Type renames ULID_Gen.New_ULID;

   --  Generate deterministic ULID from seed (for testing)
   function Generate_From_Seed (Seed : String) return ULID_Type renames
     ULID_Gen.Generate_From_Seed;

   --  Global singleton generator instance
   Global_Generator : ULID_Generator renames ULID_Gen.Global_Generator;

end TZif.Infrastructure.ULID;
