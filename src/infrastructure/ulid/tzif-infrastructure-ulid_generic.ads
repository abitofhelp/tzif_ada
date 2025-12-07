pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.ULID_Generic
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Generic ULID generator with pluggable RNG for SPARK compatibility.
--
--  Architecture:
--    Infrastructure layer - generic with RNG plugin (dependency inversion).
--    Instantiated with concrete RNG adapters (System.Random_Numbers, etc.)
--
--  Design:
--    - Generic RNG formal parameters (type + procedures)
--    - Thread-safe generation via protected type
--    - Monotonic increment for same-millisecond ULIDs
--    - Ada 2022 contracts for SPARK boundary verification
--
--  SPARK Compatibility:
--    - Spec can be SPARK_Mode => On (declares structure/contracts)
--    - Body must be SPARK_Mode => Off (uses RNG = non-deterministic)
--    - Callers can verify contracts even though implementation isn't proven
--
--  Dependencies:
--    TZif.Domain.Value_Object.Source_Info (ULID_Type)
--    Interfaces (Unsigned types)
--
--  ===========================================================================

with TZif.Domain.Value_Object.Source_Info;
with Interfaces;

generic
   --  ========================================================================
   --  RNG Plugin Interface (Port for Dependency Inversion)
   --  ========================================================================
   --
   --  This allows different RNG implementations:
   --    - System.Random_Numbers (GNAT, high quality)
   --    - Ada.Numerics.Discrete_Random (standard Ada)
   --    - Hardware RNG (embedded systems)
   --    - Deterministic RNG (testing)
   --
   --  All are non-SPARK, but users can provide SPARK-compatible wrappers
   --  if they have SPARK-proven hardware RNG.
   --
   --  ========================================================================

   type Random_State is limited private;
   --  RNG generator state (opaque to this package)

   with procedure Reset (Gen : Random_State);
   --  Initialize/reset the RNG

   with function Random_Byte (Gen : in out Random_State)
     return Interfaces.Unsigned_8;
   --  Generate a random byte (0..255)

package TZif.Infrastructure.ULID_Generic
  with SPARK_Mode => On  --  Spec can declare SPARK-compatible structure
is

   use TZif.Domain.Value_Object.Source_Info;
   use Interfaces;

   --  ========================================================================
   --  Types
   --  ========================================================================

   --  Random bytes buffer for monotonic increment
   type Random_Bytes_Type is array (1 .. 10) of Unsigned_8;

   --  ========================================================================
   --  Thread-Safe ULID Generator
   --  ========================================================================
   --
   --  Protected type ensures thread-safe ULID generation with:
   --    - Monotonic increment within same millisecond
   --    - Cryptographically random component
   --    - Automatic RNG initialization
   --
   --  SPARK Note: Protected types ARE SPARK-compatible as data structures,
   --  though the implementation uses non-SPARK RNG.
   --
   --  ========================================================================

   protected type ULID_Generator_Type is

      --  Generate a new ULID
      procedure Generate (Result : out ULID_Type)
        with Post => Result /= Null_ULID;
      --  Postcondition ensures valid ULID even though body is non-SPARK

      --  Reset the RNG (for testing/reinitialization)
      procedure Reset_RNG;

   private
      --  RNG state
      RNG         : Random_State;
      Initialized : Boolean := False;

      --  Monotonic state (for same-millisecond generation)
      Last_Timestamp_Ms : Unsigned_64 := 0;
      Last_Random_Bytes : Random_Bytes_Type := [others => 0];
   end ULID_Generator_Type;

   --  ========================================================================
   --  Singleton Instance
   --  ========================================================================
   --
   --  Global singleton for convenient access.
   --  Applications can also create their own instances if needed.
   --
   --  ========================================================================

   Global_Generator : ULID_Generator_Type;

   --  ========================================================================
   --  Convenience Functions
   --  ========================================================================

   --  Generate a new ULID using the global generator
   function New_ULID return ULID_Type
     with Post => New_ULID'Result /= Null_ULID;

   --  Generate ULID from seed (deterministic, for testing)
   --  Not using RNG - pure function based on hash
   function Generate_From_Seed (Seed : String) return ULID_Type
     with Pre  => Seed'Length > 0,
          Post => Generate_From_Seed'Result /= Null_ULID;

end TZif.Infrastructure.ULID_Generic;
