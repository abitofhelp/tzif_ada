pragma Ada_2022;
--  ===========================================================================
--  TZif.Domain.Option
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Option type for representing optional values within the Domain layer.
--    Domain-local clone of Functional.Option to maintain zero external
--    dependencies in the Domain (hexagonal architecture constraint).
--
--  Key Types:
--    Option_Kind  - Discriminator (K_Some, K_None)
--    Option       - Generic optional value container
--
--  Design:
--    - Domain layer must have ZERO external crate dependencies
--    - This is a minimal clone for Domain use only
--    - Infrastructure/Application should use Functional.Option
--
--  ===========================================================================

generic
   type T is private;
package TZif.Domain.Option with
  Preelaborate
is

   type Option_Kind is (K_Some, K_None);

   type Option (Kind : Option_Kind := K_None) is record
      case Kind is
         when K_Some =>
            Value : T;

         when K_None =>
            null;
      end case;
   end record;

   --  ========================================================================
   --  Constructors
   --  ========================================================================

   function New_Some (V : T) return Option
   with Inline;
   function None return Option
   with Inline;

   --  ========================================================================
   --  Predicates
   --  ========================================================================

   function Is_Some (O : Option) return Boolean
   with Inline;
   function Is_None (O : Option) return Boolean
   with Inline;

   --  ========================================================================
   --  Extractors
   --  ========================================================================

   function Value (O : Option) return T
   with Pre => O.Kind = K_Some, Inline;

   --  ========================================================================
   --  Unwrap with defaults
   --  ========================================================================

   function Unwrap_Or (O : Option; Default : T) return T
   with
     Post =>
       (if O.Kind = K_Some then Unwrap_Or'Result = O.Value
        else Unwrap_Or'Result = Default);

   generic
      with function F return T;
   function Unwrap_Or_With (O : Option) return T;

   --  ========================================================================
   --  Mapping and chaining
   --  ========================================================================

   --  Map: transform Some value
   generic
      with function F (X : T) return T;
   function Map (O : Option) return Option;

   --  And_Then: chain optional operations (monadic bind)
   generic
      with function F (X : T) return Option;
   function And_Then (O : Option) return Option;

   --  Filter: keep value only if predicate holds
   generic
      with function Pred (X : T) return Boolean;
   function Filter (O : Option) return Option;

   --  ========================================================================
   --  Fallback
   --  ========================================================================

   --  Or_Else: fallback to alternative (eager evaluation)
   function Or_Else (A, B : Option) return Option;

   --  Or_Else_With: fallback to alternative (lazy evaluation)
   generic
      with function F return Option;
   function Or_Else_With (O : Option) return Option;

   --  Aliases for discoverability (Result-style naming)
   function Fallback (A, B : Option) return Option renames Or_Else;

end TZif.Domain.Option;
