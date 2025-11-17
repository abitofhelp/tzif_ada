pragma Ada_2022;
--  ======================================================================
--  Test_Spies.Find_By_Pattern_Spy
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Unit tests for Spies-Find By Pattern Spy functionality.
--
--  Key Types:
--    Zone_Name_Array
--
--  Dependencies:
--    TZif.Application.Port.Inbound.Find_By_Pattern
--    Preelaborate
--
--  ======================================================================

with TZif.Application.Port.Inbound.Find_By_Pattern;

package Test_Spies.Find_By_Pattern_Spy
  with Preelaborate
is
   use TZif.Application.Port.Inbound.Find_By_Pattern;

   --  Library-level callback you pass to the use case
   procedure Collect (Name : Zone_Name_String);

   --  Simple API for assertions
   procedure Reset;
   function Count return Natural;

   --  Optional: inspect what was yielded
   type Zone_Name_Array is array (Natural range <>) of Zone_Name_String;
   function Names return Zone_Name_Array;
end Test_Spies.Find_By_Pattern_Spy;
