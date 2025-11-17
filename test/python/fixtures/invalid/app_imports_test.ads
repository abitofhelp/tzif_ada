pragma Ada_2022;
--  ======================================================================
--  App_Imports_Test
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Test support utilities.
--
--  Dependencies:
--    Test_Framework  -- ❌ Production code must not import test frameworks
--
--  ======================================================================

with Test_Framework;  -- ❌ Production code must not import test frameworks

package Application.Service is
   procedure Execute;
end Application.Service;
