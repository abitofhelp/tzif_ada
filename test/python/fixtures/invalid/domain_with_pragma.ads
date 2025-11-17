pragma Ada_2022;
--  ======================================================================
--  Domain_With_Pragma
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Test support utilities.
--
--  Key Types:
--    Count
--
--  ======================================================================

package Domain.Types is
   pragma Pure;  -- ❌ Should use "with Pure" aspect

   type Count is range 0 .. 1_000;
end Domain.Types;
