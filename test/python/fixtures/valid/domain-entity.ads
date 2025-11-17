pragma Ada_2022;
--  ======================================================================
--  Domain.Entity
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Test support utilities.
--
--  Key Types:
--    Entity_ID
--    Entity
--
--  Dependencies:
--    Pure  -- ✅ Using aspect instead of pragma
--
--  ======================================================================

package Domain.Entity
   with Pure  -- ✅ Using aspect instead of pragma
is
   type Entity_ID is range 1 .. 1_000_000;

   type Entity is record
      ID : Entity_ID;
   end record;

end Domain.Entity;
