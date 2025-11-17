pragma Ada_2022;
--  ======================================================================
--  Error_With_Unbounded_String
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Test support utilities.
--
--  Key Types:
--    Error_Code
--    Domain_Error
--
--  ======================================================================

package Domain.Error is
   type Error_Code is (Success, Failure);

   type Domain_Error is record
      Code    : Error_Code;
      Message : String;  -- ❌ Should use Bounded_String
   end record;
end Domain.Error;
