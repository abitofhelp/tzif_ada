pragma Ada_2022;
--  ======================================================================
--  Test_Framework
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Unit tests for Framework functionality.
--
--  ======================================================================

package Test_Framework is

   --  Track grand totals across all test suites
   procedure Register_Results (Total : Natural; Passed : Natural);

   --  Get cumulative results
   function Grand_Total_Tests return Natural;
   function Grand_Total_Passed return Natural;

   --  Reset counters (for test runner)
   procedure Reset;

end Test_Framework;
