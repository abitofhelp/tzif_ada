pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.Cpu
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Cpu interface and type definitions.
--
--  ===========================================================================

package TZif.Infrastructure.CPU is

   --  Get number of CPU cores available
   function Get_CPU_Count return Natural;

   --  Calculate optimal number of parallel tasks based on CPU count
   --  Strategy:
   --    1 core  -> 0 tasks (sequential)
   --    2-4     -> N-1 tasks
   --    5+      -> 50% of cores (max 8)
   function Get_Optimal_Task_Count return Natural;

end TZif.Infrastructure.CPU;
