pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.Cpu
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Cpu implementation.
--
--  Implementation Notes:
--    Uses simple exception handler for System.Multiprocessors since this
--    package must be Preelaborate (cannot depend on Functional.Try).
--    Returns safe default (1 CPU) on any exception.
--
--  ===========================================================================

with System.Multiprocessors;

package body TZif.Infrastructure.CPU is

   function Get_CPU_Count return Natural is
   begin
      return Natural (System.Multiprocessors.Number_Of_CPUs);
   exception
      when others =>
         return 1;  --  Safe default: assume single CPU
   end Get_CPU_Count;

   function Get_Optimal_Task_Count return Natural is
      CPU_Count : constant Natural := Get_CPU_Count;
   begin
      case CPU_Count is
         when 0 | 1 =>
            return 0;  -- Sequential

         when 2 .. 4 =>
            return CPU_Count - 1;  -- Leave one for system

         when others =>
            --  50% of cores, max 8
            return Natural'Min (CPU_Count / 2, 8);
      end case;
   end Get_Optimal_Task_Count;

end TZif.Infrastructure.CPU;
