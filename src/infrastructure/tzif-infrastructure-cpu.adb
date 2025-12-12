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
--    Uses Functional.Try to convert potential exceptions from
--    System.Multiprocessors to Option type, with safe default fallback.
--
--  ===========================================================================

with Functional.Option;
with Functional.Try;
with System.Multiprocessors;

package body TZif.Infrastructure.CPU is

   --  Option type for CPU count
   package Natural_Option is new Functional.Option (T => Natural);

   --  Raw action that may raise
   function Raw_Get_CPU_Count return Natural is
   begin
      return Natural (System.Multiprocessors.Number_Of_CPUs);
   end Raw_Get_CPU_Count;

   --  Wrap with Try - returns None on any exception
   function Try_Get_CPU_Count is new Functional.Try.Try_To_Functional_Option
     (T          => Natural,
      Option_Pkg => Natural_Option,
      Action     => Raw_Get_CPU_Count);

   function Get_CPU_Count return Natural is
   begin
      return Natural_Option.Unwrap_Or (Try_Get_CPU_Count, Default => 1);
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
