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
--    Uses Functional.Try.Map_To_Result to wrap the multiprocessor query,
--    returning safe default (1 CPU) on any exception.
--
--  ===========================================================================

with Functional.Result;
with Functional.Try.Map_To_Result;
with System.Multiprocessors;
with TZif.Domain.Error;

package body TZif.Infrastructure.CPU is

   use TZif.Domain.Error;

   --  ========================================================================
   --  Result type for CPU count
   --  ========================================================================

   package CPU_Result is new Functional.Result (T => Natural, E => Error_Kind);

   --  ========================================================================
   --  Make Error (used by Map_To_Result)
   --  ========================================================================

   function Make_CPU_Error
     (Kind : Error_Kind; Message : String) return CPU_Result.Result
   is
      pragma Unreferenced (Message);
   begin
      return CPU_Result.New_Error (Kind);
   end Make_CPU_Error;

   --  ========================================================================
   --  Raw_Get_CPU_Count
   --
   --  Raw action that may raise exceptions - wrapped by Map_To_Result
   --  ========================================================================

   function Raw_Get_CPU_Count return CPU_Result.Result is
   begin
      return CPU_Result.Ok (Natural (System.Multiprocessors.Number_Of_CPUs));
   end Raw_Get_CPU_Count;

   --  ========================================================================
   --  Map_To_Result wrapper
   --  ========================================================================

   package Try_CPU is new Functional.Try.Map_To_Result
     (Error_Kind_Type    => Error_Kind,
      Result_Type        => CPU_Result.Result,
      Make_Error         => Make_CPU_Error,
      Default_Error_Kind => IO_Error,
      Action             => Raw_Get_CPU_Count);

   --  Empty mappings: all exceptions map to IO_Error (default)
   CPU_Mappings : constant Try_CPU.Mapping_Array := Try_CPU.Empty_Mappings;

   --  ========================================================================
   --  Get_CPU_Count (public API)
   --  ========================================================================

   function Get_CPU_Count return Natural is
      Result : constant CPU_Result.Result := Try_CPU.Run (CPU_Mappings);
   begin
      return CPU_Result.Unwrap_Or (Result, 1);  -- Safe default: 1 CPU
   end Get_CPU_Count;

   --  ========================================================================
   --  Get_Optimal_Task_Count
   --  ========================================================================

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
