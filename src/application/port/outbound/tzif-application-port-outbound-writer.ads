pragma Ada_2022;
--  ===========================================================================
--  TZif.Application.Port.Outbound.Writer
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Outbound port for Writer.
--
--  Architecture:
--    Application layer port (hexagonal architecture).
--    Defines interface for infrastructure adapters.
--
--  Dependencies:
--    TZif.Domain.Error.Result
--    TZif.Application.Model.Unit
--    Preelaborate
--
--  ===========================================================================

with TZif.Domain.Error.Result;
with TZif.Application.Model.Unit;

package TZif.Application.Port.Outbound.Writer with
  Preelaborate
is

   --  Instantiate Result[Unit] for write operations
   package Unit_Result is new Domain.Error.Result.Generic_Result
     (T => Application.Model.Unit.Unit);

   --  ========================================================================
   --  Generic Writer
   --  ========================================================================

   --  This generic formal function is the OUTPUT PORT CONTRACT
   --
   --  Any infrastructure adapter that wants to provide write output must:
   --  1. Define a function matching this signature
   --  2. Be used to instantiate this generic package
   --
   --  The Message parameter uses String (not bounded) for flexibility at the
   --  boundary - infrastructure adapters handle the conversion

   generic
      with function Write (Message : String) return Unit_Result.Result;
   package Generic_Writer is

      --  Re-export the Write function for use case to call
      function Write_Message
        (Message : String) return Unit_Result.Result renames
        Write;

   end Generic_Writer;

end TZif.Application.Port.Outbound.Writer;
