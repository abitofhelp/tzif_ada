pragma Ada_2022;
--  ===========================================================================
--  Tzif.Infrastructure.Ulid
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Ulid interface and type definitions.
--
--  Dependencies:
--    TZif.Domain.Value_Object.Source_Info
--
--  ===========================================================================

with TZif.Domain.Value_Object.Source_Info;

package TZif.Infrastructure.ULID is

   use TZif.Domain.Value_Object.Source_Info;

   --  Generate a new ULID
   function Generate return ULID_Type;

   --  Generate deterministic ULID from seed (for testing)
   function Generate_From_Seed (Seed : String) return ULID_Type;

end TZif.Infrastructure.ULID;
