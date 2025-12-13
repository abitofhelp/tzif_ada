pragma Ada_2022;
--  ===========================================================================
--  TZif.Domain.Value_Object.Zone_Id.Result
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Result value object - immutable domain data.
--
--  ===========================================================================

with TZif_Config;

package body TZif.Domain.Value_Object.Zone_Id.Result is

   use TZif.Domain.Error;

   function Make_Zone_Id (Id : String) return Result is
   begin
      --  Validate using value object's business rules (single source of truth)
      if not Is_Valid (Id) then
         if Id'Length = 0 then
            return
              Error
                (Kind    => Validation_Error,
                 Message => "Zone ID cannot be empty");
         else
            return
              Error
                (Kind    => Validation_Error,
                 Message =>
                   "Zone ID exceeds maximum length of" &
                   TZif_Config.Max_Zone_ID_Length'Image & " characters");
         end if;
      end if;

      --  Validation passed: construct value object
      return Ok (Make_Unchecked (Id));
   end Make_Zone_Id;

end TZif.Domain.Value_Object.Zone_Id.Result;
