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

   function Validate_Zone_Id (Id : String) return Result is
   begin
      --  Railway-oriented programming: pure functional validation

      --  Validate: empty string
      if Id'Length = 0 then
         return
           Error
             (Kind => Validation_Error, Message => "Zone ID cannot be empty");
      end if;

      --  Validate: length exceeds maximum
      --  FUNCTIONAL: Check BEFORE calling bounded string constructor
      --  NO EXCEPTIONS: Pre-validate instead of catch
      if Id'Length > TZif_Config.Max_Zone_ID_Length then
         return
           Error
             (Kind    => Validation_Error,
              Message =>
                "Zone ID exceeds maximum length of " &
                TZif_Config.Max_Zone_ID_Length'Image & " characters");
      end if;

      --  All validations passed: construct value object
      --  Safe to call Make_Unchecked because we validated length
      return Ok (Make_Unchecked (Id));
   end Validate_Zone_Id;

end TZif.Domain.Value_Object.Zone_Id.Result;
