pragma Ada_2022;
--  ===========================================================================
--  TZif.Application.Port.Inbound.List_All_Order_By_Id
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Inbound port for List All Order By Id use case.
--
--  Architecture:
--    Application layer port (hexagonal architecture).
--    Defines interface for external actors to trigger use cases.
--
--  Key Types:
--    Zone_Id_List
--    List_All_Zones_Result
--
--  Dependencies:
--    TZif.Domain.Error.Result
--    TZif.Domain.Value_Object.Zone_Id
--    Preelaborate
--
--  ===========================================================================

with Ada.Containers.Vectors;
with TZif.Domain.Error.Result;
with TZif.Domain.Value_Object.Zone_Id;

package TZif.Application.Port.Inbound.List_All_Order_By_Id with
  Preelaborate
is

   use TZif.Domain.Value_Object.Zone_Id;

   --  ========================================================================
   --  Canonical Types (GPT-5 Pattern: defined ONCE, used everywhere)
   --  ========================================================================

   --  Zone ID collection type
   package Zone_Id_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Zone_Id_Type, "=" => "=");
   subtype Zone_Id_List is Zone_Id_Vectors.Vector;

   --  Result type: Result[Zone_Id_List]
   package List_All_Zones_Result_Package is new Domain.Error.Result
     .Generic_Result
     (T => Zone_Id_List);
   subtype List_All_Zones_Result is List_All_Zones_Result_Package.Result;

   --  ========================================================================
   --  Port Contract Documentation
   --  ========================================================================
   --
   --  The Execute function signature (implemented by use case generic):
   --
   --    function Execute
   --      (Source     : Source_Info_Type;
   --       Descending : Boolean)
   --      return List_All_Zones_Result;
   --
   --  Parameters:
   --    Source - The timezone database source to query
   --    Descending - Sort order (false=ascending A-Z, true=descending Z-A)
   --
   --  Returns:
   --    Ok(Zone_Id_List) - List of zone IDs sorted as requested
   --    Err(SourceNotFound) - Source ULID not in cache
   --    Err(IOError) - Filesystem error
   --
   --  Performance: O(n log n) for sorting, cached after first use

end TZif.Application.Port.Inbound.List_All_Order_By_Id;
