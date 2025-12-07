pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.Adapter.File_System.Zone_Repository
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Zone Repository infrastructure adapter (generic).
--
--  Architecture:
--    Infrastructure layer adapter (hexagonal architecture).
--    Implements outbound ports for external systems.
--    Generic over platform operations to support cross-platform builds.
--
--  Design Pattern:
--    Dependency Inversion via generic formal package. The repository
--    depends on the Platform_Operations abstraction, not concrete
--    platform implementations (POSIX, Windows).
--
--  Dependencies:
--    TZif.Domain.Value_Object.Zone_Id
--    TZif.Domain.Value_Object.Epoch_Seconds
--    TZif.Application.Port.Outbound.Zone_Repository
--    TZif.Infrastructure.Platform (formal package)
--
--  ===========================================================================

with TZif.Domain.Value_Object.Zone_Id;
with TZif.Domain.Value_Object.Epoch_Seconds;
with TZif.Application.Port.Outbound.Zone_Repository;
with TZif.Infrastructure.Platform;

generic
   --  ========================================================================
   --  Formal Package: Platform Operations
   --  ========================================================================
   --  Inject platform-specific operations at instantiation time.
   --  Desktop: TZif.Infrastructure.Platform.POSIX.Operations
   --  Windows: TZif.Infrastructure.Platform.Windows.Operations
   --  ========================================================================
   with package Platform_Ops is new
     TZif.Infrastructure.Platform.Platform_Operations (<>);

package TZif.Infrastructure.Adapter.File_System.Zone_Repository is

   use TZif.Domain.Value_Object.Zone_Id;
   use TZif.Domain.Value_Object.Epoch_Seconds;
   use TZif.Application.Port.Outbound.Zone_Repository;

   --  ========================================================================
   --  Repository Operations (implement port interface)
   --  ========================================================================

   --  Find zone by ID (search filesystem, parse TZif file)
   function Find_By_Id (Id : Zone_Id_Type) return Repository_Zone_Result;

   --  Check if zone exists (file exists on filesystem)
   function Exists (Id : Zone_Id_Type) return Repository_Boolean_Result;

   --  Get TZif version for zone (parse header only)
   function Get_Version (Id : Zone_Id_Type) return Repository_Version_Result;

   --  Get transition info at specific epoch time
   function Get_Transition_At_Epoch
     (Id : Zone_Id_Type; Epoch_Time : Epoch_Seconds_Type)
      return Repository_Transition_Info_Result;

   --  Find local system timezone ID
   function Find_My_Id return Repository_Zone_Id_Result;

   --  List all available timezone IDs
   function List_All_Zones
     (Sort_Order : Sort_Order_Type) return Repository_Zone_List_Result;

   --  List all available timezone database sources
   function List_Sources return Repository_Source_List_Result;

   --  ========================================================================
   --  Instantiate Repository Port
   --  ========================================================================

   package Repository is new Application.Port.Outbound.Zone_Repository
     .Repository_Port
     (Find_By_Id => Find_By_Id, Exists => Exists, Get_Version => Get_Version,
      Get_Transition_At_Epoch => Get_Transition_At_Epoch,
      Find_My_Id              => Find_My_Id, List_All_Zones => List_All_Zones,
      List_Sources            => List_Sources);

end TZif.Infrastructure.Adapter.File_System.Zone_Repository;
