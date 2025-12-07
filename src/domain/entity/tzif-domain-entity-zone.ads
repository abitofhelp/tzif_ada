pragma Ada_2022;
--  ===========================================================================
--  TZif.Domain.Entity.Zone
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Zone domain entity with identity.
--
--  Key Types:
--    - Zone_Type
--
--  Key Types:
--    Zone_Type
--
--  Dependencies:
--    TZif.Domain.Value_Object.Zone_Id
--    TZif.Domain.TZif_Data
--
--  ===========================================================================

with TZif.Domain.Value_Object.Zone_Id;
with TZif.Domain.TZif_Data;

package TZif.Domain.Entity.Zone with
  Preelaborate
is

   use TZif.Domain.Value_Object.Zone_Id;
   use TZif.Domain.TZif_Data;

   --  ========================================================================
   --  Zone Entity Type
   --  ========================================================================

   type Zone_Type is record
      --  Zone identifier (e.g., "America/Los_Angeles")
      Id : Zone_Id_Type;

      --  Parsed TZif data (transitions, types, leap seconds, POSIX TZ)
      Data : TZif_Data_Type;
   end record;

   --  ========================================================================
   --  Constructor Functions
   --  ========================================================================

   --  Create a zone entity
   function Make_Zone
     (Id : Zone_Id_Type; Data : TZif_Data_Type) return Zone_Type is
     (Id => Id, Data => Data);

   --  Create a zone entity from string ID
   function Make_Zone (Id : String; Data : TZif_Data_Type) return Zone_Type is
     (Id => Make_Zone_Id (Id), Data => Data);

   --  ========================================================================
   --  Query Functions
   --  ========================================================================

   --  Get zone identifier
   function Get_Id (Zone : Zone_Type) return Zone_Id_Type is (Zone.Id);

   --  Get zone identifier as string
   function Get_Id_String (Zone : Zone_Type) return String is
     (To_String (Zone.Id));

   --  Get TZif data
   function Get_Data (Zone : Zone_Type) return TZif_Data_Type is (Zone.Data);

   --  Check if zone has transitions
   function Has_Transitions (Zone : Zone_Type) return Boolean is
     (Domain.TZif_Data.Has_Transitions (Zone.Data));

   --  Get transition count
   function Transition_Count (Zone : Zone_Type) return Natural is
     (Domain.TZif_Data.Transition_Count (Zone.Data));

   --  ========================================================================
   --  Entity Identity (Equality based on Zone_Id)
   --  ========================================================================

   --  Two zones are equal if they have the same Zone_Id
   --  (Entity identity, not value equality)
   overriding function "=" (Left, Right : Zone_Type) return Boolean is
     (Left.Id = Right.Id);

   --  Check if zone has specific ID
   function Has_Id (Zone : Zone_Type; Id : Zone_Id_Type) return Boolean is
     (Zone.Id = Id);

   --  Check if zone has specific ID (string)
   function Has_Id (Zone : Zone_Type; Id : String) return Boolean is
     (To_String (Zone.Id) = Id);

end TZif.Domain.Entity.Zone;
