pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.Cache.Zone_Cache
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Zone Cache for performance optimization.
--
--  ===========================================================================

package body TZif.Infrastructure.Cache.Zone_Cache is

   use Ada.Calendar;

   --  ========================================================================
   --  Hash Functions
   --  ========================================================================

   function Hash_Zone_ID (ID : Zone_Id_Type) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (To_String (ID));
   end Hash_Zone_ID;

   function Zone_ID_Equal (Left, Right : Zone_Id_Type) return Boolean is
   begin
      return To_String (Left) = To_String (Right);
   end Zone_ID_Equal;

   --  ========================================================================
   --  Zone_Cache Protected Body
   --  ========================================================================

   protected body Zone_Cache_Type is

      --  ===================================================================
      --  Lock-Free Read Operations
      --  ===================================================================

      function Contains (ID : Zone_Id_Type) return Boolean is
      begin
         return Zones.Contains (ID);
      end Contains;

      function Get (ID : Zone_Id_Type) return Zone_Data_Option_Type is
      begin
         if Zones.Contains (ID) then
            --  Return data from cache entry
            declare
               Entry_Val : constant Zone_Cache_Entry := Zones.Element (ID);
            begin
               return TZif_Data_Option.New_Some (Entry_Val.Data);
            end;
         else
            return TZif_Data_Option.None;
         end if;
      end Get;

      function Get_All return Zone_Data_Map_Type is
      begin
         return Zones;
      end Get_All;

      function Size return Natural is
      begin
         return Natural (Zones.Length);
      end Size;

      function Is_Empty return Boolean is
      begin
         return Zones.Is_Empty;
      end Is_Empty;

      --  ===================================================================
      --  Synchronized Write Operations
      --  ===================================================================

      procedure Insert (ID : Zone_Id_Type; Data : TZif_Data_Type) is
         Now       : constant Time             := Clock;
         New_Entry : constant Zone_Cache_Entry :=
           (Data => Data, Last_Accessed => Now);
      begin
         --  Check if we need to evict (LRU) before inserting
         if not Zones.Contains (ID)
           and then Natural (Zones.Length) >= Max_Cache_Size
         then
            --  Find and evict least recently used entry
            declare
               use Zone_Data_Maps;
               Oldest_ID   : Zone_Id_Type;
               Oldest_Time : Time                  := Now;
               Cursor      : Zone_Data_Maps.Cursor := Zones.First;
            begin
               --  Find entry with oldest access time
               while Has_Element (Cursor) loop
                  declare
                     Entry_Val : constant Zone_Cache_Entry := Element (Cursor);
                  begin
                     if Entry_Val.Last_Accessed < Oldest_Time then
                        Oldest_Time := Entry_Val.Last_Accessed;
                        Oldest_ID   := Key (Cursor);
                     end if;
                  end;
                  Next (Cursor);
               end loop;

               --  Evict the oldest entry
               Zones.Delete (Oldest_ID);
            end;
         end if;

         --  Insert or update with current timestamp
         Zones.Include (ID, New_Entry);
      end Insert;

      procedure Remove (ID : Zone_Id_Type) is
      begin
         if Zones.Contains (ID) then
            Zones.Delete (ID);
         end if;
      end Remove;

      procedure Clear is
      begin
         Zones.Clear;
      end Clear;

   end Zone_Cache_Type;

end TZif.Infrastructure.Cache.Zone_Cache;
