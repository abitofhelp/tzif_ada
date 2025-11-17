pragma Ada_2022;
--  ===========================================================================
--  Tzif.Infrastructure.Cache.Json_Serialization
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Json Serialization for performance optimization.
--
--  ===========================================================================

with Ada.Calendar;
with Ada.Calendar.Formatting;
with GNAT.OS_Lib;
with TZif.Domain.Value_Object.Epoch_Seconds;
with TZif.Domain.Value_Object.UTC_Offset;

package body TZif.Infrastructure.Cache.JSON_Serialization is

   use TZif.Domain.Value_Object.Epoch_Seconds;
   use TZif.Domain.Value_Object.UTC_Offset;

   --  ========================================================================
   --  Platform Detection
   --  ========================================================================

   function Get_Platform return String is
   begin
      --  Use GNAT.OS_Lib to detect platform
      case GNAT.OS_Lib.Directory_Separator is
         when '/' =>
            --  Unix-like system, check if Darwin
            if GNAT.OS_Lib.Getenv ("OSTYPE").all = "darwin" then
               return "darwin    ";
            else
               return "linux     ";
            end if;

         when '\' =>
            return "windows   ";

         when others =>
            return "unknown   ";
      end case;
   end Get_Platform;

   --  ========================================================================
   --  Cache Header Serialization
   --  ========================================================================

   function To_JSON (Header : Cache_Header) return JSON_Value is
      Obj       : constant JSON_Value        := Create_Object;
      Now       : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Timestamp : constant String := Ada.Calendar.Formatting.Image (Now);
   begin
      Obj.Set_Field ("magic", Create (Header.Magic));
      Obj.Set_Field ("version", Create (Integer (Header.Version)));
      Obj.Set_Field ("created_at", Create (Timestamp));
      Obj.Set_Field ("platform", Create (Header.Platform));
      Obj.Set_Field ("library_version", Create (Header.Library_Version));

      --  Metadata fields (v2+)
      Obj.Set_Field
        ("expected_sources", Create (Integer (Header.Expected_Sources)));
      Obj.Set_Field
        ("expected_zones", Create (Integer (Header.Expected_Zones)));
      Obj.Set_Field
        ("actual_sources", Create (Integer (Header.Actual_Sources)));
      Obj.Set_Field ("actual_zones", Create (Integer (Header.Actual_Zones)));

      return Obj;
   end To_JSON;

   function From_JSON (J : JSON_Value) return Cache_Header is
      Magic    : constant String  := J.Get ("magic");
      Version  : constant Integer := J.Get ("version");
      Platform : constant String  := J.Get ("platform");
      Lib_Ver  : constant String  := J.Get ("library_version");

      --  Metadata fields (v2+, backward compatible with v1)
      Expected_Sources : constant Natural :=
        (if J.Has_Field ("expected_sources") then
           Natural (Integer'(J.Get ("expected_sources")))
         else 0);
      Expected_Zones   : constant Natural :=
        (if J.Has_Field ("expected_zones") then
           Natural (Integer'(J.Get ("expected_zones")))
         else 0);
      Actual_Sources   : constant Natural :=
        (if J.Has_Field ("actual_sources") then
           Natural (Integer'(J.Get ("actual_sources")))
         else 0);
      Actual_Zones     : constant Natural :=
        (if J.Has_Field ("actual_zones") then
           Natural (Integer'(J.Get ("actual_zones")))
         else 0);
   begin
      return
        (Magic            => Magic (1 .. 10), Version => Positive (Version),
         Platform => Platform (1 .. 10), Library_Version => Lib_Ver (1 .. 10),
         Expected_Sources => Expected_Sources,
         Expected_Zones   => Expected_Zones, Actual_Sources => Actual_Sources,
         Actual_Zones     => Actual_Zones);
   end From_JSON;

   function Is_Valid_Cache_Header (J : JSON_Value) return Boolean is
   begin
      if not J.Has_Field ("magic") then
         return False;
      end if;

      if not J.Has_Field ("version") then
         return False;
      end if;

      declare
         Magic : constant String := J.Get ("magic");
      begin
         return Magic = "TZIF_CACHE";
      end;
   exception
      when others =>
         return False;
   end Is_Valid_Cache_Header;

   --  ========================================================================
   --  Source_Info Serialization
   --  ========================================================================

   function To_JSON (Source : Source_Info_Type) return JSON_Value is
      Obj : constant JSON_Value := Create_Object;
   begin
      Obj.Set_Field ("ulid", Create (To_String (Get_ULID (Source))));
      Obj.Set_Field ("path", Create (To_String (Get_Path (Source))));
      Obj.Set_Field ("version", Create (To_String (Get_Version (Source))));
      Obj.Set_Field ("zone_count", Create (Integer (Get_Zone_Count (Source))));
      return Obj;
   end To_JSON;

   function From_JSON (J : JSON_Value) return Source_Info_Type is
      ULID_Str : constant String  := J.Get ("ulid");
      Path_Str : constant String  := J.Get ("path");
      Ver_Str  : constant String  := J.Get ("version");
      Count    : constant Integer := J.Get ("zone_count");
   begin
      return
        Make_Source_Info
          (ULID    => Make_ULID (ULID_Str), Path => Make_Path (Path_Str),
           Version => Make_Version (Ver_Str), Zone_Count => Natural (Count));
   end From_JSON;

   --  ========================================================================
   --  TZif_Header Serialization
   --  ========================================================================

   function To_JSON (Header : TZif_Header_Type) return JSON_Value is
      Obj         : constant JSON_Value := Create_Object;
      Version_Str : String (1 .. 9);
   begin
      --  Convert version enum to string
      case Header.Version is
         when Version_1 =>
            Version_Str := "version_1";

         when Version_2 =>
            Version_Str := "version_2";

         when Version_3 =>
            Version_Str := "version_3";

         when Version_4 =>
            Version_Str := "version_4";
      end case;

      Obj.Set_Field ("version", Create (Version_Str));
      Obj.Set_Field
        ("utc_local_count", Create (Integer (Header.UTC_Local_Count)));
      Obj.Set_Field
        ("standard_wall_count", Create (Integer (Header.Standard_Wall_Count)));
      Obj.Set_Field ("leap_count", Create (Integer (Header.Leap_Count)));
      Obj.Set_Field
        ("transition_count", Create (Integer (Header.Transition_Count)));
      Obj.Set_Field ("type_count", Create (Integer (Header.Type_Count)));
      Obj.Set_Field ("abbrev_chars", Create (Integer (Header.Abbrev_Chars)));
      return Obj;
   end To_JSON;

   function From_JSON (J : JSON_Value) return TZif_Header_Type is
      Version_Str : constant String := J.Get ("version");
      Version     : TZif_Version_Type;
   begin
      --  Convert string back to enum
      if Version_Str = "version_1" then
         Version := Version_1;
      elsif Version_Str = "version_2" then
         Version := Version_2;
      elsif Version_Str = "version_3" then
         Version := Version_3;
      elsif Version_Str = "version_4" then
         Version := Version_4;
      else
         Version := Version_1;  --  Default fallback
      end if;

      return
        (Version             => Version,
         UTC_Local_Count     => Natural (Integer'(J.Get ("utc_local_count"))),
         Standard_Wall_Count =>
           Natural (Integer'(J.Get ("standard_wall_count"))),
         Leap_Count          => Natural (Integer'(J.Get ("leap_count"))),
         Transition_Count    => Natural (Integer'(J.Get ("transition_count"))),
         Type_Count          => Natural (Integer'(J.Get ("type_count"))),
         Abbrev_Chars        => Natural (Integer'(J.Get ("abbrev_chars"))));
   end From_JSON;

   --  ========================================================================
   --  Transition Serialization
   --  ========================================================================

   function To_JSON (Trans : Transition_Type) return JSON_Value is
      Obj : constant JSON_Value := Create_Object;
   begin
      Obj.Set_Field ("time", Create (Long_Integer (Trans.Time)));
      Obj.Set_Field ("type_index", Create (Integer (Trans.Type_Index)));
      return Obj;
   end To_JSON;

   function From_JSON (J : JSON_Value) return Transition_Type is
      Time_Val : constant Long_Integer := J.Get ("time");
      Index    : constant Integer      := J.Get ("type_index");
   begin
      return
        (Time => Epoch_Seconds_Type (Time_Val), Type_Index => Natural (Index));
   end From_JSON;

   --  ========================================================================
   --  Timezone_Type Serialization
   --  ========================================================================

   function To_JSON (TZ_Type : Timezone_Type_Record) return JSON_Value is
      Obj : constant JSON_Value := Create_Object;
   begin
      Obj.Set_Field ("utc_offset", Create (Integer (TZ_Type.UTC_Offset)));
      Obj.Set_Field ("is_dst", Create (TZ_Type.Is_DST));
      Obj.Set_Field ("abbreviation", Create (Get_Abbreviation (TZ_Type)));
      return Obj;
   end To_JSON;

   function From_JSON (J : JSON_Value) return Timezone_Type_Record is
      Offset : constant Integer := J.Get ("utc_offset");
      Is_DST : constant Boolean := J.Get ("is_dst");
      Abbrev : constant String  := J.Get ("abbreviation");
   begin
      return
        Make_Timezone_Type
          (UTC_Offset   => UTC_Offset_Type (Offset), Is_DST => Is_DST,
           Abbreviation => Abbrev);
   end From_JSON;

   --  ========================================================================
   --  Leap_Second Serialization
   --  ========================================================================

   function To_JSON (Leap : Leap_Second_Type) return JSON_Value is
      Obj : constant JSON_Value := Create_Object;
   begin
      Obj.Set_Field
        ("occurrence_time", Create (Long_Integer (Leap.Occurrence_Time)));
      Obj.Set_Field ("leap_count", Create (Leap.Leap_Count));
      return Obj;
   end To_JSON;

   function From_JSON (J : JSON_Value) return Leap_Second_Type is
      Occurrence : constant Long_Integer := J.Get ("occurrence_time");
      Count      : constant Integer      := J.Get ("leap_count");
   begin
      return
        (Occurrence_Time => Epoch_Seconds_Type (Occurrence),
         Leap_Count      => Count);
   end From_JSON;

   --  ========================================================================
   --  TZif_Data Serialization
   --  ========================================================================

   function To_JSON (Data : TZif_Data_Type) return JSON_Value is
      Obj              : constant JSON_Value := Create_Object;
      Transitions_Arr  : constant JSON_Value := Create (Empty_Array);
      Types_Arr        : constant JSON_Value := Create (Empty_Array);
      Leap_Seconds_Arr : constant JSON_Value := Create (Empty_Array);
   begin
      --  Serialize header
      Obj.Set_Field ("header", To_JSON (Data.Header));

      --  Serialize transitions
      for Cursor in Data.Transitions.Iterate loop
         Append
           (Transitions_Arr, To_JSON (Transition_Vectors.Element (Cursor)));
      end loop;
      Obj.Set_Field ("transitions", Transitions_Arr);

      --  Serialize types
      for Cursor in Data.Timezone_Types.Iterate loop
         Append (Types_Arr, To_JSON (Timezone_Type_Vectors.Element (Cursor)));
      end loop;
      Obj.Set_Field ("types", Types_Arr);

      --  Serialize leap seconds
      for Cursor in Data.Leap_Seconds.Iterate loop
         Append
           (Leap_Seconds_Arr, To_JSON (Leap_Second_Vectors.Element (Cursor)));
      end loop;
      Obj.Set_Field ("leap_seconds", Leap_Seconds_Arr);

      --  Serialize POSIX TZ string
      Obj.Set_Field ("posix_tz", Create (Get_POSIX_TZ (Data)));

      return Obj;
   end To_JSON;

   function From_JSON (J : JSON_Value) return TZif_Data_Type is
      Data : TZif_Data_Type;

      --  Get header
      Header_JSON : constant JSON_Value := J.Get ("header");

      --  Get arrays
      Transitions_JSON  : constant JSON_Array := Get (J, "transitions");
      Types_JSON        : constant JSON_Array := Get (J, "types");
      Leap_Seconds_JSON : constant JSON_Array := Get (J, "leap_seconds");
      POSIX_TZ_Str      : constant String     := J.Get ("posix_tz");

      --  Array lengths
      Trans_Len : constant Natural := Length (Transitions_JSON);
      Types_Len : constant Natural := Length (Types_JSON);
      Leap_Len  : constant Natural := Length (Leap_Seconds_JSON);
   begin
      --  Deserialize header
      Data.Header := From_JSON (Header_JSON);

      --  Deserialize transitions
      for I in 1 .. Trans_Len loop
         declare
            Trans_JSON : constant JSON_Value      := Get (Transitions_JSON, I);
            Trans      : constant Transition_Type := From_JSON (Trans_JSON);
         begin
            Data.Transitions.Append (Trans);
         end;
      end loop;

      --  Deserialize types
      for I in 1 .. Types_Len loop
         declare
            Type_JSON : constant JSON_Value           := Get (Types_JSON, I);
            TZ_Type   : constant Timezone_Type_Record := From_JSON (Type_JSON);
         begin
            Data.Timezone_Types.Append (TZ_Type);
         end;
      end loop;

      --  Deserialize leap seconds
      for I in 1 .. Leap_Len loop
         declare
            Leap_JSON : constant JSON_Value := Get (Leap_Seconds_JSON, I);
            Leap      : constant Leap_Second_Type := From_JSON (Leap_JSON);
         begin
            Data.Leap_Seconds.Append (Leap);
         end;
      end loop;

      --  Deserialize POSIX TZ string
      Data.POSIX_TZ := POSIX_TZ_Strings.To_Bounded_String (POSIX_TZ_Str);

      return Data;
   end From_JSON;

end TZif.Infrastructure.Cache.JSON_Serialization;
