pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.Platform.Windows
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Windows platform-specific operations.
--
--  Requirements:
--    Windows 10 / Windows Server 2022 or later
--
--  Implementation:
--    Uses GetDynamicTimeZoneInformation Win32 API to get the Windows
--    timezone key name, then maps it to an IANA zone ID using CLDR data.
--
--  ===========================================================================

with Interfaces.C;
with TZif.Domain.Error;

package body TZif.Infrastructure.Platform.Windows is

   use Interfaces.C;
   use TZif.Domain.Error;

   --  ========================================================================
   --  Win32 Types and Bindings
   --  ========================================================================
   --
   --  GetDynamicTimeZoneInformation from kernel32.dll
   --  Returns timezone info including TimeZoneKeyName for IANA mapping
   --
   --  Reference:
   --    https://learn.microsoft.com/en-us/windows/win32/api/timezoneapi/
   --  ========================================================================

   subtype WORD is unsigned_short;
   subtype LONG is Interfaces.C.long;
   subtype DWORD is unsigned_long;
   subtype WCHAR is wchar_t;

   type SYSTEMTIME is record
      Year         : WORD;
      Month        : WORD;
      Day_Of_Week  : WORD;
      Day          : WORD;
      Hour         : WORD;
      Minute       : WORD;
      Second       : WORD;
      Milliseconds : WORD;
   end record with
     Convention => C;

   type Wide_TZ_Name is array (0 .. 31) of WCHAR with
     Convention => C;

   type Wide_TZ_Key_Name is array (0 .. 127) of WCHAR with
     Convention => C;

   type DYNAMIC_TIME_ZONE_INFORMATION is record
      Bias                           : LONG;
      Standard_Name                  : Wide_TZ_Name;
      Standard_Date                  : SYSTEMTIME;
      Standard_Bias                  : LONG;
      Daylight_Name                  : Wide_TZ_Name;
      Daylight_Date                  : SYSTEMTIME;
      Daylight_Bias                  : LONG;
      Time_Zone_Key_Name             : Wide_TZ_Key_Name;
      Dynamic_Daylight_Time_Disabled : int;
   end record with
     Convention => C;

   TIME_ZONE_ID_INVALID : constant DWORD := 16#FFFF_FFFF#;

   function GetDynamicTimeZoneInformation
     (Time_Zone_Info : access DYNAMIC_TIME_ZONE_INFORMATION) return DWORD with
     Import        => True,
     Convention    => Stdcall,
     External_Name => "GetDynamicTimeZoneInformation";

   --  ========================================================================
   --  Helper: Wide string to Ada String
   --  ========================================================================

   function Wide_To_String (Wide : Wide_TZ_Key_Name) return String is
      Result : String (1 .. Wide'Length);
      Len    : Natural := 0;
   begin
      for I in Wide'Range loop
         exit when Wide (I) = wchar_t'Val (0);
         Len := Len + 1;
         Result (Len) := Character'Val (wchar_t'Pos (Wide (I)) mod 256);
      end loop;
      return Result (1 .. Len);
   end Wide_To_String;

   --  ========================================================================
   --  CLDR Windows-to-IANA Mapping
   --  ========================================================================
   --
   --  Based on Unicode CLDR windowsZones.xml
   --  https://github.com/unicode-org/cldr/blob/main/common/supplemental/windowsZones.xml
   --
   --  This is a subset of common mappings. Full mapping has ~140 entries.
   --  The mapping returns the "default" IANA zone for each Windows zone.
   --  ========================================================================

   function Map_Windows_To_IANA (Windows_Zone : String) return String is
   begin
      --  Common US timezones
      if Windows_Zone = "Pacific Standard Time" then
         return "America/Los_Angeles";
      elsif Windows_Zone = "Mountain Standard Time" then
         return "America/Denver";
      elsif Windows_Zone = "Central Standard Time" then
         return "America/Chicago";
      elsif Windows_Zone = "Eastern Standard Time" then
         return "America/New_York";
      elsif Windows_Zone = "Alaska Standard Time" then
         return "America/Anchorage";
      elsif Windows_Zone = "Hawaiian Standard Time" then
         return "Pacific/Honolulu";
      elsif Windows_Zone = "Arizona" then
         return "America/Phoenix";

      --  UTC and variants
      elsif Windows_Zone = "UTC" then
         return "Etc/UTC";
      elsif Windows_Zone = "UTC-12" then
         return "Etc/GMT+12";
      elsif Windows_Zone = "UTC-11" then
         return "Etc/GMT+11";
      elsif Windows_Zone = "UTC-02" then
         return "Etc/GMT+2";
      elsif Windows_Zone = "UTC+12" then
         return "Etc/GMT-12";
      elsif Windows_Zone = "UTC+13" then
         return "Etc/GMT-13";
      elsif Windows_Zone = "Coordinated Universal Time" then
         return "Etc/UTC";

      --  Europe
      elsif Windows_Zone = "GMT Standard Time" then
         return "Europe/London";
      elsif Windows_Zone = "W. Europe Standard Time" then
         return "Europe/Berlin";
      elsif Windows_Zone = "Central Europe Standard Time" then
         return "Europe/Budapest";
      elsif Windows_Zone = "Central European Standard Time" then
         return "Europe/Warsaw";
      elsif Windows_Zone = "Romance Standard Time" then
         return "Europe/Paris";
      elsif Windows_Zone = "E. Europe Standard Time" then
         return "Europe/Chisinau";
      elsif Windows_Zone = "FLE Standard Time" then
         return "Europe/Kiev";
      elsif Windows_Zone = "GTB Standard Time" then
         return "Europe/Bucharest";
      elsif Windows_Zone = "Russian Standard Time" then
         return "Europe/Moscow";
      elsif Windows_Zone = "Greenwich Standard Time" then
         return "Atlantic/Reykjavik";

      --  Asia
      elsif Windows_Zone = "China Standard Time" then
         return "Asia/Shanghai";
      elsif Windows_Zone = "Tokyo Standard Time" then
         return "Asia/Tokyo";
      elsif Windows_Zone = "Korea Standard Time" then
         return "Asia/Seoul";
      elsif Windows_Zone = "Singapore Standard Time" then
         return "Asia/Singapore";
      elsif Windows_Zone = "India Standard Time" then
         return "Asia/Kolkata";
      elsif Windows_Zone = "Arabian Standard Time" then
         return "Asia/Dubai";
      elsif Windows_Zone = "Israel Standard Time" then
         return "Asia/Jerusalem";
      elsif Windows_Zone = "SE Asia Standard Time" then
         return "Asia/Bangkok";
      elsif Windows_Zone = "Taipei Standard Time" then
         return "Asia/Taipei";

      --  Australia/Pacific
      elsif Windows_Zone = "AUS Eastern Standard Time" then
         return "Australia/Sydney";
      elsif Windows_Zone = "AUS Central Standard Time" then
         return "Australia/Darwin";
      elsif Windows_Zone = "E. Australia Standard Time" then
         return "Australia/Brisbane";
      elsif Windows_Zone = "Cen. Australia Standard Time" then
         return "Australia/Adelaide";
      elsif Windows_Zone = "W. Australia Standard Time" then
         return "Australia/Perth";
      elsif Windows_Zone = "New Zealand Standard Time" then
         return "Pacific/Auckland";

      --  Americas (non-US)
      elsif Windows_Zone = "Canada Central Standard Time" then
         return "America/Regina";
      elsif Windows_Zone = "Atlantic Standard Time" then
         return "America/Halifax";
      elsif Windows_Zone = "Newfoundland Standard Time" then
         return "America/St_Johns";
      elsif Windows_Zone = "SA Pacific Standard Time" then
         return "America/Bogota";
      elsif Windows_Zone = "SA Eastern Standard Time" then
         return "America/Cayenne";
      elsif Windows_Zone = "E. South America Standard Time" then
         return "America/Sao_Paulo";
      elsif Windows_Zone = "Argentina Standard Time" then
         return "America/Buenos_Aires";
      elsif Windows_Zone = "Central America Standard Time" then
         return "America/Guatemala";
      elsif Windows_Zone = "Mexico Standard Time" then
         return "America/Mexico_City";

      --  Africa
      elsif Windows_Zone = "South Africa Standard Time" then
         return "Africa/Johannesburg";
      elsif Windows_Zone = "Egypt Standard Time" then
         return "Africa/Cairo";
      elsif Windows_Zone = "Morocco Standard Time" then
         return "Africa/Casablanca";
      elsif Windows_Zone = "W. Central Africa Standard Time" then
         return "Africa/Lagos";

      --  Unknown - return empty string
      else
         return "";
      end if;
   end Map_Windows_To_IANA;

   --  ========================================================================
   --  Read_Symbolic_Link
   --  ========================================================================
   --
   --  On Windows, this doesn't read a symlink. Instead, it queries the
   --  Windows timezone and maps it to an IANA zone ID that can be used
   --  to locate TZif files in the user-provided tzdata directory.
   --
   --  The Path parameter is ignored on Windows.
   --  ========================================================================

   function Read_Symbolic_Link (Path : String) return Platform_String_Result is
      pragma Unreferenced (Path);
      TZ_Info : aliased DYNAMIC_TIME_ZONE_INFORMATION;
      Result  : DWORD;
   begin
      --  Query Windows for timezone information
      Result := GetDynamicTimeZoneInformation (TZ_Info'Access);

      if Result = TIME_ZONE_ID_INVALID then
         return
           String_Result.Error
             (IO_Error, "Failed to get Windows timezone information");
      end if;

      --  Extract Windows timezone key name
      declare
         Windows_Zone : constant String :=
           Wide_To_String (TZ_Info.Time_Zone_Key_Name);
         IANA_Zone    : constant String := Map_Windows_To_IANA (Windows_Zone);
      begin
         if IANA_Zone = "" then
            return
              String_Result.Error
                (IO_Error,
                 "Unknown Windows timezone: " & Windows_Zone &
                 ". No IANA mapping available.");
         end if;

         return
           String_Result.Ok (Platform_Strings.To_Bounded_String (IANA_Zone));
      end;

   exception
      when others =>
         return
           String_Result.Error
             (IO_Error, "Unexpected error querying Windows timezone");
   end Read_Symbolic_Link;

end TZif.Infrastructure.Platform.Windows;
