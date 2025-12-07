pragma Ada_2022;
--  ===========================================================================
--  TZif.Domain.Value_Object.Iana_Releases
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Iana Releases value object - immutable domain data.
--
--  ===========================================================================

package body TZif.Domain.Value_Object.IANA_Releases is

   --  ========================================================================
   --  Lookup Functions
   --  ========================================================================

   function Is_Known_Version (Version : Version_String_Type) return Boolean is
      Version_Str : constant String := To_String (Version);
   begin
      --  Empty version is not valid
      if Version_Str'Length = 0 then
         return False;
      end if;

      for Release of IANA_Releases loop
         --  Compare version strings (prefix match)
         declare
            Release_Ver : constant String := Release.Version;
         begin
            --  Skip if version string is longer than release version
            if Version_Str'Length <= Release_Ver'Length then
               declare
                  Trimmed : constant String :=
                    Release_Ver
                      (Release_Ver'First ..
                           Release_Ver'First + Version_Str'Length - 1);
               begin
                  if Trimmed = Version_Str then
                     return True;
                  end if;
               end;
            end if;
         end;
      end loop;
      return False;
   end Is_Known_Version;

   function Get_Expected_Zone_Count
     (Version : Version_String_Type) return Natural
   is
      Version_Str : constant String := To_String (Version);
   begin
      --  Empty version returns 0
      if Version_Str'Length = 0 then
         return 0;
      end if;

      for Release of IANA_Releases loop
         declare
            Release_Ver : constant String := Release.Version;
         begin
            --  Skip if version string is longer than release version
            if Version_Str'Length <= Release_Ver'Length then
               declare
                  Trimmed : constant String :=
                    Release_Ver
                      (Release_Ver'First ..
                           Release_Ver'First + Version_Str'Length - 1);
               begin
                  if Trimmed = Version_Str then
                     return Release.Zone_Count;
                  end if;
               end;
            end if;
         end;
      end loop;
      return 0;  -- Unknown version
   end Get_Expected_Zone_Count;

   function Get_Expected_SHA256 (Version : Version_String_Type) return String
   is
      Version_Str : constant String := To_String (Version);
   begin
      --  Empty version returns empty string
      if Version_Str'Length = 0 then
         return "";
      end if;

      for Release of IANA_Releases loop
         declare
            Release_Ver : constant String := Release.Version;
         begin
            --  Skip if version string is longer than release version
            if Version_Str'Length <= Release_Ver'Length then
               declare
                  Trimmed : constant String :=
                    Release_Ver
                      (Release_Ver'First ..
                           Release_Ver'First + Version_Str'Length - 1);
               begin
                  if Trimmed = Version_Str then
                     return Release.SHA256;
                  end if;
               end;
            end if;
         end;
      end loop;
      return "";  -- Unknown version
   end Get_Expected_SHA256;

end TZif.Domain.Value_Object.IANA_Releases;
