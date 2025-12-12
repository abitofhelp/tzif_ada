pragma Ada_2022;
--  ===========================================================================
--  TZif.Infrastructure.Platform.Posix
--  ===========================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    POSIX platform-specific operations.
--
--  Platforms:
--    - Linux (all distributions)
--    - macOS (all versions)
--    - BSD variants (FreeBSD, OpenBSD, NetBSD)
--
--  ===========================================================================

with Interfaces.C;
with Interfaces.C.Strings;
with TZif.Domain.Error;

package body TZif.Infrastructure.Platform.POSIX is

   use Interfaces.C;
   use Interfaces.C.Strings;
   use TZif.Domain.Error;

   --  ========================================================================
   --  C Bindings to POSIX readlink(2)
   --  ========================================================================
   --
   --  ssize_t readlink(const char *pathname, char *buf, size_t bufsiz);
   --
   --  Returns:
   --    Number of bytes placed in buffer on success
   --    -1 on error (sets errno)
   --
   --  Notes:
   --    - Does NOT null-terminate the result
   --    - Buffer size should be PATH_MAX (typically 4096)
   --  ========================================================================

   function C_Readlink
     (Path : chars_ptr; Buffer : chars_ptr; Size : size_t)
      return ptrdiff_t with
     Import => True, Convention => C, External_Name => "readlink";

   --  ========================================================================
   --  Constants
   --  ========================================================================

   PATH_MAX : constant := 4_096;  --  POSIX PATH_MAX

   --  ========================================================================
   --  Read_Symbolic_Link
   --  ========================================================================

   function Read_Symbolic_Link (Path : String) return Platform_String_Result is
      --  Allocate buffer for readlink result
      Buffer : aliased char_array := [0 .. PATH_MAX - 1 => nul];
      Result : ptrdiff_t;
      Path_C : chars_ptr          := New_String (Path);
   begin
      --  Call readlink syscall
      Result :=
        C_Readlink (Path_C, To_Chars_Ptr (Buffer'Unchecked_Access), PATH_MAX);
      Free (Path_C);

      --  Check for error
      if Result < 0 then
         return
           String_Result.Error
             (IO_Error, "Failed to read symbolic link: " & Path);
      end if;

      --  Convert result to Ada string
      --  Note: readlink does NOT null-terminate, so we must use the length
      declare
         Target_Length : constant Natural := Natural (Result);
         Target_Array  : char_array (0 .. size_t (Target_Length - 1));
      begin
         --  Copy only the bytes readlink returned
         for I in 0 .. size_t (Target_Length - 1) loop
            Target_Array (I) := Buffer (I);
         end loop;

         declare
            Target         : constant String          :=
              To_Ada (Target_Array, Trim_Nul => False);
            Target_Bounded : constant Platform_String :=
              Platform_Strings.To_Bounded_String (Target);
         begin
            return String_Result.Ok (Target_Bounded);
         end;
      end;

   exception
      when others =>
         --  DESIGN DECISION: This package is Preelaborate (required for
         --  low-level C bindings). Functional.Try cannot be used here as it
         --  is not preelaborable. The exception handler at this C FFI boundary
         --  converts any unexpected exception to a Result error, which is the
         --  correct railway-oriented pattern for boundary code.
         return
           String_Result.Error
             (IO_Error,
              "Unexpected error reading symbolic link: " & Path);
   end Read_Symbolic_Link;

end TZif.Infrastructure.Platform.POSIX;
