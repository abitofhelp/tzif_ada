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
--  Implementation Notes:
--    Uses Functional.Try.Map_To_Result_With_Param to wrap exception-prone
--    C FFI operations, converting exceptions to Result types for
--    railway-oriented error handling.
--
--  ===========================================================================

with Functional.Try.Map_To_Result_With_Param;
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
   --  Make Error (used by Map_To_Result)
   --  ========================================================================

   function Make_Readlink_Error
     (Kind : Error_Kind; Message : String) return Platform_String_Result
   is
   begin
      return String_Result.Error (Kind, Message);
   end Make_Readlink_Error;

   --  ========================================================================
   --  Raw_Read_Symbolic_Link
   --
   --  Raw action that may raise exceptions - wrapped by Map_To_Result
   --  ========================================================================

   function Raw_Read_Symbolic_Link
     (Path : String) return Platform_String_Result
   is
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
   end Raw_Read_Symbolic_Link;

   --  ========================================================================
   --  Map_To_Result wrapper for C FFI boundary
   --  ========================================================================

   package Try_Readlink is new Functional.Try.Map_To_Result_With_Param
     (Error_Kind_Type    => Error_Kind,
      Param_Type         => String,
      Result_Type        => Platform_String_Result,
      Make_Error         => Make_Readlink_Error,
      Default_Error_Kind => IO_Error,
      Action             => Raw_Read_Symbolic_Link);

   --  Empty mappings: all exceptions map to IO_Error (default)
   Readlink_Mappings : constant Try_Readlink.Mapping_Array :=
     Try_Readlink.Empty_Mappings;

   --  ========================================================================
   --  Read_Symbolic_Link (public API)
   --  ========================================================================

   function Read_Symbolic_Link (Path : String) return Platform_String_Result is
   begin
      return Try_Readlink.Run (Path, Readlink_Mappings);
   end Read_Symbolic_Link;

end TZif.Infrastructure.Platform.POSIX;
