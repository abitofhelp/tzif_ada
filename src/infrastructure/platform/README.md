# Infrastructure Platform Layer

## Purpose

The Platform layer provides **OS-specific operations not available in Ada's standard cross-platform libraries** (Ada.Directories, Ada.Environment_Variables, etc.).

Ada's standard library provides excellent cross-platform file operations, but some OS-specific functionality requires direct system calls:

**Not available in Ada.Directories:**
- Reading symbolic link targets (`readlink(2)` on POSIX)
- Windows registry access
- Junction point resolution on Windows
- Platform-specific timezone configuration

**Available in Ada.Directories:** *(Use these instead)*
- File existence checking
- Directory traversal
- File attributes (size, modification time)
- Path composition
- Current directory operations

This layer fills the gaps by providing a generic interface for OS-specific operations, with platform-specific implementations using system calls or OS APIs.

## Overview

The Platform layer uses Ada generics for static dispatch, allowing adapters to work with any platform implementation without runtime overhead.

## Architecture

```
Infrastructure.Platform (Generic Interface)
├── Infrastructure.Platform.POSIX (Linux/macOS/BSD)
└── Infrastructure.Platform.Windows (Windows - Stub)
```

## Generic Interface

The `Infrastructure.Platform` package defines a generic `Platform_Operations` interface:

```ada
generic
   with function Read_Symbolic_Link (Path : String)
     return Platform_String_Result;
package Platform_Operations is
   function Read_Link (Path : String) return Platform_String_Result
     renames Read_Symbolic_Link;
end Platform_Operations;
```

## Platform Implementations

### POSIX (Linux, macOS, BSD)

**Status:** ✅ Fully Implemented  

**Implementation:**
- Uses C binding to POSIX `readlink(2)` syscall
- Reads `/etc/localtime` symlink target
- Extracts IANA zone ID from path (e.g., `/usr/share/zoneinfo/America/Los_Angeles` → `America/Los_Angeles`)

**Files:**
- `posix/infrastructure-platform-posix.ads`
- `posix/infrastructure-platform-posix.adb`

**Platform Support:**
- Linux (all distributions)
- macOS (all versions)
- FreeBSD, OpenBSD, NetBSD

### Windows

**Status:** ⏳ Stub Implementation  

**Current Behavior:**
- Returns `Infrastructure_Error` with message "Not yet implemented"

**Future Implementation Strategy:**

Windows timezone detection requires:

1. **Registry Query:**
   ```
   HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\TimeZoneInformation
   Read "TimeZoneKeyName" value
   ```

2. **Win32 API:**
   - `GetDynamicTimeZoneInformation()` from `kernel32.dll`
   - Returns `TIME_ZONE_INFORMATION` structure

3. **Mapping Table:**
   - Windows TZ names → IANA zone IDs
   - Example: `"Pacific Standard Time"` → `"America/Los_Angeles"`

4. **ICU Library (Windows 10+):**
   - Windows includes International Components for Unicode (ICU)
   - ICU has built-in IANA timezone database

**Required Bindings:**
- Win32 Registry API (`RegOpenKeyEx`, `RegQueryValueEx`, `RegCloseKey`)
- `GetDynamicTimeZoneInformation` from `kernel32.dll`
- Mapping table: Windows TZ name → IANA zone ID

**Files:**
- `windows/infrastructure-platform-windows.ads`
- `windows/infrastructure-platform-windows.adb`

**Platform Support (Future):**
- Windows 10 (1803+)
- Windows 11
- Windows Server 2019+

## How to Use Platform Layer

### In Adapters

Adapters use the instantiated `Operations` package from the desired platform:

```ada
--  File: infrastructure-adapter-file_system-zone_repository.adb

with Infrastructure.Platform.POSIX;  -- or Infrastructure.Platform.Windows

--  Call platform-specific operation via generic interface
Link_Result : constant Infrastructure.Platform.Platform_String_Result :=
  Infrastructure.Platform.POSIX.Operations.Read_Link ("/etc/localtime");
```

### Switching Platforms

To switch between platforms, change the `with` clause and package reference:

**POSIX:**
```ada
with Infrastructure.Platform.POSIX;
...
Result := Infrastructure.Platform.POSIX.Operations.Read_Link (Path);
```

**Windows:**
```ada
with Infrastructure.Platform.Windows;
...
Result := Infrastructure.Platform.Windows.Operations.Read_Link (Path);
```

### Conditional Compilation

For cross-platform builds, use scenario variables in GPR files:

```ada
--  In infrastructure.gpr or similar

type Platform_Type is ("posix", "windows");
Platform : Platform_Type := external ("PLATFORM", "posix");

case Platform is
   when "posix" =>
      for Source_Dirs use (..., "platform/posix");
   when "windows" =>
      for Source_Dirs use (..., "platform/windows");
end case;
```

Then build with:
```bash
alr build -XPLATFORM=posix   # Linux/macOS/BSD
alr build -XPLATFORM=windows # Windows
```

## Design Benefits

1. **Static Dispatch:**
   - Generic instantiation at compile-time
   - Zero runtime overhead
   - Type-safe at compilation

2. **Platform Abstraction:**
   - Adapters don't depend on specific platform implementations
   - Easy to add new platforms (e.g., Android, iOS, embedded systems)

3. **Compile-Time Selection:**
   - Platform chosen at build time
   - No runtime conditionals
   - Optimized machine code for target platform

4. **Bounded Strings:**
   - Uses `Platform_String` (bounded to 4096 characters)
   - Satisfies Ada's definite subtype requirement for Result types
   - PATH_MAX on most POSIX systems is 4096 bytes

## Type Safety

The platform layer uses bounded strings for path operations:

```ada
package Platform_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (4096);
subtype Platform_String is Platform_Strings.Bounded_String;

package String_Result is new Domain.Error.Result.Generic_Result (Platform_String);
subtype Platform_String_Result is String_Result.Result;
```

This ensures:
- No heap allocations
- Compile-time bounds checking
- Compatible with Result monad pattern

## Adding New Platforms

To add support for a new platform (e.g., Android, embedded):

1. Create directory: `platform/myplatform/`
2. Create spec: `infrastructure-platform-myplatform.ads`
3. Create body: `infrastructure-platform-myplatform.adb`
4. Implement `Read_Symbolic_Link` function
5. Instantiate `Platform_Operations` generic
6. Add to `infrastructure.gpr` Source_Dirs
7. Update this README

## Testing

Test platform implementations with the `find_my_id` example:

```bash
cd examples
alr build
./bin/find_my_id_example
```

Expected output (POSIX):
```
Local timezone: America/Los_Angeles
```

Expected output (Windows stub):
```
Error: Windows platform support not yet implemented
```
