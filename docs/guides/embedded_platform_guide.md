# Embedded Platform Guide

**Version:** 1.0.0
**Date:** December 06, 2025
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

---

## Table of Contents

- [Overview](#overview)
- [Supported Platforms](#supported-platforms)
- [Architecture](#architecture)
- [Environment Variables](#environment-variables)
- [Getting Started](#getting-started)
- [STM32F769I-DK Example](#stm32f769i-dk-example)
- [STM32H7S78-DK Example](#stm32h7s78-dk-example)
- [RAM Filesystem Setup](#ram-filesystem-setup)
- [API Usage](#api-usage)
- [Build Configuration](#build-configuration)
- [Memory Considerations](#memory-considerations)
- [Ravenscar Profile Compatibility](#ravenscar-profile-compatibility)
- [Troubleshooting](#troubleshooting)

---

## Overview

TZif provides a dedicated embedded platform adapter for use on resource-constrained systems. Unlike the Desktop adapter (which uses POSIX symlinks for system timezone detection), the Embedded adapter uses environment variables for configuration, making it suitable for:

- ARM Cortex-M microcontrollers
- Ravenscar-profile applications
- Systems with RAM-based filesystems
- Custom embedded Linux targets

**Key Differences from Desktop:**

| Feature | Desktop | Embedded |
|---------|---------|----------|
| System timezone detection | POSIX symlink (`/etc/localtime`) | Environment variable |
| Timezone data location | Standard POSIX paths | Environment variable |
| Default timezone | Detected from system | UTC |
| Runtime | Full Ada | Ravenscar compatible |

---

## Supported Platforms

### Tested Boards

| Board | RAM | Storage | Profile |
|-------|-----|---------|---------|
| **STM32F769I-DK** | 512 KB + 16 MB SDRAM | RAM filesystem | `embedded` |
| **STM32H7S78-DK** | 620 KB + 32 MB PSRAM | RAM filesystem | `stm32h7s78` |
| **STM32MP135F-DK** | 512 MB DDR | eMMC/SD | `stm32mp135_linux` |

### General Requirements

- ARM Cortex-M4 or higher (for embedded profiles)
- Minimum 512 KB RAM (for embedded profile)
- Minimum 128 KB RAM (for baremetal profile)
- Filesystem access (RAM-based or flash-based)

---

## Architecture

The embedded adapter follows the hexagonal architecture pattern:

```
┌─────────────────────────────────────────────────────────────────┐
│                    TZif.API.Embedded                            │
│                    (Composition Root)                           │
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ Instantiates with
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│              TZif.Application.Operations                        │
│              (Generic Application Layer)                        │
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ Formal package parameters
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│           TZif.Infrastructure.IO.Embedded                       │
│           (Embedded I/O Adapter)                                │
│                                                                 │
│   - Read_File: Reads TZif binary from filesystem                │
│   - Read_System_Timezone_Id: Returns TZIF_SYSTEM_ZONE or UTC    │
│   - List_Directory_Sources: Scans TZIF_DATA_PATH                │
│   - Get_Modified_Time: File timestamp queries                   │
└─────────────────────────────────────────────────────────────────┘
```

---

## Environment Variables

The Embedded adapter uses two environment variables for configuration:

### TZIF_DATA_PATH (Required)

Path to the directory containing IANA timezone data.

```bash
export TZIF_DATA_PATH="/mnt/tzdata"
```

**Contents expected:**
```
/mnt/tzdata/
├── America/
│   ├── New_York
│   ├── Los_Angeles
│   └── ...
├── Europe/
│   ├── London
│   ├── Paris
│   └── ...
├── Asia/
│   └── ...
├── UTC
├── version          # Optional: IANA release version
└── ...
```

### TZIF_SYSTEM_ZONE (Optional)

The system's local timezone identifier. Defaults to `UTC` if not set.

```bash
export TZIF_SYSTEM_ZONE="America/New_York"
```

**Valid values:** Any IANA timezone identifier (e.g., `UTC`, `America/Chicago`, `Europe/Berlin`)

---

## Getting Started

### Step 1: Import the Embedded API

In your Ada code, import `TZif.API.Embedded` instead of `TZif.API`:

```ada
with TZif.API.Embedded;

procedure My_Embedded_App is
   use TZif.API.Embedded.API;  --  Access facade operations
begin
   --  Your timezone operations here
   null;
end My_Embedded_App;
```

### Step 2: Set Environment Variables

Before running your application, configure the environment:

```bash
export TZIF_DATA_PATH="/path/to/tzdata"
export TZIF_SYSTEM_ZONE="America/New_York"  # Optional
```

### Step 3: Build with Embedded Profile

```bash
alr build -- -XTZIF_PROFILE=embedded
```

---

## STM32F769I-DK Example

### Hardware Setup

The STM32F769I-DK board provides:
- 512 KB internal SRAM
- 16 MB external SDRAM
- 16 MB QSPI flash
- LCD touchscreen (800x480)

### Memory Map

```
Internal SRAM (512 KB)  → Application code, stack, heap
SDRAM (16 MB)           → Timezone data (RAM filesystem)
QSPI Flash (16 MB)      → Persistent timezone storage (optional)
```

### Loading Timezone Data

Create a RAM filesystem loader that copies tzdata from QSPI flash to SDRAM:

```ada
with TZif.API.Embedded;
with Ada.Text_IO;

procedure STM32F769_TZif_Demo is
   use TZif.API.Embedded.API;
   use Ada.Text_IO;

   --  Environment variables set in startup code or configuration
   --  TZIF_DATA_PATH = "/ram/tzdata"
   --  TZIF_SYSTEM_ZONE = "America/Chicago"
begin
   --  Find local timezone (reads TZIF_SYSTEM_ZONE)
   declare
      Result : constant My_Zone_Result := Find_My_Id;
   begin
      if Is_Ok (Result) then
         Put_Line ("Local timezone: " & To_String (Value (Result)));
      else
         Put_Line ("Using default: UTC");
      end if;
   end;

   --  Look up a specific timezone
   declare
      Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("America/Chicago");
      Result  : constant Zone_Result := Find_By_Id (Zone_Id);
   begin
      if Is_Ok (Result) then
         Put_Line ("Found: " & To_String (Zone_Id));
      else
         Put_Line ("Zone not found in RAM filesystem");
      end if;
   end;
end STM32F769_TZif_Demo;
```

### GPR Configuration

```ada
with "tzif.gpr";

project STM32F769_Demo is
   for Target use "arm-eabi";
   for Runtime ("Ada") use "embedded-stm32f769disco";

   for Source_Dirs use ("src");
   for Object_Dir use "obj";
   for Exec_Dir use "bin";
   for Main use ("stm32f769_tzif_demo.adb");
end STM32F769_Demo;
```

Build command:
```bash
gprbuild -P stm32f769_demo.gpr -XTZIF_PROFILE=embedded
```

---

## STM32H7S78-DK Example

### Hardware Setup

The STM32H7S78-DK board provides:
- 620 KB internal RAM
- 32 MB external PSRAM (HyperRAM)
- 64 MB OSPI flash
- LCD touchscreen (800x480)

### Memory Configuration

The `stm32h7s78` profile is optimized for this board:

```ada
--  config/profiles/stm32h7s78/tzif_config.ads
package TZif_Config is
   pragma Pure;

   Profile_Name    : constant String := "stm32h7s78";
   Target_Platform : constant String := "STM32H7S78-DK";

   --  Bounded string configuration (smaller for embedded)
   Max_Zone_Id_Length : constant := 64;
   Max_Path_Length    : constant := 128;
   Max_Error_Length   : constant := 256;

   --  Zone limits
   Max_Zones       : constant := 100;   --  Reduced from desktop
   Max_Transitions : constant := 200;   --  Reduced from desktop

   --  Runtime configuration
   Enable_Contracts : constant Boolean := True;
   Enable_Debug     : constant Boolean := True;
end TZif_Config;
```

### Build Command

```bash
alr build -- -XTZIF_PROFILE=stm32h7s78
```

---

## RAM Filesystem Setup

For boards without persistent storage, load timezone data into RAM at boot.

### Option 1: Compile Data into Binary

```ada
package TZif_Embedded_Data is
   --  Compiled timezone data as byte arrays
   America_New_York : constant array (1 .. 3584) of Interfaces.Unsigned_8 := (
      16#54#, 16#5A#, 16#69#, 16#66#, ...  --  TZif magic + header
   );

   America_Los_Angeles : constant array (1 .. 2856) of Interfaces.Unsigned_8 := (
      16#54#, 16#5A#, 16#69#, 16#66#, ...
   );

   --  ... other zones
end TZif_Embedded_Data;
```

### Option 2: Load from Flash at Boot

```ada
procedure Initialize_TZif_RAM_Filesystem is
   use type Interfaces.Unsigned_8;

   type Flash_Address is mod 2**32;

   TZif_Flash_Base : constant Flash_Address := 16#9000_0000#;  --  QSPI mapped
   TZif_RAM_Base   : constant System.Address := ...;           --  SDRAM location
begin
   --  Copy timezone data from flash to RAM
   --  Set TZIF_DATA_PATH to point to RAM location
   Ada.Environment_Variables.Set
     ("TZIF_DATA_PATH", "/ram/tzdata");
end Initialize_TZif_RAM_Filesystem;
```

### Option 3: Minimal Zone Set

For extremely constrained systems, include only essential zones:

```
/ram/tzdata/
├── UTC              --  Always include
├── America/
│   └── New_York     --  Primary US zone
├── Europe/
│   └── London       --  Primary EU zone
└── Asia/
    └── Tokyo        --  Primary Asia zone
```

This reduces memory footprint from ~4 MB (full tzdata) to ~50 KB.

---

## API Usage

### Finding Local Timezone

```ada
with TZif.API.Embedded;
use TZif.API.Embedded.API;

procedure Get_Local_Zone is
   Result : constant My_Zone_Result := Find_My_Id;
begin
   if Is_Ok (Result) then
      --  Returns TZIF_SYSTEM_ZONE value or "UTC" if not set
      Ada.Text_IO.Put_Line ("Local: " & To_String (Value (Result)));
   end if;
end Get_Local_Zone;
```

### Looking Up Specific Timezone

```ada
with TZif.API.Embedded;
use TZif.API.Embedded.API;

procedure Lookup_Zone is
   Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("Europe/Paris");
   Result  : constant Zone_Result := Find_By_Id (Zone_Id);
begin
   if Is_Ok (Result) then
      --  Zone found in TZIF_DATA_PATH
      null;
   elsif Is_Error (Result) then
      --  Zone not found or data path not configured
      null;
   end if;
end Lookup_Zone;
```

### Getting Transition Information

```ada
with TZif.API.Embedded;
use TZif.API.Embedded.API;

procedure Get_Offset is
   Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("America/Chicago");
   Epoch   : constant Epoch_Seconds := 1_700_000_000;  --  Unix timestamp
   Result  : constant Transition_Result := Get_Transition_At_Epoch (Zone_Id, Epoch);
begin
   if Is_Ok (Result) then
      declare
         Info : constant Transition_Info := Value (Result);
      begin
         --  Info.UTC_Offset contains seconds offset from UTC
         --  Info.Is_DST indicates if daylight saving time is active
         null;
      end;
   end if;
end Get_Offset;
```

---

## Build Configuration

### Profile Selection

| Profile | Use Case | Command |
|---------|----------|---------|
| `embedded` | Ravenscar systems (512KB+ RAM) | `-XTZIF_PROFILE=embedded` |
| `baremetal` | Zero footprint (128KB+ RAM) | `-XTZIF_PROFILE=baremetal` |
| `stm32h7s78` | STM32H7S78-DK board | `-XTZIF_PROFILE=stm32h7s78` |
| `stm32mp135_linux` | STM32MP135 with Linux | `-XTZIF_PROFILE=stm32mp135_linux` |

### Cross-Compilation Example

```bash
# For ARM Cortex-M with embedded runtime
gprbuild -P your_project.gpr \
    --target=arm-eabi \
    --RTS=embedded-stm32f769disco \
    -XTZIF_PROFILE=embedded

# For STM32MP135F-DK Linux
gprbuild -P your_project.gpr \
    --target=arm-linux-gnueabihf \
    -XTZIF_PROFILE=stm32mp135_linux
```

---

## Memory Considerations

### Stack Usage

The Embedded adapter is designed for minimal stack usage:

| Operation | Stack (approx) |
|-----------|----------------|
| `Find_My_Id` | 256 bytes |
| `Find_By_Id` | 1 KB |
| `Get_Transition_At_Epoch` | 2 KB |
| `List_All_Order_By_Id` | 4 KB |

### Heap Allocation

The embedded profiles avoid dynamic allocation where possible:

- **Bounded strings**: Fixed-size buffers
- **Zone arrays**: Stack-allocated bounded arrays
- **Result types**: Value semantics (no pointers)

### Reducing Footprint

To minimize memory usage:

1. **Use minimal zone set**: Include only required timezones
2. **Reduce Max_Zones**: Configure in profile
3. **Disable debug**: Set `Enable_Debug => False`
4. **Use baremetal profile**: For <256 KB RAM systems

---

## Ravenscar Profile Compatibility

The Embedded adapter is designed for Ravenscar profile compatibility:

### Restrictions Supported

```ada
pragma Restrictions (No_Abort_Statements);
pragma Restrictions (No_Dynamic_Attachment);
pragma Restrictions (No_Dynamic_Priorities);
pragma Restrictions (No_Local_Protected_Objects);
pragma Restrictions (No_Protected_Type_Allocators);
pragma Restrictions (No_Task_Allocators);
pragma Restrictions (No_Task_Hierarchy);
pragma Restrictions (Simple_Barriers);
pragma Restrictions (Max_Task_Entries => 0);
pragma Restrictions (No_Select_Statements);
```

### Thread Safety

For concurrent access in Ravenscar applications, use protected objects:

```ada
protected Timezone_Cache is
   procedure Get_Local_Zone (Result : out My_Zone_Result);
   procedure Lookup_Zone (Id : Zone_Id_Type; Result : out Zone_Result);
private
   Initialized : Boolean := False;
end Timezone_Cache;
```

---

## Troubleshooting

### Q: Find_By_Id returns error for all zones

**A:** Check that `TZIF_DATA_PATH` is set correctly:

```ada
if not Ada.Environment_Variables.Exists ("TZIF_DATA_PATH") then
   Ada.Text_IO.Put_Line ("ERROR: TZIF_DATA_PATH not set");
end if;
```

### Q: Find_My_Id always returns UTC

**A:** This is expected behavior when `TZIF_SYSTEM_ZONE` is not set. The Embedded adapter defaults to UTC for safety. Set the environment variable if you need a different default.

### Q: Application crashes when reading timezone files

**A:** Verify:
1. RAM filesystem is properly initialized
2. Timezone files are valid TZif format
3. File paths don't exceed `Max_Path_Length`

### Q: Compilation fails with "restriction violation"

**A:** Ensure you're using the correct profile for your runtime:
- Ravenscar runtime → `embedded` profile
- ZFP runtime → `baremetal` profile
- Full Ada runtime → `standard` profile

### Q: Build fails with "package not found"

**A:** The Embedded API is in a separate package. Ensure you're importing:

```ada
with TZif.API.Embedded;  --  Correct
--  Not: with TZif.API;  --  This is Desktop
```

---

## See Also

- [Quick Start Guide](../quick_start.md) - General TZif usage
- [Build Profiles Guide](build_profiles.md) - All profile configurations
- [Error Handling Strategy](error_handling_strategy.md) - Result monad patterns
- [Architecture Enforcement](architecture_enforcement.md) - Layer dependencies

---

**License:** BSD-3-Clause
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.
