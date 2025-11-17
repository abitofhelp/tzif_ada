# TZif Build Profiles

**Version:** 1.0.0-rc1  
**Date:** November 16, 2025  
**SPDX-License-Identifier:** BSD-3-Clause  
**License File:** See the LICENSE file in the project root.  
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.  
**Status:** Unreleased  


## Overview

TZif library supports multiple build profiles optimized for different target environments.

## Profile Selection

```bash
# Build with specific profile
alr build -- -Xprofile=embedded

# Build with specific mode
alr build -- -Xmode=release

# Combine profile and mode
alr build -- -Xprofile=embedded -Xmode=release
```

## Available Profiles

### General Profiles

#### 1. Standard Profile (Default)

**Target**: Desktop/server applications (1+ GB RAM)
**Runtime**: Full Ada runtime
**Restrictions**: None
**Use Case**: Development, testing, production servers

```bash
alr build -- -Xprofile=standard -Xmode=debug
```

**Features**:
- All warnings enabled
- Full debugging support
- No memory constraints
- Maximum flexibility

#### 2. Embedded Profile

**Target**: Ravenscar-compatible embedded systems (STM32F769+ / 532KB+ RAM)
**Runtime**: Full or Ravenscar runtime
**Restrictions**: Provided in `config/profiles/embedded/restrictions.adc`
**Use Case**: GPS devices, IoT, industrial control

```bash
alr build -- -Xprofile=embedded -Xmode=release
```

**Features**:
- Suppresses exception propagation warnings (compatible with No_Exception_Propagation)
- Memory-efficient compilation
- Deterministic behavior
- Compatible with Ravenscar profile

**To apply restrictions in your application**:

```ada
-- In your application's GPR file:
package Compiler is
   for Local_Configuration_Pragmas use
     "tzif/config/profiles/embedded/restrictions.adc";
end Compiler;
```

Or copy the file:
```bash
cp tzif/config/profiles/embedded/restrictions.adc ./gnat.adc
```

#### 3. Concurrent Profile

**Target**: Multi-threaded applications (desktop/server)
**Runtime**: Full Ada runtime with tasking
**Restrictions**: Provided in `config/profiles/concurrent/restrictions.adc`
**Use Case**: High-performance servers, parallel processing

```bash
alr build -- -Xprofile=concurrent -Xmode=optimize
```

**Features**:
- Elaboration warnings (important for tasking)
- Size/alignment warnings (concurrent access considerations)
- Optimized for multi-core processors
- Full tasking support

#### 4. Bare Metal Profile

**Target**: Zero Footprint Profile (ZFP) systems
**Runtime**: ZFP runtime (minimal, no OS)
**Restrictions**: Provided in `config/profiles/baremetal/restrictions.adc`
**Use Case**: Microcontrollers, bare metal firmware

```bash
alr build -- -Xprofile=baremetal -Xmode=release --RTS=zfp
```

**Features**:
- Minimal runtime footprint
- No operating system required
- Static memory allocation only
- Strict restrictions for safety

**Note**: Requires ZFP runtime selection via `--RTS=zfp` or in GPR:
```ada
package Runtime is
   for Runtime ("Ada") use "zfp";
end Runtime;
```

### Board-Specific Profiles

#### 5. STM32H7S78-DK Profile

**Target**: STM32H7S78-DK Discovery Kit
**Hardware**: Cortex-M7 @ 600 MHz, 620KB SRAM + 32MB PSRAM
**Runtime**: Full or Ravenscar runtime
**Restrictions**: Provided in `config/profiles/stm32h7s78/restrictions.adc`
**Use Case**: High-performance embedded with external memory

```bash
alr build -- -XTZIF_PROFILE=stm32h7s78 -Xmode=release
```

**Features**:
- Generous limits leveraging external PSRAM
- Can cache 300+ parsed timezone files
- Max transitions: 500 per zone (>100 years)
- Cache capacity: 300 zones
- Ideal for industrial control, advanced GPS systems

**Hardware Details**:
- 620 KB internal SRAM for critical data structures
- 32 MB external PSRAM for timezone cache
- 128 MB external Flash for TZif file storage

#### 6. STM32MP135F-DK Profile (Linux)

**Target**: STM32MP135F-DK running OpenSTLinux
**Hardware**: Cortex-A7 @ 1 GHz, 512 MB DDR3L RAM
**Runtime**: Full Ada runtime on Linux
**Restrictions**: Optional (primarily for memory safety)
**Use Case**: Embedded Linux server, IoT gateway

```bash
alr build -- -XTZIF_PROFILE=stm32mp135_linux -Xmode=optimize
```

**Features**:
- Server-class configuration (no memory constraints)
- Can cache entire IANA database (600+ zones)
- Max transitions: 2000 per zone (>400 years)
- Cache capacity: 600 zones
- Maximum compatibility and performance

**Hardware Details**:
- 512 MB DDR3L RAM
- Linux operating system (OpenSTLinux)
- Full filesystem access for TZif files
- Network connectivity for timezone updates

**Use Cases**:
- IoT gateway providing timezone services
- Embedded web server with timezone API
- Development and testing platform
- Edge computing with timezone awareness

## Build Modes

### Debug Mode (Default)

```bash
alr build -- -Xmode=debug
```

**Optimizations**: None (`-O0`)
**Checks**: All enabled
**Features**:
- `-g` - Debug info
- `-gnatVa` - All validity checks
- `-gnateE` - Extra exception info
- `-gnata` - Assertions enabled
- `-gnatU` - Tag uninitialized scalars

**Use**: Development, debugging

### Release Mode

```bash
alr build -- -Xmode=release
```

**Optimizations**: Level 2 (`-O2`)
**Checks**: Suppressed (`-gnatp`)
**Features**:
- `-gnatn` - Enable inlining
- No debug info

**Use**: Production deployments

### Optimize Mode

```bash
alr build -- -Xmode=optimize
```

**Optimizations**: Maximum (`-O3`)
**Checks**: Suppressed (`-gnatp`)
**Features**:
- `-gnatn` - Enable inlining
- `-funroll-loops` - Loop unrolling
- `-ffunction-sections` - Separate sections (enables GC)
- `-fdata-sections` - Separate data sections

**Use**: Performance-critical applications

## Embedded Restrictions

TZif library is **compatible** with embedded restrictions but does not enforce them. Applications choose which restrictions to apply.

### Standard Embedded Restrictions

Located in: `config/profiles/embedded/restrictions.adc`

```ada
pragma Restrictions (No_Implicit_Heap_Allocations);
pragma Restrictions (No_Recursion);
pragma Restrictions (No_Access_Parameter_Allocators);
pragma Restrictions (No_Coextensions);
pragma Restrictions (No_Anonymous_Allocators);
pragma Restrictions (No_Unchecked_Deallocation);
```

### Bare Metal Restrictions

Located in: `config/profiles/baremetal/restrictions.adc`

Includes all embedded restrictions plus:
```ada
pragma Restrictions (No_Fixed_Point);
pragma Restrictions (No_Implementation_Attributes);
```

## Verification

To verify library compliance with restrictions:

```bash
# Test embedded profile with restrictions
alr exec -- gprbuild -P tzif.gpr \
  -gnatec=config/profiles/embedded/restrictions.adc \
  -Xprofile=embedded

# Test bare metal profile
alr exec -- gprbuild -P tzif.gpr \
  -gnatec=config/profiles/baremetal/restrictions.adc \
  -Xprofile=baremetal
```

## Examples

### GPS Device (Embedded)

```bash
# Build library
cd tzif/
alr build -- -Xprofile=embedded -Xmode=release

# In your GPS application GPR:
with "tzif.gpr";

project GPS_Device is
   for Main use ("gps_main.adb");

   package Compiler is
      for Local_Configuration_Pragmas use
        "../tzif/config/profiles/embedded/restrictions.adc";
   end Compiler;
end GPS_Device;
```

### Multi-threaded Server (Concurrent)

```bash
# Build library
cd tzif/
alr build -- -Xprofile=concurrent -Xmode=optimize

# No restrictions needed - full runtime
```

### Microcontroller (Bare Metal)

```bash
# Build library with ZFP runtime
cd tzif/
alr build -- -Xprofile=baremetal -Xmode=release --RTS=zfp

# In your firmware GPR:
project Firmware is
   for Runtime ("Ada") use "zfp";

   package Compiler is
      for Local_Configuration_Pragmas use
        "../tzif/config/profiles/baremetal/restrictions.adc";
   end Compiler;
end Firmware;
```

## Design Philosophy

Following Ada ecosystem best practices:

1. **Libraries are restriction-compatible, not restriction-enforcing**
   - Source code has no `pragma Restrictions`
   - Allows library to be tested with AUnit (which uses recursion)

2. **Applications decide restriction policy**
   - Restrictions are partition-wide in Ada
   - Applications choose appropriate restrictions via `.adc` files

3. **Release process verifies compliance**
   - Library is built with all profile restrictions during release validation
   - Ensures library remains compatible with embedded use cases

## References

- `shared_config.gpr` - Profile and mode configuration
- `config/profiles/*/restrictions.adc` - Restriction files per profile
- See: EMBEDDED_RESTRICTIONS_SOLUTION.md in zoneinfo project for detailed rationale
