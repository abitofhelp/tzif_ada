# TZif Quick Start Guide

**Version:** 1.0.0  
**Date:** December 02, 2025  
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>  
**Status:** Released  

---

## Table of Contents

- [Installation](#installation)
- [First Program](#first-program)
- [Finding a Timezone](#finding-a-timezone)
- [Finding Local Timezone](#finding-local-timezone)
- [Discovering Timezone Sources](#discovering-timezone-sources)
- [Error Handling](#error-handling)
- [Build Profiles](#build-profiles)
- [Running Tests](#running-tests)
- [Next Steps](#next-steps)
- [Common Issues](#common-issues)

---

## Installation

### Using Alire (Recommended)

```bash
# Add TZif to your project
alr with tzif

# Or get TZif standalone
alr get tzif
cd tzif_1.0.0_*
alr build
```

### Manual Installation

```bash
git clone https://github.com/abitofhelp/tzif.git
cd tzif
alr build
```

---

## First Program

Create a simple program to validate a timezone identifier:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with TZif.API;

procedure My_First_TZif is
   use TZif.API;

   Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("America/New_York");
begin
   Put_Line ("Successfully created Zone_Id: " & To_String (Zone_Id));
end My_First_TZif;
```

**Build and Run:**
```bash
alr build
./bin/my_first_tzif
```

**Output:**
```
Successfully created Zone_Id: America/New_York
```

---

## Finding a Timezone

Look up a timezone by its IANA identifier:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with TZif.API;

procedure Find_Zone_Example is
   use TZif.API;

   Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("Europe/London");
   Result  : constant Zone_Result := Find_By_Id (Zone_Id);
begin
   if Is_Ok (Result) then
      Put_Line ("Found timezone: " & To_String (Zone_Id));
   elsif Is_Error (Result) then
      Put_Line ("Timezone not found or error occurred");
   end if;
end Find_Zone_Example;
```

**Key Points:**
- `Find_By_Id` returns a `Result[Zone, Error]` - **NO EXCEPTIONS**
- Always check `Is_Ok(Result)` or `Is_Error(Result)` before proceeding
- Railway-oriented programming: explicit error paths as values

**Try These Zone IDs:**
- `UTC` - Coordinated Universal Time
- `America/New_York` - Eastern Time (US)
- `America/Los_Angeles` - Pacific Time (US)
- `Europe/London` - British Time
- `Asia/Tokyo` - Japan Standard Time

---

## Finding Local Timezone

Automatically detect the system's local timezone:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with TZif.API;

procedure Find_Local_Example is
   use TZif.API;

   Result : constant My_Zone_Result := Find_My_Id;
begin
   if Is_Ok (Result) then
      declare
         Zone_Id : constant Zone_Id_Type := Value (Result);
      begin
         Put_Line ("Local timezone: " & To_String (Zone_Id));
      end;
   elsif Is_Error (Result) then
      Put_Line ("Could not detect local timezone");
      Put_Line ("(This is normal on some systems without /etc/localtime)");
   end if;
end Find_Local_Example;
```

**Platform Support:**
- **Linux/BSD**: Reads `/etc/localtime` symlink
- **macOS**: Reads system timezone configuration
- **Windows 10/Server 2022+**: Queries Windows timezone API, maps to IANA zone ID
  - User must provide path to IANA tzdata directory
  - Download from: https://www.iana.org/time-zones

---

## Discovering Timezone Sources

Scan the filesystem for available timezone data sources:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with TZif.API;

procedure Discover_Example is
   use TZif.API;

   --  Standard POSIX timezone locations
   Paths : Path_List (1 .. 2);
begin
   Paths (1) := To_Bounded_String ("/usr/share/zoneinfo");
   Paths (2) := To_Bounded_String ("/var/db/timezone/zoneinfo");

   declare
      Result : constant Discovery_Result := Discover_Sources (Paths);
   begin
      if Is_Ok (Result) then
         Put_Line ("Timezone sources discovered successfully");
      else
         Put_Line ("No timezone sources found");
      end if;
   end;
end Discover_Example;
```

**Common Timezone Locations:**
- Linux/BSD: `/usr/share/zoneinfo`
- macOS: `/usr/share/zoneinfo` or `/var/db/timezone/zoneinfo`

---

## Error Handling

TZif uses the **Result Monad Pattern** for **NO EXCEPTIONS** error handling:

### Pattern 1: Check Success/Failure

```ada
Result : constant Zone_Result := Find_By_Id (Zone_Id);

if Is_Ok (Result) then
   --  Success! Zone was found
   Put_Line ("Success");
elsif Is_Error (Result) then
   --  Error occurred
   Put_Line ("Error");
end if;
```

### Pattern 2: Extract Value on Success

```ada
Result : constant My_Zone_Result := Find_My_Id;

if Is_Ok (Result) then
   declare
      Zone_Id : constant Zone_Id_Type := Value (Result);
   begin
      --  Use the zone ID...
      Put_Line ("Zone: " & To_String (Zone_Id));
   end;
end if;
```

### Pattern 3: Early Return

```ada
function Process_Zone (Zone_Id : Zone_Id_Type) return Boolean is
   Result : constant Zone_Result := Find_By_Id (Zone_Id);
begin
   if Is_Error (Result) then
      return False;  --  Early exit on error
   end if;

   --  Continue with success path...
   return True;
end Process_Zone;
```

**Why This Approach?**
- **Explicit Error Paths**: Compiler enforces error checking
- **SPARK Compatible**: No exceptions = formal verification possible
- **Deterministic**: No stack unwinding, predictable timing
- **Composable**: Errors are values that can be passed and transformed

---

## Build Profiles

TZif supports multiple build profiles for different environments:

### Standard (Desktop/Server)

```bash
# Default profile - already active
alr build
```

**Characteristics:**
- Full Ada runtime
- 1+ GB RAM recommended
- All features enabled
- Optimized for developer productivity

### Embedded (Ravenscar)

```bash
alr build -- -XTZIF_PROFILE=embedded
```

**Characteristics:**
- Ravenscar profile (restricted Ada for safety-critical systems)
- 512KB+ RAM
- Limited zones/transitions
- SPARK-verifiable domain logic

### Bare Metal

```bash
alr build -- -XTZIF_PROFILE=baremetal
```

**Characteristics:**
- Zero Footprint Profile
- 128KB+ RAM
- Minimal zone support
- No dynamic allocation
- Suitable for microcontrollers

### See All Profiles

```bash
ls config/profiles/
```

Available: `standard`, `embedded`, `concurrent`, `baremetal`, `stm32h7s78`, `stm32mp135_linux`

---

## Running Tests

### All Tests

```bash
make test-all
```

### Specific Test Suites

```bash
# Unit tests only
./test/bin/unit_runner

# Integration tests only
./test/bin/integration_runner
```

### Expected Output

```
########################################
###                                  ###
###    UNIT TESTS: SUCCESS           ###
###    All  126 tests passed!        ###
###                                  ###
########################################
```

**All 253 tests passing** (126 unit + 116 integration + 11 examples)

---

## Next Steps

### Learn More

- **[Error Handling Strategy](guides/error_handling_strategy.md)** - Deep dive into Result monad
- **[Build Profiles Guide](guides/build_profiles.md)** - Configure for your platform
- **[Embedded Platform Guide](guides/embedded_platform_guide.md)** - STM32, Ravenscar, and embedded targets
- **[Architecture Enforcement](guides/architecture_enforcement.md)** - Layer dependency rules

### Example Programs

TZif includes 11 complete examples in `examples/`:

```bash
# Build all examples
make build-examples

# Run examples
./bin/examples/find_by_id
./bin/examples/find_my_id
./bin/examples/discover_sources
./bin/examples/get_transition_at_epoch
```

**Available Examples:**
- `find_by_id` - Find timezone by exact ID
- `find_my_id` - Detect local timezone
- `find_by_pattern` - Search zones by substring
- `find_by_region` - Search zones by region
- `find_by_regex` - Search zones by regular expression
- `get_transition_at_epoch` - Query timezone at specific time
- `list_all_zones` - Enumerate all zones
- `discover_sources` - Scan for timezone sources
- `load_source` - Load timezone data source
- `validate_source` - Validate source integrity
- `get_version` - Query database version

### API Reference

See the full API documentation in:
- **[TZif.API](../src/api/tzif-api.ads)** - Main public API
- **[Documentation Index](index.md)** - Complete documentation tree

---

## Common Issues

### Q: Where are timezone files located?

**A:** TZif looks for timezone files in standard locations:
- Linux/BSD: `/usr/share/zoneinfo`
- macOS: `/usr/share/zoneinfo` or `/var/db/timezone/zoneinfo`

### Q: How do I add a custom timezone directory?

**A:** Use the `Discover_Sources` API to scan custom directories - see example above.

### Q: Why does `Find_My_Id` return an error?

**A:**
- **Linux/BSD/macOS**: Some systems don't have `/etc/localtime` configured
- **Windows**: The Windows timezone may not have an IANA mapping (uncommon zones)
- This is normal behavior - handle with `Is_Error(Result)`

### Q: Can I use TZif without the Functional library dependency?

**A:** The `functional` library is only used in the infrastructure layer for the Result monad implementation. The domain layer has zero dependencies. If you need to eliminate the dependency, you can implement your own Result type.

### Q: Does TZif support all IANA timezone versions?

**A:** Yes! TZif supports TZif versions 1, 2, and 3. Tested with IANA tzdb 2025b.

---

**License:** BSD-3-Clause  
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.  
