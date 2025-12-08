# TZif Quick Start Guide

**Version:** 2.0.0  
**Date:** December 07, 2025  
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** 2025 Michael Gardner, A Bit of Help, Inc.<br>  
**Status:** Released  

---

## Table of Contents

- [Installation](#installation)
- [First Program](#first-program)
- [Finding a Timezone](#finding-a-timezone)
- [Finding Local Timezone](#finding-local-timezone)
- [Querying Transitions](#querying-transitions)
- [Error Handling](#error-handling)
- [Running Tests](#running-tests)
- [Example Programs](#example-programs)
- [Common Issues](#common-issues)

---

## Installation

### Using Alire (Recommended)

```bash
# Add TZif to your project
alr with tzif

# Or get TZif standalone
alr get tzif
cd tzif_2.0.0_*
alr build
```

### Manual Installation

```bash
git clone --recurse-submodules https://github.com/abitofhelp/tzif.git
cd tzif
alr build
```

---

## First Program

Create a simple program to detect your local timezone:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with TZif.API;

procedure My_First_TZif is
   use TZif.API;
   Result : constant My_Zone_Result := Find_My_Id;
begin
   if Is_Ok (Result) then
      Put_Line ("Local timezone: " & To_String (Value (Result)));
   else
      Put_Line ("Could not detect local timezone");
   end if;
end My_First_TZif;
```

**Build and Run:**
```bash
alr build
./bin/my_first_tzif
```

**Expected Output:**
```
Local timezone: America/Denver
```

---

## Finding a Timezone

Look up a timezone by its IANA identifier:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with TZif.API;

procedure Find_Zone_Example is
   use TZif.API;

   Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("America/New_York");
   Result  : constant Zone_Result := Find_By_Id (Zone_Id);
begin
   if Is_Ok (Result) then
      Put_Line ("Found timezone: " & To_String (Zone_Id));
   else
      Put_Line ("Timezone not found");
   end if;
end Find_Zone_Example;
```

**Common Zone IDs:**
- `UTC` - Coordinated Universal Time
- `America/New_York` - Eastern Time (US)
- `America/Los_Angeles` - Pacific Time (US)
- `America/Phoenix` - Arizona (no DST)
- `Europe/London` - British Time
- `Asia/Tokyo` - Japan Standard Time

---

## Finding Local Timezone

Detect the system's local timezone:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with TZif.API;

procedure Find_Local_Example is
   use TZif.API;
   Result : constant My_Zone_Result := Find_My_Id;
begin
   if Is_Ok (Result) then
      Put_Line ("Local timezone: " & To_String (Value (Result)));
   else
      Put_Line ("Could not detect local timezone");
   end if;
end Find_Local_Example;
```

**Platform Implementation:**
- **Linux/BSD**: Reads `/etc/localtime` symlink
- **macOS**: Reads system timezone configuration
- **Windows**: Uses Win32 API with CLDR mapping to IANA zone ID

---

## Querying Transitions

Query timezone offset at a specific epoch time:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with TZif.API;

procedure Query_Transition is
   use TZif.API;

   Zone_Id      : constant Zone_Id_String := Make_Zone_Id_String ("America/Los_Angeles");
   Summer_Epoch : constant Epoch_Seconds_Type := 1_719_792_000;  -- July 1, 2024
   Result       : constant Transition_Result := Get_Transition_At_Epoch (Zone_Id, Summer_Epoch);
begin
   if Is_Ok (Result) then
      Put_Line ("Transition info retrieved successfully");
   else
      Put_Line ("Could not get transition info");
   end if;
end Query_Transition;
```

---

## Error Handling

TZif uses the Result monad pattern - no exceptions are raised.

### Pattern 1: Check Success/Failure

```ada
Result : constant Zone_Result := Find_By_Id (Zone_Id);

if Is_Ok (Result) then
   --  Success path
   Put_Line ("Found zone");
else
   --  Error path
   Put_Line ("Zone not found");
end if;
```

### Pattern 2: Extract Value on Success

```ada
Result : constant My_Zone_Result := Find_My_Id;

if Is_Ok (Result) then
   declare
      Zone_Id : constant Zone_Id_Type := Value (Result);
   begin
      Put_Line ("Zone: " & To_String (Zone_Id));
   end;
end if;
```

### Pattern 3: Early Return

```ada
function Process_Zone (Id : String) return Boolean is
   Zone_Id : constant Zone_Id_Type := Make_Zone_Id (Id);
   Result  : constant Zone_Result := Find_By_Id (Zone_Id);
begin
   if Is_Error (Result) then
      return False;  --  Early exit on error
   end if;

   --  Continue with success path
   return True;
end Process_Zone;
```

**Why No Exceptions?**
- Explicit error paths enforced by compiler
- SPARK compatible for formal verification
- Deterministic timing (no stack unwinding)
- Errors are values that can be passed and transformed

---

## Running Tests

### All Tests

```bash
make test-all
```

### Specific Test Suites

```bash
# Unit tests only
make test-unit

# Integration tests only
make test-integration

# Examples only
make test-examples
```

---

## Example Programs

TZif includes 11 example programs in `examples/`:

```bash
# Build all examples
make build-examples

# Run individual examples
./bin/examples/find_by_id
./bin/examples/find_my_id
./bin/examples/get_transition_at_epoch
```

**Available Examples:**

| Example | Description |
|---------|-------------|
| `find_by_id` | Find timezone by exact ID |
| `find_my_id` | Detect local timezone |
| `find_by_pattern` | Search zones by substring |
| `find_by_region` | Search zones by region |
| `find_by_regex` | Search zones by regex |
| `get_transition_at_epoch` | Query timezone at time |
| `list_all_zones` | Enumerate all zones |
| `discover_sources` | Find timezone sources |
| `load_source` | Load timezone source |
| `validate_source` | Validate source integrity |
| `get_version` | Query library version |

---

## Common Issues

### Q: Where are timezone files located?

**A:** Standard locations by platform:
- **Linux/BSD**: `/usr/share/zoneinfo`
- **macOS**: `/var/db/timezone/zoneinfo`
- **Windows**: User must provide IANA tzdata directory

### Q: Why does `Find_My_Id` return an error?

**A:** Common causes:
- **Linux/BSD/macOS**: `/etc/localtime` symlink not configured
- **Windows**: Windows timezone may not have IANA mapping
- This is expected behavior - handle with `Is_Error(Result)`

### Q: What TZif versions are supported?

**A:** TZif supports versions 1, 2, and 3 as defined in RFC 9636.

### Q: Can I use TZif without the functional library?

**A:** The `functional` library is only used in the infrastructure layer. The domain layer has zero external dependencies. For minimal builds, you can use domain types directly.

---

## Next Steps

- **[Documentation Index](index.md)** - Complete documentation overview
- **[Error Handling Strategy](common/guides/error_handling_strategy.md)** - Deep dive into Result monad
- **[Architecture Enforcement](common/guides/architecture_enforcement.md)** - Layer dependency rules

---

**License:** BSD-3-Clause  
**Copyright:** 2025 Michael Gardner, A Bit of Help, Inc.  
