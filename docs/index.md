# TZif Documentation

**Version:** 3.0.2<br>
**Date:** 2025-12-16<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

---

## Overview

TZif is an Ada 2022 library for parsing and querying IANA's compiled timezone information (TZif binary format, RFC 9636). It provides a clean, functional API with Result monad error handling, hexagonal architecture, and SPARK-verified domain logic.

**Key Capabilities:**
- Parse TZif binary files (versions 1, 2, and 3)
- Query timezone transitions at any Unix epoch time
- Discover and validate timezone data sources
- Find zones by ID, pattern, region, or regex
- Detect the system's local timezone
- Cross-platform: Linux, macOS, BSD, Windows, Embedded

**Project Status:**
- 569 tests passing (424 unit + 134 integration + 11 examples)
- SPARK verified: 87% proof rate (1179 proved, 171 unproved)
- Zero compiler warnings

---

## Quick Navigation

### Getting Started

- **[Quick Start Guide](quick_start.md)** - Installation and first program
- **[Build Profiles](common/guides/build_profiles.md)** - Configure for different targets

### Formal Documentation

- **[Software Requirements Specification](formal/software_requirements_specification.md)** - Functional and non-functional requirements
- **[Software Design Specification](formal/software_design_specification.md)** - Architecture and detailed design
- **[Software Test Guide](formal/software_test_guide.md)** - Testing strategy and execution

### Developer Guides

- **[All About Our API](common/guides/all_about_our_api.md)** - Three-package API pattern
- **[Architecture Enforcement](common/guides/architecture_enforcement.md)** - Layer dependency rules
- **[Error Handling Strategy](common/guides/error_handling_strategy.md)** - Result monad patterns

### Reference

- **[CHANGELOG](../CHANGELOG.md)** - Release history
- **[README](../README.md)** - Project overview
- **[Roadmap](roadmap.md)** - Future development plans

---

## Architecture

TZif implements a 4-layer hexagonal architecture:

```
+-----------------------------------------------------------------+
|                          API Layer                               |
|  TZif.API (facade) + TZif.API.Desktop/Windows/Embedded (roots)  |
+----------------------------------+------------------------------+
                                   |
+----------------------------------v------------------------------+
|                      Application Layer                           |
|  Use Cases (11 operations) + Inbound/Outbound Ports             |
+----------------------------------+------------------------------+
                                   |
+----------------------------------v------------------------------+
|                    Infrastructure Layer                          |
|  I/O Adapters (Desktop, Windows, Embedded) + Platform Ops       |
+----------------------------------+------------------------------+
                                   |
+----------------------------------v------------------------------+
|                       Domain Layer                               |
|  Entities (Zone) + Value Objects + Parser + Result Monad        |
+-----------------------------------------------------------------+
```

**Design Principles:**
- Dependencies flow inward (toward Domain)
- Domain layer has zero external dependencies
- Infrastructure implements ports defined in Application
- API provides stable public interface
- Generic I/O plugin pattern enables platform portability
- Functional.Try.Map_To_Result at all infrastructure boundaries

---

## API Operations

TZif provides 11 operations through `TZif.API`:

| Operation | Description |
|-----------|-------------|
| `Find_By_Id` | Lookup timezone by exact IANA identifier |
| `Find_By_Pattern` | Search zones by substring match |
| `Find_By_Region` | Search zones by geographic region |
| `Find_By_Regex` | Search zones using regular expressions |
| `Find_My_Id` | Detect the system's local timezone |
| `Get_Transition_At_Epoch` | Query UTC offset and DST at any time |
| `Get_Version` | Retrieve IANA database version |
| `List_All_Zones` | Enumerate all available timezones |
| `Discover_Sources` | Find timezone data directories |
| `Load_Source` | Load a specific timezone data source |
| `Validate_Source` | Validate timezone data integrity |

---

## Platform Support

| Platform | Status | Timezone Source |
|----------|--------|-----------------|
| **Linux** | Full | `/usr/share/zoneinfo` |
| **macOS** | Full | `/var/db/timezone/zoneinfo` |
| **BSD** | Full | `/usr/share/zoneinfo` |
| **Windows** | Full | User-provided IANA tzdata + Win32 API |
| **Embedded** | Stub | Custom adapter required |

---

## Quick Example

```ada
with TZif.API;
with Ada.Text_IO; use Ada.Text_IO;

procedure Show_Local_Timezone is
   use TZif.API;
   Result : constant My_Zone_Result := Find_My_Id;
begin
   if Is_Ok (Result) then
      Put_Line ("Local timezone: " & To_String (Value (Result)));
   else
      Put_Line ("Could not detect local timezone");
   end if;
end Show_Local_Timezone;
```

---

## Dependencies

| Package | Version | Purpose |
|---------|---------|---------|
| `functional` | ^4.0.0 | Result/Option monads, Functional.Try |
| `gnatcoll` | ^25.0.0 | GNAT Components Collection |

**Note:** Domain layer has zero external dependencies (pure Ada 2022).

---

## Test Results

| Category | Count | Status |
|----------|-------|--------|
| Unit Tests | 424 | Passing |
| Integration Tests | 134 | Passing |
| Example Programs | 11 | Passing |
| **Total** | **569** | **All Passing** |

---

## Need Help?

- Check the [Quick Start Guide](quick_start.md) for common issues
- Review the [Software Test Guide](formal/software_test_guide.md) for testing help
- See [Error Handling Strategy](common/guides/error_handling_strategy.md) for Result monad usage

---

**License:** BSD-3-Clause<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.
