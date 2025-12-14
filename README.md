# IANA Timezone Information File Library

[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE) [![Ada](https://img.shields.io/badge/Ada-2022-blue.svg)](https://ada-lang.io) [![Alire](https://img.shields.io/badge/Alire-2.0+-blue.svg)](https://alire.ada.dev) [![SPARK](https://img.shields.io/badge/SPARK-Proved-green.svg)](https://www.adacore.com/about-spark)

**Version:** 99.99.99<br>
**Date:** 2025-12-13<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

## Important
In a few days, I will release a library that provides higher level 
functions for working with timezones and durations in Ada 2022.

## Overview

TZif is an Ada 2022 library for parsing and querying IANA's compiled timezone information (TZif format, RFC 9636) files. It provides a clean, functional API with Result monad error handling, hexagonal architecture, and embedded-safe patterns.

Designed for safety-critical, embedded, and high-assurance applications with full SPARK compatibility.

## SPARK Formal Verification

<table>
<tr>
<td width="120"><strong>Status</strong></td>
<td><img src="https://img.shields.io/badge/SPARK-Proved-green.svg" alt="SPARK Proved"></td>
</tr>
<tr>
<td><strong>Scope</strong></td>
<td>Domain + Application layers (value objects, containers, parser, operations, ports)</td>
</tr>
<tr>
<td><strong>Mode</strong></td>
<td>gnatprove --mode=prove --level=2</td>
</tr>
<tr>
<td><strong>Results</strong></td>
<td>1350 checks: 1179 proved, 171 unproved (in generic instantiations)</td>
</tr>
</table>

The domain and application port layers are formally verified using SPARK Ada, providing mathematical guarantees of:

- **No runtime errors** - Division by zero, overflow, range violations
- **No uninitialized data** - All variables properly initialized before use
- **Contract compliance** - Pre/postconditions proven correct
- **Data flow integrity** - No aliasing or information flow violations

### Verification Commands

```bash
make spark-check    # Run SPARK legality verification
make spark-prove    # Run full SPARK proof verification
```

### SPARK Layer Coverage

| Layer | SPARK_Mode | Description |
|-------|-----------|-------------|
| Domain | On | Value objects, bounded containers, parser, error types |
| Application | On | Operations, inbound ports, outbound ports |
| Infrastructure | Off | I/O operations (file system, platform) |
| API | Off | Facade over infrastructure |

## Features

- Parse IANA TZif binary files (versions 1, 2, and 3)
- Query timezone transitions at any Unix epoch time
- Discover and validate timezone data sources
- Find zones by ID, pattern, region, or regex
- Detect the system's local timezone
- Cross-platform: Linux, macOS, BSD, Windows 11, Embedded
- 4-layer hexagonal architecture (Domain, Application, Infrastructure, API)
- Result monad error handling (via `functional` crate)
- Generic I/O plugin pattern for platform portability

## Getting Started

### Clone with Submodules

```bash
git clone --recurse-submodules https://github.com/abitofhelp/tzif.git
```

Or if already cloned:

```bash
git submodule update --init --recursive
```

### Building

```bash
# Build the library
make build

# Build release version
make build-release
```

### Using in Your Project

Add to your `alire.toml`:

```toml
[[depends-on]]
tzif = "^2.0.0"
```

## Quick Example

```ada
with TZif.API;

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

## Architecture

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

## Platform Support

| Platform | Status | Timezone Source |
|----------|--------|-----------------|
| **Linux** | Full | `/usr/share/zoneinfo` |
| **macOS** | Full | `/var/db/timezone/zoneinfo` |
| **BSD** | Full | `/usr/share/zoneinfo` |
| **Windows** | Full | User-provided IANA tzdata |
| **Embedded** | Stub | Custom adapter required |

## Testing

```bash
# Run all tests
make test-all

# Run unit tests only
make test-unit

# Run integration tests only
make test-integration
```

**Test Results:** 424 unit + 134 integration + 11 examples = **569 tests passing**

## Documentation

- [Documentation Index](docs/index.md) - Complete documentation overview
- [Quick Start Guide](docs/quick_start.md) - Get started in minutes
- [Software Requirements Specification](docs/formal/software_requirements_specification.md)
- [Software Design Specification](docs/formal/software_design_specification.md)
- [Software Test Guide](docs/formal/software_test_guide.md)
- [Roadmap](docs/roadmap.md) - Future plans
- [CHANGELOG](CHANGELOG.md) - Release history

## Examples

The `examples/` directory contains working programs:

| Example | Description |
|---------|-------------|
| `find_by_id` | Find timezone by exact ID |
| `find_my_id` | Detect system's local timezone |
| `find_by_pattern` | Search zones by substring |
| `get_transition_at_epoch` | Query offset at specific time |

```bash
# Build and run examples
make build-examples
./bin/examples/find_by_id
```

## Dependencies

| Package | Version | Purpose |
|---------|---------|---------|
| `functional` | ^3.0.0 | Result/Option monads |
| `gnatcoll` | ^25.0.0 | GNAT Components Collection |

**Note:** Domain layer has zero external dependencies (pure Ada 2022).

## Submodule Management

This project uses git submodules:
- `scripts/python` - Build, release, and architecture scripts
- `docs/common` - Shared documentation

```bash
# Initialize submodules
make submodule-init

# Update submodules
make submodule-update
```

## Contributing

This project is not open to external contributions at this time.

## AI Assistance & Authorship

This project — including its source code, tests, documentation, and other deliverables — is designed, implemented, and maintained by human developers, with Michael Gardner as the Principal Software Engineer and project lead.

We use AI coding assistants (such as OpenAI GPT models and Anthropic Claude Code) as part of the development workflow to help with:

- drafting and refactoring code and tests,
- exploring design and implementation alternatives,
- generating or refining documentation and examples,
- and performing tedious and error-prone chores.

AI systems are treated as tools, not authors. All changes are reviewed, adapted, and integrated by the human maintainers, who remain fully responsible for the architecture, correctness, and licensing of this project.

## License

Copyright 2025 Michael Gardner, A Bit of Help, Inc.

Licensed under the BSD-3-Clause License. See [LICENSE](LICENSE) for details.

## Author

Michael Gardner
A Bit of Help, Inc.
https://github.com/abitofhelp

## Project Status

**Status:** Released (v2.0.0)

- TZif v1/v2/v3 binary parsing (RFC 9636)
- 4-layer hexagonal architecture
- Public API facade with stable interface
- Cross-platform: POSIX and Windows
- Comprehensive test coverage (90%+)
- Comprehensive documentation
- 11 example programs
