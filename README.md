# TZif - IANA Timezone Information Library

[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE) [![Ada](https://img.shields.io/badge/Ada-2022-blue.svg)](https://ada-lang.io) [![Alire](https://img.shields.io/badge/Alire-2.0+-blue.svg)](https://alire.ada.dev)

**Version:** 1.0.0<br>
**Date:** 2025-11-29<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

## Overview

TZif is an Ada 2022 library for parsing and querying IANA's compiled timezone information (TZif format, RFC 9636). It provides a clean, functional API for timezone operations with Result monad error handling, hexagonal architecture, and embedded-safe patterns.

## Features

- ✅ Parse IANA TZif binary files (version 2/3)
- ✅ Query timezone transitions at any epoch time
- ✅ Discover and validate timezone sources
- ✅ Find zones by ID, pattern, region, or regex
- ✅ Detect system's local timezone
- ✅ 4-layer hexagonal architecture (Domain, Application, Infrastructure, API)
- ✅ Result monad error handling (via `functional` crate)
- ✅ Public API facade with stable interface
- ✅ Generic I/O plugin pattern for platform portability
- ✅ RFC 9636 compliant

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                       TZif.API                              │
│              (Public Facade - Stable Interface)             │
├─────────────────────────────────────────────────────────────┤
│  API.Operations     │     API.Desktop     │   (API.Embedded)│
│  (Generic I/O)      │ (File System DI)    │   (Future)      │
├─────────────────────┼─────────────────────┼─────────────────┤
│                    Application Layer                        │
│     Use Cases  │  Ports (Inbound/Outbound)  │  Operations   │
├─────────────────────────────────────────────────────────────┤
│                   Infrastructure Layer                      │
│        Adapters (File System, Parser, Repository)           │
├─────────────────────────────────────────────────────────────┤
│                      Domain Layer                           │
│   Entities (Zone) │ Value Objects │ Errors │ Result Monad   │
└─────────────────────────────────────────────────────────────┘
```

## Quick Start

### Building

```bash
# Build the library
make build

# Build release version
make build-release

# Using Alire directly
alr build
```

### Using in Your Project

Add to your `alire.toml`:

```toml
[[depends-on]]
tzif = "^1.0.0"
```

## Usage

### Find System Timezone

```ada
with TZif.API;

procedure Main is
   use TZif.API;

   Result : constant My_Zone_Result := Find_My_Id;
begin
   if Is_Ok (Result) then
      Put_Line ("System timezone: " & To_String (Value (Result)));
   else
      Put_Line ("Could not detect timezone");
   end if;
end Main;
```

### Query Timezone Transition

```ada
with TZif.API;

procedure Main is
   use TZif.API;

   Zone_Id : constant Zone_Id_String := Make_Zone_Id_String ("America/New_York");
   Epoch   : constant Epoch_Seconds_Type := 1700000000;  -- Nov 2023
   Result  : constant Transition_Result :=
     Get_Transition_At_Epoch (Zone_Id, Epoch);
begin
   if Is_Ok (Result) then
      --  Access transition info (offset, DST status, etc.)
      null;
   else
      Put_Line ("Error: " & Error_Strings.To_String (Error_Info (Result)));
   end if;
end Main;
```

### List All Timezones

```ada
with TZif.API;

procedure Main is
   use TZif.API;

   --  First load a timezone source
   Source_Result : constant Load_Source_Result :=
     Load_Source (Make_Path_String ("/usr/share/zoneinfo"));
begin
   if Load_Port.Load_Source_Result_Package.Is_Ok (Source_Result) then
      declare
         Source : constant Source_Info_Type :=
           Load_Port.Load_Source_Result_Package.Value (Source_Result);
         Zones : constant Zone_List_Result := List_All_Zones (Source);
      begin
         --  Process zone list...
         null;
      end;
   end if;
end Main;
```

## Testing

```bash
# Run all tests
make test-all

# Run unit tests only
make test-unit

# Run integration tests only
make test-integration
```

**Test Results**: All 126 tests passing

## Documentation

- 📚 **[Documentation Index](docs/index.md)** - Complete documentation overview
- 🚀 **[Quick Start Guide](docs/quick_start.md)** - Get started in minutes
- 🏗️ **[All About Our API](docs/guides/all_about_our_api.md)** - API architecture and platform customization
- 📖 **[Software Requirements Specification](docs/formal/software_requirements_specification.md)**
- 🏗️ **[Software Design Specification](docs/formal/software_design_specification.md)**
- 🧪 **[Software Test Guide](docs/formal/software_test_guide.md)**
- 🗺️ **[Roadmap](docs/roadmap.md)** - Future plans
- 📝 **[CHANGELOG](CHANGELOG.md)** - Release history

### Examples

The `examples/` directory contains working programs demonstrating each API operation:

| Example | Description |
|---------|-------------|
| `find_my_id` | Detect system's local timezone |
| `find_by_id` | Look up zone by exact identifier |
| `find_by_pattern` | Search zones by substring |
| `find_by_region` | Search zones by geographic region |
| `find_by_regex` | Search zones by regular expression |
| `get_transition_at_epoch` | Query offset at specific time |
| `get_version` | Query timezone database version |
| `list_all_zones` | Enumerate all available timezones |
| `discover_sources` | Find timezone data locations |
| `load_source` | Load timezone source |
| `validate_source` | Validate source integrity |

## Code Standards

This project follows:
- **Ada Agent** (`~/.claude/agents/ada.md`) - Ada 2022 standards
- **Architecture Agent** (`~/.claude/agents/architecture.md`) - DDD/Clean/Hexagonal
- **Functional Agent** (`~/.claude/agents/functional.md`) - Result/Option patterns

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

Copyright © 2025 Michael Gardner, A Bit of Help, Inc.

Licensed under the BSD-3-Clause License. See [LICENSE](LICENSE) for details.

## Author

Michael Gardner
A Bit of Help, Inc.
https://github.com/abitofhelp

## Project Status

**Status**: Released (v1.0.0)

- ✅ TZif v2/v3 binary parsing (RFC 9636)
- ✅ 4-layer hexagonal architecture
- ✅ Public API facade with stable interface
- ✅ Desktop platform support (file system)
- ✅ Full test suite (126 tests)
- ✅ Comprehensive documentation
- ✅ 11 example programs
- ✅ Alire publication
