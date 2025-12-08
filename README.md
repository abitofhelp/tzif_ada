# IANA Timezone Information Library

[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE) [![Ada](https://img.shields.io/badge/Ada-2022-blue.svg)](https://ada-lang.io) [![SPARK](https://img.shields.io/badge/SPARK-Proved-brightgreen.svg)](https://www.adacore.com/about-spark) [![Alire](https://img.shields.io/badge/Alire-2.0+-blue.svg)](https://alire.ada.dev) [![Windows](https://img.shields.io/badge/Windows-11-0078D6.svg)](https://github.com/abitofhelp/tzif/actions)

**Version:** 1.1.0<br>
**Date:** December 06, 2025<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released  

## Overview

TZif is an Ada 2022 library for parsing and querying IANA's compiled timezone information (TZif format, RFC 9636). It provides a clean, functional API for timezone operations with Result monad error handling, hexagonal architecture, and embedded-safe patterns.

## SPARK Formal Verification

<table>
<tr>
<td width="120"><strong>Status</strong></td>
<td><img src="https://img.shields.io/badge/SPARK-Proved-brightgreen.svg" alt="SPARK Proved"></td>
</tr>
<tr>
<td><strong>Scope</strong></td>
<td>Domain Layer (entities, value objects, parser, services)</td>
</tr>
<tr>
<td><strong>Mode</strong></td>
<td>gnatprove --mode=prove --level=2 (full proof)</td>
</tr>
<tr>
<td><strong>Results</strong></td>
<td>1350 checks: 66 flow, 1089 proved, 195 unproved (complex parser)</td>
</tr>
</table>

The **domain layer** is formally verified using SPARK Ada, providing mathematical guarantees of:

- **No runtime errors** - Division by zero, overflow, range violations
- **No uninitialized data** - All variables properly initialized before use
- **Contract compliance** - Pre/postconditions proven correct
- **Data flow integrity** - No aliasing or information flow violations

### Verification Commands

```bash
make spark-check    # Run SPARK legality verification (fast)
make spark-prove    # Run full SPARK proof verification
```

### Verified Packages

| Package | SPARK_Mode | Description |
|---------|-----------|-------------|
| `TZif.Domain.Entity.*` | On | Zone entity and operations |
| `TZif.Domain.Value_Object.*` | On | All value objects (Epoch, Offset, Header, etc.) |
| `TZif.Domain.Parser` | On | TZif binary file parser |
| `TZif.Domain.Service.*` | On | Domain services |
| `TZif.Domain.Error.*` | On | Error types and Result monad |
| `TZif.Domain.Types.*` | On | Bounded vectors, Option type |

Infrastructure and API layers use `SPARK_Mode => Off` as they perform I/O operations.

## Getting Started

### Clone with Submodules

This repository uses git submodules for shared tooling. Clone with:

```bash
git clone --recurse-submodules https://github.com/abitofhelp/tzif.git
```

Or if already cloned without submodules:

```bash
git submodule update --init --recursive
# Or: make submodule-init
```

## Features

- ✅ Parse IANA TZif binary files (version 2/3)
- ✅ Query timezone transitions at any epoch time
- ✅ Discover and validate timezone sources
- ✅ Find zones by ID, pattern, region, or regex
- ✅ Detect the system's local timezone
- ✅ Cross-platform: Linux, macOS, BSD, Windows 11
- ✅ 4-layer hexagonal architecture (Domain, Application, Infrastructure, API)
- ✅ Result monad error handling (via `functional` crate)
- ✅ Public API facade with stable interface
- ✅ Generic Repository Pattern for platform abstraction
- ✅ RFC 9636 compliant

## Platform Support

| Platform | Status | Notes |
|----------|--------|-------|
| **Linux** | ✅ Full | All distributions with `/usr/share/zoneinfo` |
| **macOS** | ✅ Full | All versions with `/var/db/timezone/zoneinfo` |
| **FreeBSD/NetBSD/OpenBSD** | ✅ Full | Standard zoneinfo paths |
| **Windows 11** | ✅ Full | CLDR mapping from Windows timezone names to IANA |
| **Embedded** | 🔧 Custom | Requires I/O plugin implementation |

### Windows Support

Windows 11 is fully supported via the Generic Repository Pattern with platform-specific adapters:

- **Timezone Detection**: Uses Win32 API with CLDR mapping to convert Windows timezone names (e.g., "Pacific Standard Time") to IANA identifiers (e.g., "America/Los_Angeles")
- **Zone Lookup**: Requires bundled zoneinfo files (Windows does not ship TZif files natively)
- **CI/CD**: Full Windows CI via GitHub Actions

**Setup for Windows**:
```bash
# Build for Windows
alr build -- -XTZIF_OS=windows

# Run tests
make test
```

**Note**: `Find_My_Id` on Windows uses CLDR mapping. Deploy bundled zoneinfo files for zone lookups.

### Platform Abstraction

TZif uses a **multi-package API pattern** with dependency injection for platform portability:

| Package | Purpose |
|---------|---------|
| `TZif.API.Operations` | Generic operations (SPARK-safe, no I/O dependencies) |
| `TZif.API.Desktop` | Composition root for POSIX (Linux/macOS/BSD) |
| `TZif.API.Windows` | Composition root for Windows 11 |
| `TZif.API` | Public facade (auto-selects based on platform) |

**Platform Selection**: GPR automatically selects the correct composition root based on the `TZIF_OS` scenario variable.

**For embedded platforms**, create your own composition root:

```ada
--  1. Implement the I/O port for your platform
function Flash_Read_Zone (Path : Path_String) return Zone_Data_Result;

--  2. Instantiate operations with your I/O adapter
package Embedded_Ops is new TZif.API.Operations (Read_Zone => Flash_Read_Zone);

--  3. Use operations directly
Result : constant Zone_Result := Embedded_Ops.Find_By_Id (Zone_Id);
```

See **[All About Our API](docs/guides/all_about_our_api.md)** for detailed architecture and implementation guidance.

## Architecture

```
┌───────────────────────────────────────────────────────────────────┐
│                           TZif.API                                │
│                (Public Facade - Stable Interface)                 │
├───────────────────────────────────────────────────────────────────┤
│  API.Operations  │  API.Desktop  │  API.Windows  │  API.Embedded  │
│  (Generic I/O)   │  (POSIX DI)   │  (Win32 DI)   │   (Custom)     │
├──────────────────┴───────────────┴───────────────┴────────────────┤
│                      Application Layer                            │
│       Use Cases  │  Ports (Inbound/Outbound)  │  Operations       │
├───────────────────────────────────────────────────────────────────┤
│                     Infrastructure Layer                          │
│  ┌─────────────────────────────────────────────────────────────┐  │
│  │  Platform Adapters: POSIX_Repository │ Windows_Repository   │  │
│  │  Generic Repository Pattern with Platform_Ops injection     │  │
│  └─────────────────────────────────────────────────────────────┘  │
│              Parser  │  Repository  │  Platform Abstraction       │
├───────────────────────────────────────────────────────────────────┤
│                        Domain Layer                               │
│     Entities (Zone) │ Value Objects │ Errors │ Result Monad       │
└───────────────────────────────────────────────────────────────────┘
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

## Quick Snippets

All operations use `TZif.API` and return Result types. See `/examples` for complete programs.

```ada
with TZif.API; use TZif.API;

--  Detect system timezone
Result : constant My_Zone_Result := Find_My_Id;

--  Find zone by exact ID
Result : constant Zone_Result := Find_By_Id (Make_Zone_Id ("America/New_York"));

--  Get transition info at epoch time
Result : constant Transition_Result :=
  Get_Transition_At_Epoch (Make_Zone_Id_String ("America/New_York"), 1700000000);

--  Get timezone database version
Result : constant Version_Result := Get_Version (Source);

--  Discover available timezone sources
Result : constant Sources_Result := Discover_Sources;

--  Load a specific timezone source
Result : constant Load_Source_Result :=
  Load_Source (Make_Path_String ("/usr/share/zoneinfo"));

--  Validate source integrity
Result : constant Validate_Result := Validate_Source (Source);

--  List all zones in a source
Result : constant Zone_List_Result := List_All_Zones (Source);

--  Search by substring pattern
Result : constant Zone_List_Result := Find_By_Pattern (Source, "New");

--  Search by geographic region
Result : constant Zone_List_Result := Find_By_Region (Source, "America");

--  Search by regex
Result : constant Zone_List_Result := Find_By_Regex (Source, "America/New.*");
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

**Test Results**: All 437 tests passing (295 unit + 131 integration + 11 examples)

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

## Submodule Management

This project uses git submodules for shared Python tooling:

- `scripts/python` - Build, release, and architecture scripts
- `test/python` - Shared test fixtures and configuration

### Workflow

```
hybrid_python_scripts (source repo)
         │
         │ git push (manual)
         ▼
      GitHub
         │
         │ make submodule-update (in each consuming repo)
         ▼
┌─────────────────────────────────┐
│  1. Pull new submodule commit   │
│  2. Stage reference change      │
│  3. Commit locally              │
│  4. Push to remote              │
└─────────────────────────────────┘
```

### Commands

```bash
# After fresh clone
make submodule-init

# Pull latest from submodule repos
make submodule-update

# Check current submodule commits
make submodule-status
```

### Bulk Update (all repositories)

```bash
python3 ~/Python/src/github.com/abitofhelp/git/update_submodules.py

# Options:
#   --dry-run   Show what would happen without changes
#   --no-push   Update locally but do not push to remote
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

Copyright © 2025 Michael Gardner, A Bit of Help, Inc.

Licensed under the BSD-3-Clause License. See [LICENSE](LICENSE) for details.

## Author

Michael Gardner
A Bit of Help, Inc.
https://github.com/abitofhelp

## Project Status

**Status**: Released (v1.1.0)

- ✅ TZif v2/v3 binary parsing (RFC 9636)
- ✅ 4-layer hexagonal architecture
- ✅ Public API facade with stable interface
- ✅ Cross-platform: POSIX (Linux/macOS/BSD) and Windows 11
- ✅ Generic Repository Pattern for platform abstraction
- ✅ Full test suite (437 tests)
- ✅ Windows CI via GitHub Actions
- ✅ Comprehensive documentation
- ✅ 11 example programs
- ✅ Alire publication
