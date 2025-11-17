# TZif - IANA Timezone Information Library for Ada 2022

[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)
[![Ada](https://img.shields.io/badge/Ada-2022-blue.svg)](https://ada-lang.io)
[![Alire](https://img.shields.io/badge/Alire-2.0-blue.svg)](https://alire.ada.dev)

---

> **📦 COMPANION PROJECT ALERT**
>
> **[ZoneInfo](https://github.com/abitofhelp/zoneinfo)** provides a comprehensive, application-ready API for the IANA timezone database, with automatic timezone detection, DST handling, and querying. It will be available around **November 21, 2025**.
>
> **Use TZif directly** if you need fine-grained control over TZif file parsing and caching.
> **Use ZoneInfo** for most application development scenarios.

---

**Production-ready timezone parsing and query engine for Ada desktop and embedded applications.**

TZif brings comprehensive IANA timezone support to Ada 2022 with a unique combination of formal methods readiness, zero-exception error handling, and flexible I/O architecture suitable for everything from high-performance servers to resource-constrained microcontrollers.

---

## Why TZif?

- ✅ **SPARK-Compatible Architecture** - I/O plugin system enables formal verification of domain logic
- ✅ **Railway-Oriented Programming** - Type-safe Result monads, NO EXCEPTIONS in business logic
- ✅ **Extensively Validated** - 217 comprehensive tests + full-scale validation across 13,156 timezone calculations (598 zones × 22 test epochs)
- ✅ **Hexagonal Architecture** - Clean separation enables easy testing and platform adaptation
- ✅ **Embedded-Ready** - Six build profiles from bare metal (128KB RAM) to desktop (1GB+ RAM)
- ✅ **Complete TZif Support** - Parse IANA timezone files (versions 1, 2, 3) including tzdb 2025b
- ✅ **Cross-Platform** - POSIX (Linux, macOS, BSD) and Windows support

---

## Features

### Core Capabilities

- **TZif Parsing** - Full support for TZif binary format versions 1, 2, and 3 (RFC 9636)
- **IANA tzdb Support** - Tested with IANA timezone database version 2025b
- **Timezone Queries** - Find by exact ID, region, pattern, or regex
- **Transition Lookups** - Get UTC offset and timezone info for any epoch (±26 hour range for historical LMT)
- **Source Discovery** - Scan filesystem for timezone data sources
- **Cache Management** - High-performance in-memory caching with JSON export/import
- **Thread-Safe** - Safe concurrent operations for multi-threaded applications

### SPARK & Formal Verification

- ✅ **SPARK-Compatible Core** - Domain layer designed for formal verification
- ✅ **I/O Plugin Architecture** - Generic operations with formal packages enable SPARK verification
- ✅ **Pure Domain Logic** - Zero dependencies in domain layer, contracts with Ada 2022 aspects
- ✅ **Memory Safe** - Bounded types, stack allocation, no dynamic memory in core logic

### Embedded & Resource-Constrained Systems

- ✅ **Multiple Build Profiles** - Standard, Embedded (Ravenscar), Bare Metal, STM32 targets
- ✅ **Ravenscar Profile** - Safety-critical embedded systems (512KB+ RAM)
- ✅ **Zero Footprint** - Bare metal profile for microcontrollers (128KB+ RAM)
- ✅ **STM32 Support** - Pre-configured profiles for STM32H7S78-DK and STM32MP135F-DK
- ✅ **Configurable Limits** - Memory bounds tunable via `TZif_Config` package

### Architecture Highlights

- **SPARK I/O Plugin Pattern** - Domain and application layers are SPARK-compatible; I/O injected via generic formal packages
- **Result Monad Error Handling** - All fallible operations return `Result[T, Error]` - no hidden exceptions
- **Hexagonal (Ports and Adapters)** - Domain logic isolated from infrastructure
- **Generic-Based Dependency Injection** - Type-safe I/O plugins without runtime overhead
- **Platform Abstraction** - Swap I/O implementations for Desktop, Embedded, or Test environments

---

## Quick Example

```ada
with TZif.API;
with Ada.Text_IO; use Ada.Text_IO;

procedure Check_Meeting_Time is
   use TZif.API;

   -- Find timezone
   NY_Zone : constant Zone_Result := Find_By_Id (Make_Zone_Id ("America/New_York"));

   -- Get offset at specific time (Unix epoch)
   Meeting_Time : constant Epoch_Seconds_Type := 1_735_689_600; -- Jan 1, 2025
   Transition   : constant Transition_Result :=
      Get_Transition_At_Epoch (Make_Zone_Id ("America/New_York"), Meeting_Time);

begin
   -- Railway-oriented: explicit success/error handling
   if Is_Ok (NY_Zone) and Is_Ok (Transition) then
      -- Process timezone data (no exceptions thrown!)
      Put_Line ("UTC Offset: " & Get_UTC_Offset (Value (Transition))'Image);
   else
      -- Handle error
      Put_Line ("Timezone lookup failed");
   end if;
end Check_Meeting_Time;
```

**13 API Operations**: `Find_By_Id`, `Find_My_Id`, `Get_Transition_At_Epoch`, `List_All_Zones`, `Find_By_Pattern`, `Find_By_Region`, `Find_By_Regex`, `Get_Version`, `Discover_Sources`, `Load_Source`, `Validate_Source`, `Import_Cache`, `Export_Cache`

---

## Installation

### Using Alire (Recommended)

```bash
# Add to your project
alr with tzif

# Or get standalone
alr get tzif
cd tzif_1.0.0_*
alr build
```

### Manual Build

```bash
git clone https://github.com/abitofhelp/tzif.git
cd tzif
alr build
```

---

## Build Profiles

TZif supports six pre-configured build profiles optimized for different deployment targets:

| Profile | Runtime | Memory | Use Case |
|---------|---------|--------|----------|
| **Standard** | Full Ada | 1+ GB RAM | Desktop/Server applications |
| **Embedded** | Ravenscar | 512KB+ RAM | Safety-critical embedded systems |
| **Concurrent** | Multi-threaded | 1+ GB RAM | High-performance parallel processing |
| **Baremetal** | Zero Footprint | 128KB+ RAM | Microcontrollers (minimal zones) |
| **STM32H7S78** | Ravenscar | 620KB + PSRAM | STM32H7S78-DK boards |
| **STM32MP135** | Linux | 512 MB | STM32MP135F-DK Linux systems |

```bash
# Switch to embedded profile
cp config/profiles/embedded/tzif_config.ads config/tzif_config.ads
alr build
```

See [Build Profiles Guide](docs/guides/build_profiles.md) for detailed configuration.

---

## Testing

```bash
# Run all tests
make test-all

# Run with coverage analysis
make test-coverage

# Run full-scale validation (598 zones × 22 epochs = 13,156 checks)
make test-full-scale
```

**Test Results**: ✅ All 217 tests passing (86 unit + 118 integration + 13 examples)

**Validation**: ✅ Full-scale validation across all 598 IANA timezone zones

---

## Documentation

### Quick Start
- 🚀 **[Quick Start Guide](docs/quick_start.md)** - Get up and running in 5 minutes

### Formal Documentation
- 📋 **[Software Requirements Specification (SRS)](docs/formal/software_requirements_specification.md)** - Complete functional and non-functional requirements
- 🏗️ **[Software Design Specification (SDS)](docs/formal/software_design_specification.md)** - Architecture, patterns, and design decisions
- 🧪 **[Software Test Guide](docs/formal/software_test_guide.md)** - Testing strategy, organization, and procedures

### Development Guides
- 📚 **[Development Guides Index](docs/guides/index.md)** - Comprehensive guides covering:
  - **[Hexagonal Architecture](docs/guides/hybrid-architecture/index.md)** - Complete architectural foundations
  - **[Error Handling Strategy](docs/guides/error_handling_strategy.md)** - Railway-oriented programming with Result monads
  - **[Build Profiles](docs/guides/build_profiles.md)** - Configuration for different deployment targets
  - **[Architecture Enforcement](docs/guides/architecture_enforcement.md)** - Layer dependency validation
  - And more...

### Visual Documentation
- 🎨 **[UML Diagrams](docs/diagrams/index.md)** - 7 sequence diagrams showing API flows

### Release Information
- 📝 **[CHANGELOG](CHANGELOG.md)** - Detailed release history
- 🗺️ **[ROADMAP](docs/roadmap.md)** - Future development plans

---

## Architecture Overview

TZif uses **Hexagonal Architecture** (Ports and Adapters) with SPARK-compatible I/O injection:

```
┌─────────────────────────────────────────┐
│         Application Layer               │  ← SPARK-Compatible
│  (13 API Operations, Type-Safe Ports)   │     (I/O injected via generics)
│                                          │
│  ┌────────────────────────────────────┐ │
│  │      Domain Layer                  │ │  ← SPARK-Compatible
│  │  (Pure Business Logic, Contracts)  │ │     (No I/O, No Exceptions)
│  └────────────────────────────────────┘ │
└─────────────────────────────────────────┘
           ↑                    ↑
           │ Plugin I/O         │ Plugin I/O
┌──────────┴──────────┐  ┌─────┴──────────┐
│  Desktop I/O        │  │  Embedded I/O   │  ← Infrastructure Layer
│  (File System)      │  │  (Platform API) │     (Actual I/O operations)
└─────────────────────┘  └─────────────────┘
```

**Key Patterns**:
- **I/O Plugin via Generics** - Domain logic verified with SPARK; I/O swapped at compile-time
- **Railway-Oriented Error Handling** - Result monad eliminates exceptions
- **Repository Pattern** - Abstract data access via ports
- **Value Objects** - Validated construction prevents invalid states

---

## SPARK Formal Verification Support

TZif v1.0.0 lays the foundation for **formal verification with SPARK**:

- ✅ **Domain Layer** - 100% SPARK-compatible (pure functions, no I/O, no exceptions)
- ✅ **Application Layer** - SPARK-compatible generic operations (I/O injected as parameters)
- ✅ **I/O Plugin Architecture** - Generic formal packages enable type-safe dependency injection
- 🔜 **Future** (v1.1.0) - Full SPARK verification with contracts and proofs

**Why This Matters**: Safety-critical applications can formally prove correctness of timezone calculations while maintaining flexibility for different I/O backends (Desktop, Embedded, Mock for testing).

---

## Use Cases

### Desktop Applications
- **Distributed Systems** - Coordinate events across timezones
- **Scheduling Applications** - Handle DST transitions correctly
- **Log Analysis** - Convert timestamps between zones
- **Travel/Booking Systems** - Display times in user's local timezone

### Embedded Systems
- **IoT Devices** - Local time display with accurate DST handling
- **Industrial Controllers** - Timestamp events in local time zones
- **Automotive Systems** - Automatic timezone detection and conversion
- **Avionics** - Safety-critical time calculations with SPARK verification

---

## Requirements

- **Compiler**: GNAT FSF 14.2+ or GNAT Pro 25.0+
- **Ada Version**: Ada 2022
- **Build System**: Alire 2.0+
- **Dependencies**:
  - `functional` ^2.0.0 (Result/Option monads)
  - `gnatcoll` ^25.0.0 (JSON serialization)

---

## Examples

See `examples/` directory for **13 working examples** demonstrating all API operations:

- `find_by_id.adb` - Find timezone by exact ID
- `find_my_id.adb` - Detect local system timezone
- `get_transition_at_epoch.adb` - Lookup timezone offset at specific time
- `list_all_zones.adb` - Enumerate all available timezones
- `discover_sources.adb` - Scan filesystem for timezone sources
- `get_version.adb` - Get IANA tzdb version
- `validate_source.adb` - Validate timezone source
- `load_source.adb` - Load timezone source
- `find_by_pattern.adb` - Pattern matching search
- `find_by_region.adb` - Region-based search
- `find_by_regex.adb` - Regex search
- `import_cache.adb` / `export_cache.adb` - Cache serialization
- `cache_export_import.adb` - Cache round-trip

```bash
# Build and run examples
make build-examples
./bin/examples/find_by_id
```

---

## Contributing

Contributions welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for:
- Code style guidelines (GNAT style, 80 character lines)
- Testing requirements (unit + integration tests)
- Architecture constraints (hexagonal architecture enforcement)
- Pull request process

---

## Project Status

**Status**: Production Ready (v1.0.0) - Released November 16, 2025

- ✅ All 13 API operations implemented and tested
- ✅ Comprehensive test coverage (217 tests + 13,156 validation checks)
- ✅ Complete documentation (SRS, SDS, Test Guide, development guides)
- ✅ Zero compiler warnings, zero style violations
- ✅ SPARK-compatible architecture for future formal verification
- ✅ Multi-platform support (Linux, macOS, BSD, Windows)
- ✅ Ready for production use in desktop and embedded applications

---

## Roadmap

**v1.1.0** (Q1 2026):
- Full SPARK verification of domain layer
- Complete Windows platform testing
- Cache performance monitoring

See [Roadmap](docs/roadmap.md) for detailed future plans.

---

## License

Copyright © 2025 Michael Gardner, A Bit of Help, Inc.

Licensed under the **BSD-3-Clause License**. See [LICENSE](LICENSE) for details.

---

## Support

For issues, questions, or contributions:
- 📧 **Email**: support@abitofhelp.com
- 🐛 **Issues**: [GitHub Issues](https://github.com/abitofhelp/tzif/issues)
- 📖 **Documentation**: See `docs/` directory for comprehensive guides
- 💬 **Discussions**: [GitHub Discussions](https://github.com/abitofhelp/tzif/discussions)

---

**TZif** - Professional timezone handling for Ada, from bare metal to the cloud.
