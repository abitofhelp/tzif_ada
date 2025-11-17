# Changelog

**Version:** 1.0.0
**Date:** November 16, 2025
**SPDX-License-Identifier:** BSD-3-Clause
**License File:** See the LICENSE file in the project root.
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.
**Status:** Released  


All notable changes to TZif - IANA Timezone Information Library for Ada 2022 will be documented in this file.

The format is based on [Common Changelog](https://common-changelog.org),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

## [Unreleased]

---

## [1.0.0] - 2025-11-16

_Initial production release with SPARK-compatible architecture._

This is the first release of TZif, a production-ready IANA timezone information library for Ada 2022. The library provides comprehensive support for parsing and working with IANA TZif format files using pure functional error handling with the Result monad pattern. This release features a SPARK-compatible I/O plugin architecture enabling formal verification of core domain logic.

### Features

#### Core Capabilities
- Complete parser for IANA TZif file format (versions 1, 2, and 3)
- Support for IANA timezone database (tzdb) version 2025b
- Pure functional error handling using Result monad - NO EXCEPTIONS in business logic
- Hexagonal/Clean architecture with clear layer separation
- Multi-platform support (POSIX: Linux/macOS, Windows)
- High-performance in-memory caching with serialization
- ULID-based source tracking and version management
- SPARK-compatible I/O plugin architecture with formal package pattern
- `TZif.API.Desktop` - Desktop profile API assembly with I/O wiring

#### Value Objects
- `Zone_Id` - Timezone identifiers with validated construction
- `UTC_Offset` - Timezone offset from UTC (±26 hours for historical LMT support)
- `Epoch_Seconds` - Unix epoch timestamps
- `Transition` - Timezone transition points
- `Source_Info` - ULID-based source metadata

#### Architecture
- **Domain Layer**: Pure business logic with Ada 2022 contracts, no dependencies
- **Application Layer**: 13 use cases with type-safe inbound ports
- **Infrastructure Layer**: File system adapter, TZif parser, platform abstraction
- **Error Handling**: Generic Result monad with composable error types
- **Port-Adapter Pattern**: Generic-based (not inheritance)
- **I/O Plugin Pattern**: Generic operations use formal packages for Result types, all I/O operations injected as generic parameters

#### Build Profiles
Six pre-configured build profiles for different deployment targets:
- **Standard**: Desktop/server (1+ GB RAM)
- **Embedded**: Ravenscar embedded (512KB+ RAM)
- **Concurrent**: Multi-threaded (1+ GB RAM)
- **Baremetal**: Zero footprint (128KB+ RAM)
- **STM32H7S78**: STM32H7S78-DK (620KB + 32MB PSRAM)
- **STM32MP135**: STM32MP135F-DK Linux (512 MB)

#### Testing
- **217 total tests** (all passing):
  - 118 integration tests
  - 86 unit tests
  - 13 example E2E tests
- Comprehensive test infrastructure with Result pattern validation
- Test organization by layer and use case
- Full-scale validation test covering all 598 IANA zones across 22 test epochs (13,156 validation checks)

#### Documentation
- Complete API documentation for all public packages
- Architecture guides (hexagonal/clean architecture)
- Error handling strategy guide (Result monad pattern)
- Build profile configuration guide
- Quick start guide
- 7 UML sequence diagrams for use case flows
- Software design specification
- Software requirements specification
- Test organization and infrastructure guides

### Technical Specifications

#### Language & Standards
- Ada 2022 with aspects and contracts
- Pure functional approach - NO EXCEPTIONS in business logic
- Memory safe with bounded types and stack allocation
- Strong typing with Ada 2022 postconditions
- SPARK-compatible core domain logic

#### Dependencies
- `functional` ^2.0.0 - Result monad and Option types
- `gnatcoll` ^25.0.0 - GNAT Components Collection
- GNAT Community 2024 or later

#### Architecture Patterns
- **Result Monad**: All fallible operations return `Result[T, Error]`
- **Value Object Pattern**: Private types with validated constructors
- **Child Package Pattern**: `.Result` packages provide validation
- **Repository Pattern**: Abstract data access
- **Cache Pattern**: Write-through caching strategy
- **Railway-Oriented Programming**: Explicit error paths as values
- **I/O Plugin Pattern**: Dependency injection via generics for SPARK compatibility

#### Error Handling
- NO EXCEPTIONS policy in business logic
- Four error kinds: `Validation_Error`, `Parse_Error`, `IO_Error`, `Not_Found_Error`
- Exception boundary in infrastructure layer (catches and transforms to Result)
- Functional error composition
- All error messages include diagnostic information

#### Quality Assurance
- GNAT style validation (80 character lines, proper formatting)
- Automated architecture layer dependency enforcement
- Modern `with Aspect` syntax throughout (no legacy pragmas)
- All public APIs fully documented with Ada 2022 headers
- Zero compiler warnings
- Zero style violations

#### Performance
- O(1) hash-based zone lookup
- Lazy parsing (files parsed on first access)
- Memory efficient bounded types
- Fast startup with optional cache pre-loading

#### Compatibility
- **Platforms**: POSIX (Linux, macOS), Windows
- **Architectures**: x86-64, ARM, STM32
- **Compilers**: GNAT Community 2024+, GNAT FSF 14.2+, GNAT Pro 25.0+
- **IANA tzdb**: All versions supported (tested with 2025b)

### API Functions (13 operations)
- `Find_By_Id` - Lookup zone by exact identifier
- `Find_My_Id` - Discover local system timezone
- `Get_Transition_At_Epoch` - Query timezone offset at specific time
- `List_All_Zones` - Enumerate all available timezones
- `Find_By_Pattern` - Search zones by substring
- `Find_By_Region` - Search zones by geographic region
- `Find_By_Regex` - Search zones by regular expression
- `Get_Version` - Query database version
- `Discover_Sources` - Scan filesystem for timezone sources
- `Load_Source` - Load timezone data source
- `Validate_Source` - Validate source integrity
- `Import_Cache` - Import zone cache from JSON
- `Export_Cache` - Export zone cache to JSON

### Changed
- Refactored Application.Operations to use SPARK-compatible I/O plugin pattern
- Modified Desktop infrastructure to use package renames for Result type consistency
- Expanded `UTC_Offset_Type` range from ±15 hours to ±26 hours to support historical Local Mean Time (LMT) offsets
  - Historical timezones like Asia/Manila (-15.94h) and America/Juneau (+15.04h) now supported
- Improved `test-all` Makefile target to run all test suites explicitly
  - Now explicitly depends on: test-unit, test-integration, test-api, test-validation, test-full-scale

### Added
- SPARK I/O plugin architecture with formal package pattern
- `TZif.API.Desktop` - Desktop profile API assembly with I/O wiring
- API validation tests in `test/api/` directory
- Formal package strategy resolves Result type ambiguity in generics
- Full-scale validation test covering all 598 IANA zones

### Fixed
- **Critical**: File handle leak in `tzif_parser.adb` when `Parse_From_Stream` raises exception
  - Added exception handler block to ensure file is always closed
  - Resolves "reopening shared file" errors on subsequent access
- **Critical**: Exception handlers now preserve diagnostic information
  - All error messages now include `Exception_Message(Occ)` details
  - Desktop I/O handlers now include file paths in error messages
  - Enables proper root cause analysis instead of generic error messages
- **Critical**: Range check failures with historical LMT offsets exceeding ±15 hours
  - UTC_Offset_Type now supports ±26 hours for pre-standardization timezones
- Result type ambiguity in generic Application.Operations using qualified names
- Discovery operation type mismatch by using shared Result package instances
- Percentage formatting in test reports now shows "100%" instead of "1.00000E+02%"
  - Changed Pass_Rate type from Float to Natural in all test files
- Style warnings for lines exceeding 80 characters across all test files
  - Fixed duplicate comment prefixes from gnatformat
  - Wrapped long variable declarations, Assert statements, and Put_Line calls
- Compilation errors in `test/validation/check_errors.adb` due to `Error_Type` visibility
  - Fully qualified type name: `TZif.Domain.Error.Error_Type`

### License

BSD-3-Clause - Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.

---

## License & Copyright

- **License**: BSD-3-Clause
- **Copyright**: © 2025 Michael Gardner, A Bit of Help, Inc.
- **SPDX-License-Identifier**: BSD-3-Clause
