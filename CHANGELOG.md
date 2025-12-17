# Changelog

**Version:** 3.0.3<br>
**Date:** 2025-12-17<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [3.0.3] - 2025-12-17

**Tests:** 361 unit + 134 integration + 11 examples = 506 total - All passing<br>
**SPARK Status:** 1248 checks: 48 flow, 999 proved, 201 unproved (~80% proved) (--mode=prove --level=2)<br>

### Changed

- **Domain.Error.Result**: Moved `Is_Ok`/`Is_Error` to expression functions in spec's private section for SPARK prover visibility

---

## [3.0.2] - 2025-12-16

**Tests:** 361 unit + 134 integration + 11 examples = 506 total - All passing<br>
**SPARK Status:** 1248 checks: 48 flow, 999 proved, 201 unproved (~80% proved) (--mode=prove --level=2)<br>

### Changed

- **Domain.Error.Result**: Slimmed to 7 essential operations for SPARK compatibility
  - Kept: `Ok`, `Error`, `From_Error`, `Is_Ok`, `Is_Error`, `Value`, `Error_Info`
  - Removed: `Map`, `And_Then`, `Fallback`, `Recover`, `Unwrap_Or`, `Bimap`, `Ensure`,
    `With_Context`, `Tap`, `Map_Error`, `Fallback_With`, `Recover_With`, `Unwrap_Or_With`,
    `And_Then_Into`
  - Advanced combinators available via `Functional.Result` in infrastructure layer

### Fixed

- **SPARK prover crash**: Eliminated GNAT BUG DETECTED error that occurred in the
  `Fallback` function due to nested generic with enum discriminant. The removed
  combinators were unused in SPARK-proven layers (Domain/Application).

---

## [3.0.1] - 2025-12-15

**Test Coverage:** 361 unit + 134 integration + 11 examples = 506 total
**SPARK Status:** Unchanged from v2.0.0 (1350 checks: 1179 proved, 171 unproved)

### Added

- Unit tests for `Canonicalize` path normalization (10 tests)

### Fixed

- **Cross-platform path handling**: `Get_Zoneinfo_Base` in Windows and Embedded
  IO adapters now uses `Canonicalize` to normalize paths, preventing mixed
  separator issues (e.g., `D:\path/file`) when `TZIF_DATA_PATH` environment
  variable contains backslashes on Windows

---

## [3.0.0] - 2025-12-13

**Test Coverage:** 361 unit + 134 integration + 11 examples = 506 total
**SPARK Status:** Unchanged from v2.0.0 (1350 checks: 1179 proved, 171 unproved)

### Breaking Changes

- **Dependency**: `functional` upgraded from ^3.0.0 to ^4.0.0
  - `Functional.Try.Map_To_Result` pattern replaces manual exception handlers
  - `Functional.Try.Map_To_Result_With_Param` for parameterized operations
  - Smart constructor pattern with declarative exception mapping

### Changed

- Refactored all infrastructure adapters to use `Map_To_Result` pattern
- Desktop adapter: 12 I/O operations converted to `Map_To_Result`
- Windows adapter: Win32 API calls wrapped with `Map_To_Result`
- Embedded adapter: Stub implementations updated for new pattern
- Platform operations: `Read_Symbolic_Link` uses `Map_To_Result`
- Zone repository: All file operations use declarative exception mapping
- Improved exception handling consistency across all I/O boundaries
- All docs regenerated with v3.0.0 structure and accurate test counts

### Added

- Windows platform tests: Win32 GetDynamicTimeZoneInformation + CLDR mapping
- Windows CI workflow with URL pin to functional v4.0.0
- Exception mapping arrays for declarative error categorization
- Resource_Error kind added to Error_Kind enumeration

### Fixed

- Windows CI: functional v4.0.0 not in Alire index, using URL pin
- Style warnings: Line length > 79 chars in Windows adapter code
- Test counts in documentation: 569 total (was incorrectly showing 11)

---

## [2.0.0] - 2025-12-07

**Test Coverage:** 361 unit + 134 integration + 11 examples = 506 total

### Breaking Changes

- **Dependency**: `functional` upgraded from ^2.2.1 to ^3.0.0
  - Result discriminant changed from `Kind : Kind_Type` to `Is_Ok : Boolean`
  - Option discriminant changed from `Kind : Kind_Type` to `Has_Value : Boolean`
  - Constructor `Err(e)` renamed to `New_Error(e)`
  - Predicate `Is_Err(r)` renamed to `Is_Error(r)`
  - Transform `Map_Err(r)` renamed to `Map_Error(r)`
  - Field `Err_Value` renamed to `Error_Value`

### Added

#### SPARK Formal Verification
- Domain layer formally verified using SPARK Ada (gnatprove --mode=prove --level=2)
- Application layer formally verified (operations, inbound ports, outbound ports)
- 1350 checks: 1179 proved, 171 unproved (in generic instantiations)
- Mathematical guarantees: no runtime errors, no uninitialized data, contract compliance
- `make spark-check` for legality verification
- `make spark-prove` for full proof verification

#### Platform Support
- Full Windows support with Desktop I/O adapter
- Windows CI workflow (GitHub Actions)
- Embedded platform stub adapter for custom implementations
- Generic Repository pattern for cross-platform portability

#### Examples
- 11 complete example programs demonstrating all API operations
- `find_by_id` - Find timezone by exact ID
- `find_by_pattern` - Search zones by substring
- `find_by_region` - Search zones by geographic region
- `find_by_regex` - Search zones using regular expressions
- `find_my_id` - Detect system's local timezone
- `get_transition_at_epoch` - Query offset at specific time
- `get_version` - Retrieve IANA database version
- `list_all_zones` - Enumerate all available timezones
- `discover_sources` - Find timezone data directories
- `load_source` - Load a specific timezone data source
- `validate_source` - Validate timezone data integrity

#### Testing
- Expanded test coverage: 567 tests (425 unit + 131 integration + 11 examples)
- Result combinator tests with proper generic instantiation
- Bounded_Vector comprehensive tests

### Changed

- Internal refactoring to use Option types instead of sentinel values
- Boolean discriminants for Result/Option/Either types (SPARK-friendly)
- Examples updated to use non-prefixed Bounded_Vector API calls
- Documentation regenerated with SPARK verification details

### Fixed

- test_result_combinators.adb: And_Then generic instantiation (was using access parameter)
- discover_sources.adb: Bounded_Vector API usage (non-tagged type)
- list_all_zones.adb: Bounded_Vector API usage (non-tagged type)
- Style fixes: reserved words case, line length compliance

### Dependencies

- `functional` ^3.0.0 (Result/Option/Either/Try monads) - **BREAKING**
- `gnatcoll` ^25.0.0 (GNAT Components Collection)

---


## [1.0.0] - 2025-12-02

### Overview
Initial release of TZif, an Ada 2022 library for parsing and querying IANA's
compiled timezone information (TZif format, RFC 9636).

### Added

#### Core Features
- Parse IANA TZif binary files (version 1, 2, and 3)
- Query timezone transitions at any Unix epoch time
- Discover and validate timezone data sources
- Find zones by exact ID, substring pattern, geographic region, or regex
- Detect the system's local timezone (POSIX platforms)
- List all available timezones with configurable sort order

#### API Operations (11 total)
- `Find_By_Id` - Lookup timezone by exact IANA identifier
- `Find_By_Pattern` - Search zones by substring match
- `Find_By_Region` - Search zones by geographic region
- `Find_By_Regex` - Search zones using regular expressions
- `Find_My_Id` - Detect the system's local timezone
- `Get_Transition_At_Epoch` - Query UTC offset and DST at any time
- `Get_Version` - Retrieve IANA database version
- `List_All_Zones` - Enumerate all available timezones
- `Discover_Sources` - Find timezone data directories
- `Load_Source` - Load a specific timezone data source
- `Validate_Source` - Validate timezone data integrity

#### Domain Value Objects
- `Zone_Id` - Validated IANA timezone identifier
- `Epoch_Seconds` - Unix timestamp type
- `UTC_Offset` - Timezone offset from UTC
- `Transition_Info` - DST/standard time transition data
- `TZif_Header` - Parsed TZif file header
- `Timezone_Type` - Individual timezone type record
- `Source_Info` - Timezone data source metadata with ULID

#### Architecture
- 4-layer hexagonal architecture (Domain, Application, Infrastructure, API)
- Public API facade at `TZif.API` with stable interface
- Three-package API pattern (Operations, Desktop, facade)
- Generic I/O plugin pattern for platform portability
- Pure Domain parser (no I/O dependencies, SPARK-friendly)
- Thin Infrastructure wrapper for file system operations
- Library_Standalone mode with explicit Library_Interface
- Result monad error handling via `functional` crate
- Submodule `export-ignore` for clean Alire deployment

#### Platform Support
- Full support: Linux, macOS, FreeBSD, NetBSD, OpenBSD
- Custom I/O adapters for embedded platforms
- Windows: Not supported (see roadmap.md for future plans)

#### Testing
- 327 tests total (200 unit + 116 integration + 11 examples)
- TZif parser error handling tests (29 cases)
- Zone repository tests (14 cases)
- IANA releases metadata lookup tests
- ULID generation and validation tests
- Comprehensive integration tests for all API operations

#### Documentation
- Software Requirements Specification (SRS)
- Software Design Specification (SDS)
- Software Test Guide (STG)
- API guide with usage examples
- Architecture enforcement documentation

#### Build System
- Alire package manager integration
- Comprehensive Makefile with architecture validation
- Build profile support (standard, embedded, concurrent, baremetal, STM32)
- Python-based release automation scripts

### Fixed

#### Architecture Compliance
- Domain layer now has ZERO external crate dependencies (hexagonal architecture)
- Clone `TZif.Domain.Option` from `Functional.Option` for Domain-local use
- arch_guard now detects and reports Domain external dependency violations
- Added 14 Python tests for domain external dependency validation

### Dependencies
- `functional` ^2.2.1 (Result/Option/Try monads)
- `gnatcoll` ^25.0.0 (GNAT Components Collection)

### Known Limitations
- Windows timezone detection not supported (different format, requires registry)
- Cache import/export deferred to post-1.0 (pending user demand)
