# Changelog

**Version:** 1.0.0  
**Date:** December 02, 2025  
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>  
**Status:** Released  

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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
