# Software Requirements Specification (SRS)

**Version:** 1.1.0<br>
**Date:** 2025-12-06<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

---

## 1. Introduction

### 1.1 Purpose

This Software Requirements Specification (SRS) describes the functional and non-functional requirements for TZif, a production-ready Ada 2022 library for parsing and querying IANA timezone information from TZif binary files.

### 1.2 Scope

TZif provides:
- Parsing of TZif binary format (versions 2 and 3)
- Query operations for timezone data by ID, region, pattern, and regex
- Timezone transition lookups for specific epochs
- Source discovery and validation
- High-performance in-memory caching
- Thread-safe operations
- Railway-oriented error handling

### 1.3 Definitions and Acronyms

| Term | Definition |
|------|------------|
| TZif | Timezone Information Format (IANA standard binary format) |
| IANA | Internet Assigned Numbers Authority |
| SRS | Software Requirements Specification |
| API | Application Programming Interface |
| UTC | Coordinated Universal Time |
| RFC | Request for Comments |
| ULID | Universally Unique Lexicographically Sortable Identifier |

### 1.4 References

- IANA Time Zone Database: https://www.iana.org/time-zones
- TZif Format Specification: RFC 9636
- Ada 2022 Language Reference Manual

---

## 2. Overall Description

### 2.1 Product Perspective

TZif is a standalone Ada library implementing hexagonal (ports and adapters) architecture with clean separation between domain logic, application use cases, and infrastructure adapters.

**Architecture Layers**:

| Layer | Purpose |
|-------|---------|
| Domain | Pure business logic, value objects, entities |
| Application | Use cases, ports (interfaces) |
| Infrastructure | Adapters for file system, parsing |
| API | Public facade with stable interface |

### 2.2 Product Features

1. **TZif Parsing**: Parse TZif v2, v3 binary files (RFC 9636)
2. **Timezone Queries**: Find by ID, region, pattern, regex
3. **Transition Lookups**: Get timezone info for specific epoch
4. **Source Management**: Discover and validate timezone sources
5. **In-Memory Caching**: High-performance zone caching with automatic invalidation
6. **Error Handling**: Railway-oriented programming with Result monads

### 2.3 User Classes

| User Class | Description |
|------------|-------------|
| Application Developers | Integrate timezone functionality into applications |
| System Administrators | Configure timezone data sources |
| Library Maintainers | Extend and maintain the codebase |

### 2.4 Operating Environment

| Requirement | Specification |
|-------------|---------------|
| Platforms | POSIX (Linux, macOS, BSD), Windows 10+, Embedded (STM32, ARM) |
| Ada Compiler | GNAT FSF 14.2+ or GNAT Pro 25.0+ |
| Ada Version | Ada 2022 |
| Dependencies | functional ^2.0.0 (Result/Option monads) |

#### 2.4.1 Platform-Specific Notes

**POSIX Systems (Linux, macOS, BSD)**:
- TZif files are typically pre-installed at `/usr/share/zoneinfo/`
- System timezone is determined via `/etc/localtime` symlink

**Windows (10/Server 2022+)**:
- User must provide path to IANA tzdata directory
- Download tzdata from https://www.iana.org/time-zones
- System timezone detected via Win32 API (`GetDynamicTimeZoneInformation`)
- Windows timezone names automatically mapped to IANA zone IDs via CLDR data

**Embedded Systems (STM32F769I, ARM Cortex-M)**:
- TZif files stored in RAM-based filesystem or flash
- System timezone configured via environment variable, config file, or compile-time constant
- Supports Ravenscar profile for real-time constraints
- No dynamic memory allocation in core operations

---

## 3. Functional Requirements

### 3.1 TZif Parsing (FR-01)

**Priority**: High
**Description**: Parse TZif binary files in supported versions.

| ID | Requirement |
|----|-------------|
| FR-01.1 | Parse TZif version 2 (64-bit) |
| FR-01.2 | Parse TZif version 3 (with extensions) |
| FR-01.3 | Validate file format and magic numbers |
| FR-01.4 | Handle malformed files gracefully |
| FR-01.5 | Extract transition times, types, and abbreviations |

### 3.2 Timezone Query Operations (FR-02)

**Priority**: High
**Description**: Provide query operations for timezone data.

| ID | Requirement |
|----|-------------|
| FR-02.1 | Find timezone by exact ID (e.g., "America/New_York") |
| FR-02.2 | Find timezones by region (e.g., "America") |
| FR-02.3 | Find timezones by substring pattern matching |
| FR-02.4 | Find timezones by regular expression |
| FR-02.5 | List all available timezones (ordered by ID) |
| FR-02.6 | Get local system timezone ID |

### 3.3 Transition Lookups (FR-03)

**Priority**: High
**Description**: Retrieve timezone information for specific points in time.

| ID | Requirement |
|----|-------------|
| FR-03.1 | Get transition info for given epoch seconds |
| FR-03.2 | Return UTC offset at specific time |
| FR-03.3 | Return timezone abbreviation (e.g., "PST", "PDT") |
| FR-03.4 | Handle times before/after transition data |

### 3.4 Source Management (FR-04)

**Priority**: Medium
**Description**: Discover and validate timezone data sources from developer-provided paths.

| ID | Requirement |
|----|-------------|
| FR-04.1 | Scan developer-provided filesystem paths for timezone sources |
| FR-04.2 | Validate source directory structure |
| FR-04.3 | Check for required version information |
| FR-04.4 | Count available zone files |
| FR-04.5 | Generate unique IDs for sources (ULID) |
| FR-04.6 | Protect against infinite loops during recursive directory traversal |

**Implementation Notes**:
- v1.0.0 implements sequential recursive directory traversal
- Developer explicitly provides list of paths to scan
- Infinite loop protection: canonical path deduplication + 15-level depth limit

### 3.5 In-Memory Caching (FR-05)

**Priority**: Medium
**Description**: High-performance in-memory zone caching with automatic invalidation.

| ID | Requirement | Status |
|----|-------------|--------|
| FR-05.1 | High-performance in-memory caching | Implemented |
| FR-05.2 | Automatic cache invalidation on source changes | Implemented |
| FR-05.3 | Cache miss triggers automatic zone parsing | Implemented |
| FR-05.4 | Thread-safe cache operations | Implemented |

**Implementation Notes**:
- v1.0.0 provides excellent performance (~20ms cold start)
- Cache persistence (export/import) deferred pending user demand
- See roadmap.md for future cache persistence considerations

### 3.6 Error Handling (FR-06)

**Priority**: High
**Description**: Railway-oriented error handling without exceptions.

| ID | Requirement |
|----|-------------|
| FR-06.1 | Use Result monad for all fallible operations |
| FR-06.2 | Provide descriptive error messages |
| FR-06.3 | Error codes for all failure modes |
| FR-06.4 | No exceptions in library code |

---

## 4. Non-Functional Requirements

### 4.1 Performance (NFR-01)

| ID | Requirement |
|----|-------------|
| NFR-01.1 | Parse TZif file in < 10ms |
| NFR-01.2 | Zone lookup in < 1ms (cached) |
| NFR-01.3 | Transition lookup in < 100μs |
| NFR-01.4 | Cold start (all sources) in < 25ms |

### 4.2 Reliability (NFR-02)

| ID | Requirement |
|----|-------------|
| NFR-02.1 | Handle all malformed inputs gracefully |
| NFR-02.2 | No memory leaks |
| NFR-02.3 | Thread-safe repository operations |

### 4.3 Portability (NFR-03)

| ID | Requirement |
|----|-------------|
| NFR-03.1 | Support POSIX platforms (Linux, macOS, BSD) |
| NFR-03.2 | Support Windows platforms (10/Server 2022+) |
| NFR-03.3 | Support embedded platforms (STM32F769I, ARM Cortex-M) |
| NFR-03.4 | No platform-specific code in domain layer |
| NFR-03.5 | No infrastructure types exposed in application layer ports |
| NFR-03.6 | Platform adapters selectable via GPR configuration |

### 4.6 Platform Abstraction (NFR-06)

| ID | Requirement |
|----|-------------|
| NFR-06.1 | Application layer defines abstract outbound ports using pure function signatures |
| NFR-06.2 | Infrastructure layer provides platform-specific adapters implementing ports |
| NFR-06.3 | Composition roots (API.Desktop, API.Windows, API.Embedded) wire adapters to ports |
| NFR-06.4 | Domain types used in port signatures, not infrastructure types |
| NFR-06.5 | New platforms addable without modifying application layer |
| NFR-06.6 | All platform adapters testable via mock implementations |
| NFR-06.7 | Cross-platform tests runnable on all supported platforms |

### 4.4 Maintainability (NFR-04)

| ID | Requirement |
|----|-------------|
| NFR-04.1 | Hexagonal architecture with clear boundaries |
| NFR-04.2 | Comprehensive documentation (docstrings) |
| NFR-04.3 | > 90% test coverage |
| NFR-04.4 | Zero compiler warnings |

### 4.5 Usability (NFR-05)

| ID | Requirement |
|----|-------------|
| NFR-05.1 | Clear, intuitive API |
| NFR-05.2 | Working examples for all use cases |
| NFR-05.3 | Comprehensive error messages |

---

## 5. System Requirements

### 5.1 Hardware Requirements

| Category | Requirement |
|----------|-------------|
| CPU | Any modern processor |
| RAM | 64 MB minimum |
| Disk | 10 MB minimum |

### 5.2 Software Requirements

| Category | Requirement |
|----------|-------------|
| Operating System | Linux, macOS, BSD |
| Compiler | GNAT FSF 14.2+ or GNAT Pro 25.0+ |
| Build System | Alire 2.0+ |

---

## 6. API Operations

### 6.1 Use Case Summary

The TZif API provides 11 operations:

| Operation | Description |
|-----------|-------------|
| `Find_By_Id` | Retrieve zone by exact ID |
| `Find_By_Region` | Find zones in geographic region |
| `Find_By_Pattern` | Pattern matching search |
| `Find_By_Regex` | Regex-based search |
| `Find_My_Id` | Detect local timezone |
| `Get_Transition_At_Epoch` | Lookup transition at specific time |
| `Get_Version` | Get timezone database version |
| `List_All_Order_By_Id` | List all zones ordered by ID |
| `Discover_Sources` | Scan filesystem for sources |
| `Load_Source` | Load timezone data from source |
| `Validate_Source` | Validate source integrity |

---

## 7. Verification and Validation

### 7.1 Test Coverage

| Test Type | Count |
|-----------|-------|
| Unit tests | 200 |
| Integration tests | 116 |
| Example programs | 11 |

### 7.2 Verification Methods

| Method | Description |
|--------|-------------|
| Code Review | All code reviewed before merge |
| Static Analysis | Zero compiler warnings |
| Dynamic Testing | All tests must pass |
| Coverage Analysis | > 90% line coverage |

---

## 8. Appendices

### 8.1 TZif Format Overview

TZif (Timezone Information Format) is a binary format defined by IANA for storing timezone data (RFC 9636). The format includes:
- Header with version and counts
- Transition times (64-bit timestamps)
- Transition types
- Timezone abbreviations
- Leap second information
- Standard/wall indicators
- UTC/local indicators
- POSIX TZ string for future transitions

### 8.2 Project Statistics

| Metric | Value |
|--------|-------|
| Ada specification files | ~85 |
| Ada implementation files | ~26 |
| Architecture layers | Domain, Application, Infrastructure, API |
| Examples | 11 |
| Unit tests | 200 |

---

**Document Control**:
- Version: 1.1.0
- Last Updated: 2025-12-06
- Status: Draft (Platform Abstraction Refactoring)
- Copyright © 2025 Michael Gardner, A Bit of Help, Inc.
- License: BSD-3-Clause

**Change History**:
| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0.0 | 2025-12-02 | Michael Gardner | Initial release |
| 1.1.0 | 2025-12-06 | Michael Gardner | Added platform abstraction requirements (NFR-06), embedded platform support, Windows platform validation |
