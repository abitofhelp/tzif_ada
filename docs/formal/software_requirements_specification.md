# Software Requirements Specification (SRS)

**Version:** 99.99.99<br>
**Date:** 2025-12-13<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

---

## 1. Introduction

### 1.1 Purpose

This Software Requirements Specification describes the functional and non-functional requirements for TZif, an Ada 2022 library for parsing and querying IANA timezone information from TZif binary files (RFC 9636).

### 1.2 Scope

TZif provides:
- Parsing of TZif binary format (versions 1, 2, and 3)
- Query operations for timezone data by ID, region, pattern, and regex
- Timezone transition lookups for specific Unix epochs
- Source discovery and validation for timezone databases
- Thread-safe operations via bounded containers
- Railway-oriented error handling via Result monad (functional ^4.0.0)

### 1.3 Definitions and Acronyms

| Term | Definition |
|------|------------|
| TZif | Timezone Information Format - IANA standard binary format |
| IANA | Internet Assigned Numbers Authority - maintains timezone database |
| RFC 9636 | TZif format specification document |
| UTC | Coordinated Universal Time |
| DST | Daylight Saving Time |
| ULID | Universally Unique Lexicographically Sortable Identifier |
| Result Monad | Functional pattern for error handling without exceptions |
| SPARK | Ada subset for formal verification |

### 1.4 References

- IANA Time Zone Database: https://www.iana.org/time-zones
- TZif Format Specification: RFC 9636
- Ada 2022 Language Reference Manual (ISO/IEC 8652:2023)
- SPARK 2014 Reference Manual
- Hexagonal Architecture (Alistair Cockburn)

---

## 2. Overall Description

### 2.1 Product Perspective

TZif is a standalone Ada library implementing hexagonal (ports and adapters) architecture with clean separation between domain logic, application use cases, and infrastructure adapters.

```
+-----------------------------------------------------------------+
|                          API Layer                               |
|  TZif.API (facade) + TZif.API.Desktop/Windows/Embedded (roots)  |
+----------------------------------+------------------------------+
                                   |
+----------------------------------v------------------------------+
|                      Application Layer                           |
|  Operations (11 use cases) + Inbound/Outbound Ports             |
+----------------------------------+------------------------------+
                                   |
+----------------------------------v------------------------------+
|                    Infrastructure Layer                          |
|  I/O Adapters (Desktop, Windows, Embedded) + Platform Ops       |
+----------------------------------+------------------------------+
                                   |
+----------------------------------v------------------------------+
|                       Domain Layer                               |
|  Entity (Zone) + Value Objects + Parser + Error Types           |
+-----------------------------------------------------------------+
```

**Architecture Layers:**

| Layer | Purpose | SPARK_Mode |
|-------|---------|------------|
| Domain | Pure business logic, value objects, parser, error types | On |
| Application | Use cases, operations, inbound/outbound ports | On |
| Infrastructure | Adapters for file system, I/O, platform operations | Off |
| API | Public facade with stable interface, composition roots | Off |

### 2.2 Product Features

1. **TZif Parsing**: Parse TZif v1, v2, v3 binary files per RFC 9636
2. **Timezone Queries**: Find zones by ID, region, pattern, or regex
3. **Transition Lookups**: Get UTC offset and DST status at any epoch
4. **Source Management**: Discover, load, and validate timezone sources
5. **Error Handling**: Railway-oriented programming with Result monads
6. **Platform Abstraction**: Generic I/O plugin pattern for portability

### 2.3 User Classes

| User Class | Description |
|------------|-------------|
| Application Developers | Integrate timezone functionality into Ada applications |
| Embedded Developers | Require heap-free, SPARK-compatible timezone operations |
| System Integrators | Configure timezone data sources across platforms |

### 2.4 Operating Environment

| Requirement | Specification |
|-------------|---------------|
| Platforms | POSIX (Linux, macOS, BSD), Windows 11, Embedded |
| Ada Compiler | GNAT FSF 13+ or GNAT Pro |
| Ada Version | Ada 2022 |
| Dependencies | functional ^4.0.0, gnatcoll ^25.0.0 |

### 2.5 Constraints

| Constraint | Rationale |
|------------|-----------|
| Ada 2022 | Required for modern language features (aggregates, aspects) |
| GNAT 13+ | Required compiler version for Ada 2022 support |
| Bounded Containers | Embedded system compatibility, SPARK verification |
| SPARK Subset | Formal verification of domain/application layers |

---

## 3. Functional Requirements

### 3.1 TZif Parsing (FR-01)

**Priority**: High
**Description**: Parse TZif binary files in all supported versions.

| ID | Requirement |
|----|-------------|
| FR-01.1 | Parse TZif version 1 (32-bit timestamps, legacy) |
| FR-01.2 | Parse TZif version 2 (64-bit timestamps) |
| FR-01.3 | Parse TZif version 3 (with POSIX TZ string extensions) |
| FR-01.4 | Validate file format magic number ("TZif") |
| FR-01.5 | Handle malformed files gracefully via Result monad |
| FR-01.6 | Extract transition times, types, and abbreviations |
| FR-01.7 | Extract leap second information |
| FR-01.8 | Extract standard/wall and UTC/local indicators |

**Acceptance Criteria (FR-01):**
- Parser returns Ok(TZif_Data) on valid files
- Parser returns Error(Parse_Error, message) on invalid files
- No exceptions raised for malformed input

### 3.2 Timezone Query Operations (FR-02)

**Priority**: High
**Description**: Provide 11 query operations for timezone data.

| ID | Requirement | Operation |
|----|-------------|-----------|
| FR-02.1 | Find timezone by exact IANA ID | Find_By_Id |
| FR-02.2 | Find timezones by geographic region prefix | Find_By_Region |
| FR-02.3 | Find timezones by substring pattern match | Find_By_Pattern |
| FR-02.4 | Find timezones by regular expression | Find_By_Regex |
| FR-02.5 | List all available timezones ordered by ID | List_All_Order_By_Id |
| FR-02.6 | Detect local system timezone | Find_My_Id |
| FR-02.7 | Get transition info at specific epoch | Get_Transition_At_Epoch |
| FR-02.8 | Get database version string | Get_Version |
| FR-02.9 | Discover timezone sources in filesystem | Discover_Sources |
| FR-02.10 | Load timezone data from source path | Load_Source |
| FR-02.11 | Validate source directory structure | Validate_Source |

**Acceptance Criteria (FR-02):**
- All operations return Result types (Ok or Error)
- Find operations support callback-based streaming
- List operations support ascending/descending order

### 3.3 Domain Layer (FR-03)

**Priority**: High
**Description**: Provide core domain entities and value objects.

| ID | Requirement |
|----|-------------|
| FR-03.1 | Zone_Id value object with bounded string (1-256 characters) |
| FR-03.2 | Zone_Id validates IANA format on construction |
| FR-03.3 | Zone entity aggregates Zone_Id with TZif_Data |
| FR-03.4 | Source_Info value object with path, version, zone count |
| FR-03.5 | Transition_Info value object with offset, DST, abbreviation |
| FR-03.6 | Epoch_Seconds type for Unix timestamp representation |
| FR-03.7 | Bounded_Vector generic for SPARK-compatible collections |

### 3.4 Error Handling (FR-04)

**Priority**: High
**Description**: Railway-oriented error handling without exceptions.

| ID | Requirement |
|----|-------------|
| FR-04.1 | Use Result monad for all fallible operations |
| FR-04.2 | Provide 6 error categories via Error_Kind enumeration |
| FR-04.3 | Provide descriptive error messages (bounded 512 chars) |
| FR-04.4 | No exceptions in library code (SPARK compatibility) |
| FR-04.5 | Use Functional.Try.Map_To_Result at infrastructure boundaries |

**Error Kinds:**

| Kind | Description |
|------|-------------|
| Validation_Error | Domain validation failures (invalid input) |
| Parse_Error | Malformed data (corrupted TZif, bad magic) |
| Not_Found_Error | Resource not found (file, zone, type) |
| IO_Error | I/O operations (read/write, permissions) |
| Resource_Error | Resource exhaustion (buffer overflow) |
| Internal_Error | Precondition violations (shouldn't happen) |

---

## 4. Non-Functional Requirements

### 4.1 Performance (NFR-01)

| ID | Requirement |
|----|-------------|
| NFR-01.1 | Parse TZif file in < 10ms |
| NFR-01.2 | Zone lookup by ID in < 1ms |
| NFR-01.3 | Transition lookup in < 100us |
| NFR-01.4 | Support caching for repeated lookups |

### 4.2 Reliability (NFR-02)

| ID | Requirement |
|----|-------------|
| NFR-02.1 | Handle all malformed inputs gracefully (no crashes) |
| NFR-02.2 | No memory leaks (bounded containers only) |
| NFR-02.3 | Thread-safe cache operations (protected types) |
| NFR-02.4 | All errors returned via Result monad |

### 4.3 Portability (NFR-03)

| ID | Requirement |
|----|-------------|
| NFR-03.1 | Support POSIX platforms (Linux, macOS, BSD) |
| NFR-03.2 | Support Windows platforms |
| NFR-03.3 | Support embedded platforms via custom adapters |
| NFR-03.4 | No platform-specific code in domain layer |
| NFR-03.5 | No infrastructure types exposed in application ports |
| NFR-03.6 | Platform adapters selectable via composition roots |

### 4.4 Maintainability (NFR-04)

| ID | Requirement |
|----|-------------|
| NFR-04.1 | Hexagonal architecture with clear layer boundaries |
| NFR-04.2 | Comprehensive docstrings on all packages |
| NFR-04.3 | > 90% test coverage |
| NFR-04.4 | Zero compiler warnings with style checks |

### 4.5 Usability (NFR-05)

| ID | Requirement |
|----|-------------|
| NFR-05.1 | Clear, intuitive API via TZif.API facade |
| NFR-05.2 | Working examples for all 11 operations |
| NFR-05.3 | Comprehensive error messages with context |

### 4.6 Platform Abstraction (NFR-06)

| ID | Requirement |
|----|-------------|
| NFR-06.1 | Application layer defines abstract outbound ports |
| NFR-06.2 | Infrastructure layer provides platform-specific adapters |
| NFR-06.3 | Composition roots wire adapters: API.Desktop, API.Windows, API.Embedded |
| NFR-06.4 | Domain types used in port signatures, not infrastructure types |
| NFR-06.5 | New platforms addable without modifying application layer |
| NFR-06.6 | All platform adapters testable via mock implementations |

### 4.7 SPARK Formal Verification (NFR-07)

| ID | Requirement |
|----|-------------|
| NFR-07.1 | Domain layer shall pass SPARK legality checking |
| NFR-07.2 | Application layer shall pass SPARK legality checking |
| NFR-07.3 | No runtime errors provable (overflow, range, division) |
| NFR-07.4 | All variables properly initialized before use |
| NFR-07.5 | Pre/postconditions proven correct |
| NFR-07.6 | SPARK legality via `make spark-check` |
| NFR-07.7 | SPARK proof via `make spark-prove` |
| NFR-07.8 | Infrastructure/API layers use `SPARK_Mode => Off` |

**Verification Scope:**

| Layer | SPARK_Mode | Rationale |
|-------|-----------|-----------|
| Domain | On | Pure business logic, formally provable |
| Application | On | Operations and ports, provable contracts |
| Infrastructure | Off | I/O operations, platform-specific |
| API | Off | Facade wiring, composition roots |

### 4.8 Testability (NFR-08)

| ID | Requirement |
|----|-------------|
| NFR-08.1 | Unit tests for all domain packages |
| NFR-08.2 | Integration tests for cross-layer interactions |
| NFR-08.3 | Example programs demonstrating all API operations |
| NFR-08.4 | Test runners with pass/fail reporting |
| NFR-08.5 | CI/CD integration (GitHub Actions) |

---

## 5. System Requirements

### 5.1 Hardware Requirements

| Category | Requirement |
|----------|-------------|
| CPU | Any modern processor |
| RAM | 64 MB minimum |
| Disk | 10 MB minimum (plus timezone database) |

### 5.2 Software Requirements

| Category | Requirement |
|----------|-------------|
| Operating System | Linux, macOS, BSD, Windows 11 |
| Compiler | GNAT FSF 13+ or GNAT Pro |
| Build System | Alire 2.0+ |
| Timezone Data | IANA tzdata (system or user-provided) |

---

## 6. API Operations

### 6.1 Operation Summary

The TZif API provides 11 operations through `TZif.API`:

| Operation | Description | Returns |
|-----------|-------------|---------|
| `Find_By_Id` | Lookup zone by exact IANA identifier | Zone_Result |
| `Find_By_Pattern` | Search zones by substring match | Pattern_Result |
| `Find_By_Region` | Search zones by geographic region | Region_Result |
| `Find_By_Regex` | Search zones using regular expressions | Regex_Result |
| `Find_My_Id` | Detect the system's local timezone | My_Zone_Result |
| `Get_Transition_At_Epoch` | Query UTC offset and DST at any time | Transition_Result |
| `Get_Version` | Retrieve IANA database version | Version_Result |
| `List_All_Zones` | Enumerate all available timezones | Zone_List_Result |
| `Discover_Sources` | Find timezone data directories | Discovery_Result |
| `Load_Source` | Load a specific timezone data source | Load_Source_Result |
| `Validate_Source` | Validate timezone data integrity | Validation_Result |

---

## 7. Verification and Validation

### 7.1 Verification Methods

| Method | Description |
|--------|-------------|
| Code Review | All code reviewed before merge |
| Static Analysis | Zero compiler warnings, SPARK legality |
| Dynamic Testing | All tests must pass |
| Coverage Analysis | > 90% line coverage |
| Formal Proof | SPARK prove for domain/application |

### 7.2 Traceability Matrix

| Requirement | Implementation | Test |
|-------------|----------------|------|
| FR-01.1-8 | TZif.Infrastructure.TZif_Parser | test_tzif_parser.adb |
| FR-02.1 | TZif.API.Find_By_Id | test_api.adb |
| FR-02.6 | TZif.API.Find_My_Id | test_api.adb |
| FR-02.7 | TZif.API.Get_Transition_At_Epoch | test_api.adb |
| FR-03.1-2 | TZif.Domain.Value_Object.Zone_Id | test_zone_id.adb |
| FR-03.7 | TZif.Domain.Types.Bounded_Vector | test_bounded_vector.adb |
| FR-04.1-5 | TZif.Domain.Error + Functional.Try | test_error_result.adb |

---

## 8. Appendices

### 8.1 TZif Format Overview

TZif (Timezone Information Format) is a binary format defined by IANA for storing timezone data (RFC 9636). Key components:

- **Header**: Magic number, version, counts
- **Transition times**: 32-bit (v1) or 64-bit (v2/v3) Unix timestamps
- **Transition types**: Index into type array for each transition
- **Type definitions**: UTC offset, DST flag, abbreviation index
- **Abbreviations**: Null-terminated timezone name strings
- **Leap seconds**: Leap second correction data
- **Indicators**: Standard/wall and UTC/local flags
- **Footer** (v2/v3): POSIX TZ string for future transitions

### 8.2 Project Statistics

| Metric | Value |
|--------|-------|
| Ada specification files | 92 |
| Ada implementation files | 31 |
| Architecture layers | 4 (Domain, Application, Infrastructure, API) |
| Unit tests | 424 |
| Integration tests | 134 |
| Example programs | 11 |
| **Total tests** | **569** |

---

**Document Control:**
- Version: 99.99.99
- Last Updated: 2025-12-13
- Status: Released

**Change History:**

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 3.0.0 | 2025-12-13 | Michael Gardner | Complete regeneration for v3.0.0; updated functional to ^4.0.0; updated test counts (424 unit + 134 integration + 11 examples = 569 total); added 6th error kind (Resource_Error) |
| 1.0.0 | 2025-12-07 | Michael Gardner | Initial release |
