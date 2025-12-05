# Software Design Specification (SDS)

**Version:** 1.0.0<br>
**Date:** 2025-12-02<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

---

## 1. Introduction

### 1.1 Purpose

This Software Design Specification (SDS) describes the architectural design and detailed design of the TZif library for parsing and querying IANA timezone information.

### 1.2 Scope

This document covers:
- Architectural patterns and decisions
- Layer organization and dependencies
- Key components and their responsibilities
- Data flow and error handling
- Design patterns employed

---

## 2. Architectural Design

### 2.1 Architecture Style

TZif uses **Hexagonal Architecture** (Ports and Adapters), also known as Clean Architecture.

**Benefits**:
- Clear separation of concerns
- Testable business logic
- Swappable infrastructure
- Compiler-enforced boundaries

### 2.2 Layer Organization

```
┌─────────────────────────────────────────────────────────────┐
│                         API Layer                            │
│              (Public Facade - Stable Interface)              │
├─────────────────────────────────────────────────────────────┤
│  API.Operations     │     API.Desktop     │   (API.Embedded) │
│  (Generic I/O)      │ (File System DI)    │   (Future)       │
├─────────────────────┼─────────────────────┼──────────────────┤
│                    Application Layer                         │
│     Use Cases  │  Ports (Inbound/Outbound)  │  Operations    │
├─────────────────────────────────────────────────────────────┤
│                   Infrastructure Layer                       │
│        Adapters (File System, Parser, Repository)            │
├─────────────────────────────────────────────────────────────┤
│                      Domain Layer                            │
│   Entities (Zone) │ Value Objects │ Errors │ Result Monad    │
└─────────────────────────────────────────────────────────────┘
```

### 2.3 Layer Responsibilities

#### Domain Layer
- **Purpose**: Pure business logic, no dependencies
- **Components**:
  - Value Objects: Zone_Id, Epoch_Seconds, UTC_Offset, Source_Info, etc.
  - Entities: Zone
  - Error types and Result monads

#### Application Layer
- **Purpose**: Orchestrate domain logic, define interfaces
- **Components**:
  - Use Cases: Find_By_Id, Discover_Sources, Get_Transition_At_Epoch, etc.
  - Inbound Ports: Interfaces for external actors
  - Outbound Ports: Interfaces for infrastructure

#### Infrastructure Layer
- **Purpose**: Implement technical concerns, adapt external systems
- **Components**:
  - Adapters: File_System.Repository, TZif_Parser
  - I/O: Desktop file system operations

#### API Layer
- **Purpose**: Public facade with stable interface
- **Components**:
  - TZif.API: Re-exports types and operations
  - API.Operations: Generic operations
  - API.Desktop: Desktop composition root

---

## 3. Detailed Design

### 3.1 Domain Layer Design

#### 3.1.1 Value Objects

**Zone_Id**:
- Immutable timezone identifier
- Validates format (e.g., "America/New_York")
- Maximum 64 characters
- Case-sensitive

**Epoch_Seconds**:
- Signed 64-bit Unix timestamp
- Represents seconds since 1970-01-01 00:00:00 UTC
- Range: -2^63 to 2^63-1

**Source_Info**:
- Timezone source metadata
- Contains: ULID, path, version, zone_count
- Immutable after construction

#### 3.1.2 Entities

**Zone**:
- Timezone entity with identity (Zone_Id)
- Contains transition data and POSIX TZ string
- Immutable after construction

### 3.2 Application Layer Design

#### 3.2.1 Use Cases

Each use case implements a single user operation:

| Use Case | Description |
|----------|-------------|
| Find_By_Id | Retrieve zone by exact ID |
| Find_By_Region | Find zones in region |
| Find_By_Pattern | Pattern matching search |
| Find_By_Regex | Regex-based search |
| Find_My_Id | Detect local timezone |
| Get_Transition_At_Epoch | Lookup transition |
| Get_Version | Get database version |
| Discover_Sources | Scan filesystem |
| Load_Source | Load timezone data |
| Validate_Source | Validate source |
| List_All_Order_By_Id | List all zones |

#### 3.2.2 Port Design

**Inbound Ports**:
- Define use case interfaces
- Input types and result types
- No implementation

**Outbound Ports**:
- Define infrastructure interfaces
- Repository operations
- No implementation

### 3.3 Infrastructure Layer Design

#### 3.3.1 TZif Parser

**Responsibilities**:
- Read TZif binary files
- Parse header, transitions, types
- Handle format versions 2 and 3
- Validate data integrity

**Design**:
- State machine for parsing
- Sequential reading with validation
- Error handling via Result monad

#### 3.3.2 File System Repository

**Responsibilities**:
- Load zones from filesystem
- Discover timezone sources
- Navigate directory structure
- Resolve symbolic links

**Design**:
- Platform abstraction for file operations
- Lazy loading of zone data
- Canonical path handling
- Infinite loop protection via visited path tracking

#### 3.3.3 ULID Infrastructure

**Responsibilities**:
- Generate unique, sortable identifiers for timezone sources
- Provide thread-safe ULID generation
- Support ULID parsing and validation

**Design**:
- Generic RNG plugin architecture
- Thread-safe via protected type
- Monotonic increment for same-millisecond generation
- Crockford Base32 alphabet

---

## 4. Design Patterns

### 4.1 Railway-Oriented Programming

**Pattern**: Result monad for error handling
**Purpose**: Avoid exceptions, explicit error handling
**Implementation**: `Functional.Result` (external crate)

**Usage**:
```ada
function Find_By_Id (Zone_Id) return Zone_Result;
-- Returns: Ok(Zone) or Error(Error_Type)
```

### 4.2 Repository Pattern

**Pattern**: Abstract data access
**Purpose**: Decouple business logic from data storage
**Implementation**: `Application.Port.Outbound.Zone_Repository`

### 4.3 Adapter Pattern

**Pattern**: Adapt external systems to ports
**Purpose**: Implement infrastructure concerns
**Implementation**: `Infrastructure.Adapter.File_System.*`

### 4.4 Generic I/O Plugin Pattern

**Pattern**: Platform abstraction via generics
**Purpose**: Support multiple I/O backends (desktop, embedded)
**Implementation**: `TZif.Application.Operations.All_Operations`

### 4.5 Platform Abstraction

**Pattern**: Platform-specific implementations behind common interface
**Purpose**: Support POSIX and Windows platforms consistently
**Implementation**: `TZif.Infrastructure.Platform.*`

#### 4.5.1 Package Structure

```
TZif.Infrastructure.Platform           -- Generic interface definition
TZif.Infrastructure.Platform.POSIX     -- Linux, macOS, BSD implementation
TZif.Infrastructure.Platform.Windows   -- Windows 10/Server 2022+ implementation
```

#### 4.5.2 POSIX Implementation

| Operation | Implementation |
|-----------|----------------|
| `Read_Symbolic_Link` | POSIX `readlink(2)` syscall |
| Local TZ detection | Read `/etc/localtime` symlink target |

#### 4.5.3 Windows Implementation

| Operation | Implementation |
|-----------|----------------|
| `Read_Symbolic_Link` | Win32 `GetDynamicTimeZoneInformation` API |
| Local TZ detection | Query Windows timezone, map to IANA via CLDR |
| TZif file access | User-provided tzdata directory path |

**Windows-to-IANA Mapping**:
- Uses CLDR `windowsZones.xml` mapping data
- ~50 common timezone mappings embedded in code
- Returns error for unmapped Windows timezones

---

## 5. Data Flow

### 5.1 Zone Lookup Flow

```
User Request
    ↓
TZif.API.Find_By_Id
    ↓
Use Case (Find_By_Id)
    ↓
Repository Port
    ↓
File System Adapter
    ↓
TZif Parser
    ↓
Zone Entity ← Domain Value Objects
    ↓
Result(Zone) ← Error Handling
    ↓
User Response
```

### 5.2 Error Propagation

All errors propagate up via Result monad:
1. Infrastructure error occurs
2. Wrapped in domain error type
3. Returned as Error variant
4. Use case handles or propagates
5. User receives descriptive error

---

## 6. Concurrency Design

### 6.1 Thread Safety

| Layer | Thread Safety |
|-------|---------------|
| Domain Layer | Pure, stateless → thread-safe |
| Application Layer | Stateless → thread-safe |
| Repository | Protected operations → thread-safe |

### 6.2 Source Discovery Implementation

**v1.0.0 Implementation**:
- Sequential recursive directory traversal
- Single-threaded source scanning
- Sufficient performance for typical use (5-15ms for standard sources)

**Infinite Loop Protection**:
- Canonical path deduplication using Ada.Directories.Full_Name
- Visited directory tracking using Ada.Containers.Hashed_Sets
- Depth limit of 15 levels (belt-and-suspenders approach)
- Protection against directory symlink cycles

---

## 7. SPARK Verification Boundaries

### 7.1 Overview

TZif uses SPARK 2014 for formal verification of core logic while excluding I/O operations that cannot be formally verified.

### 7.2 SPARK_Mode by Package

| Package | SPARK_Mode | Reason |
|---------|------------|--------|
| `TZif.Domain.Parser` | **On** | Pure parsing logic, formally verifiable |
| `TZif.Application.Operations` | **On** | Generic operations, no I/O dependencies |
| `TZif.API.Operations` | **On** | Generic facade, formally verifiable |
| `TZif.Infrastructure.ULID_Generic` | **On** (spec) | Structure/contracts verifiable |
| `TZif.Infrastructure.ULID_Generic` | Off (body) | RNG is non-deterministic |
| `TZif.Infrastructure.ULID` | Off | Uses GNAT-internal non-standard RNG |
| `TZif.Infrastructure.IO.Desktop` | Off | File system I/O operations |
| `TZif.API.Desktop` | Off | Composition root with I/O wiring |

### 7.3 Verification Strategy

**Formally Verifiable (SPARK_Mode On)**:
- Domain parsing logic with preconditions
- Generic operations without infrastructure dependencies
- Value object construction and validation

**Excluded from Verification (SPARK_Mode Off)**:
- File system operations (non-deterministic)
- Random number generation (non-deterministic)
- Platform-specific composition roots

### 7.4 Benefits

| Benefit | Description |
|---------|-------------|
| Absence of runtime errors | Proven for SPARK-verified code |
| Contract verification | Preconditions/postconditions checked at proof time |
| No exceptions | SPARK code cannot raise unexpected exceptions |
| Portable core | Verified logic works across platforms |

---

## 8. Performance Design

### 8.1 Caching Strategy

- Parse zones once, cache in memory
- Lazy loading of zone data
- Automatic invalidation on source changes

### 8.2 Optimization Techniques

- Bounded strings (no heap allocation in domain)
- Stack allocation where possible
- Minimal copying of data structures

---

## 9. Security Design

### 9.1 Input Validation

- Validate all zone IDs
- Bounds checking on all inputs
- Path canonicalization to prevent traversal attacks

### 9.2 Error Information

- No sensitive data in error messages
- Safe error types for external display

---

## 10. Build and Deployment

### 10.1 Build System

| Tool | Purpose |
|------|---------|
| Alire | Ada Library Repository |
| GPR | GNAT Project files |
| Make | Build automation |

### 10.2 Project Structure

```
tzif/
├── src/
│   ├── api/           # Public facade
│   ├── application/   # Use cases, ports
│   ├── domain/        # Value objects, entities
│   └── infrastructure/# Adapters
├── test/
│   ├── unit/          # Unit tests
│   └── integration/   # Integration tests
├── examples/          # Working examples
├── docs/              # Documentation
└── scripts/           # Automation
```

---

## 11. Appendices

### 11.1 Package Dependency Graph

```
TZif.API
    ├── TZif.API.Desktop
    │       └── TZif.Infrastructure.IO.Desktop
    │               └── TZif.Application.Operations
    │                       └── TZif.Domain.*
    └── TZif.Domain.Value_Object.*
            └── TZif.Domain.Error
```

### 11.2 Change History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0.0 | 2025-11-29 | Michael Gardner | Initial release |

---

**Document Control**:
- Version: 1.0.0
- Last Updated: 2025-12-02
- Status: Released
- Copyright © 2025 Michael Gardner, A Bit of Help, Inc.
- License: BSD-3-Clause
