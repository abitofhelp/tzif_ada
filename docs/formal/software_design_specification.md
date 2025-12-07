# Software Design Specification (SDS)

**Version:** 1.2.0<br>
**Date:** 2025-12-06<br>
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
│  API.Operations     │  API.Desktop  │  API.Windows  │  API.Embedded  │
│  (SPARK-safe)       │  (POSIX)      │  (Win32)      │  (STM32)       │
├─────────────────────┴───────────────┴───────────────┴────────────────┤
│                    Application Layer                                  │
│     Use Cases (Generic)  │  Outbound Ports (Abstract Signatures)     │
├───────────────────────────────────────────────────────────────────────┤
│                   Infrastructure Layer                                │
│  ┌──────────────────┬───────────────────┬──────────────────────┐     │
│  │ Adapter.Desktop  │  Adapter.Windows  │  Adapter.Embedded    │     │
│  │ (POSIX symlinks) │  (Win32 API)      │  (Config-based)      │     │
│  └──────────────────┴───────────────────┴──────────────────────┘     │
│  Parser, Repository, Cache (Platform-Independent)                     │
├───────────────────────────────────────────────────────────────────────┤
│                      Domain Layer                                     │
│   Entities (Zone) │ Value Objects │ Errors │ Result Monad (Pure)     │
└───────────────────────────────────────────────────────────────────────┘
```

### 2.3 Dependency Flow (Hexagonal Architecture)

```
                    ┌──────────────────┐
                    │   Client Code    │
                    └────────┬─────────┘
                             │
                             ▼
┌────────────────────────────────────────────────────────────────────┐
│                          API Layer                                  │
│  ┌─────────────┐   ┌─────────────┐   ┌─────────────────────────┐  │
│  │ Operations  │   │   Desktop   │   │  Windows / Embedded     │  │
│  │ (Pure)      │   │ (Comp Root) │   │  (Composition Roots)    │  │
│  └─────────────┘   └──────┬──────┘   └────────────┬────────────┘  │
└────────────────────────────┼──────────────────────┼────────────────┘
                             │                      │
         ┌───────────────────┴──────────────────────┘
         │           Wires adapters to ports
         ▼
┌────────────────────────────────────────────────────────────────────┐
│                     Application Layer                               │
│  ┌────────────────────────┐    ┌─────────────────────────────────┐│
│  │     Use Cases          │    │     Outbound Ports              ││
│  │  (Generic packages     │    │  (Abstract function signatures) ││
│  │   with function params)│    │                                 ││
│  └────────────────────────┘    └─────────────────────────────────┘│
└────────────────────────────────────────────────────────────────────┘
         ▲                                     ▲
         │ Implements                          │ Implements
         │                                     │
┌────────────────────────────────────────────────────────────────────┐
│                   Infrastructure Layer                              │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────────────────┐ │
│  │   Desktop    │  │   Windows    │  │      Embedded            │ │
│  │   Adapter    │  │   Adapter    │  │      Adapter             │ │
│  │ (Implements) │  │ (Implements) │  │    (Implements)          │ │
│  └──────────────┘  └──────────────┘  └──────────────────────────┘ │
└────────────────────────────────────────────────────────────────────┘
         │
         ▼
┌────────────────────────────────────────────────────────────────────┐
│                      Domain Layer (Pure)                            │
│        Value Objects │ Entities │ Domain Services │ Errors         │
└────────────────────────────────────────────────────────────────────┘
```

### 2.4 Layer Responsibilities

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

**Pattern**: Platform-specific implementations behind abstract port interfaces
**Purpose**: Support POSIX, Windows, and Embedded platforms with dependency inversion
**Implementation**: `TZif.Application.Port.Outbound.*` + `TZif.Infrastructure.Adapter.*`

#### 4.5.1 Design Principle: Dependency Inversion

The platform abstraction follows the zoneinfo project pattern (proven correct):

1. **Application layer defines abstract ports** using pure function signatures
2. **Infrastructure layer provides adapters** implementing those signatures
3. **Composition roots wire adapters to ports** at build time
4. **No infrastructure types in port signatures** - only Domain types

#### 4.5.2 Outbound Port Definition (Application Layer)

```ada
--  Application.Port.Outbound.System_Timezone
generic
   with function Get_System_Timezone_Id return Zone_Id_Result;
package Application.Port.Outbound.System_Timezone;
```

**Key Characteristics**:
- Pure function signatures only
- Return types are Domain types (Zone_Id_Result)
- No infrastructure package dependencies
- Formally verifiable (SPARK-safe)

#### 4.5.3 Platform Adapters (Infrastructure Layer)

```
src/infrastructure/adapter/
├── desktop/                    -- POSIX systems (Linux, macOS, BSD)
│   └── tzif-infrastructure-adapter-desktop.adb
│       └── Get_System_Timezone_Id → Resolve /etc/localtime symlink
│
├── windows/                    -- Windows 10/Server 2022+
│   └── tzif-infrastructure-adapter-windows.adb
│       └── Get_System_Timezone_Id → Win32 API + CLDR mapping
│
└── embedded/                   -- STM32F769I, ARM Cortex-M
    └── tzif-infrastructure-adapter-embedded.adb
        └── Get_System_Timezone_Id → Config file, env var, or constant
```

#### 4.5.4 Composition Roots (API Layer)

```ada
--  API.Desktop - Wires Desktop adapter to ports
package TZ_Port is new Application.Port.Outbound.System_Timezone
  (Get_System_Timezone_Id => Infrastructure.Adapter.Desktop.Get_System_Timezone_Id);

--  API.Windows - Wires Windows adapter to ports
package TZ_Port is new Application.Port.Outbound.System_Timezone
  (Get_System_Timezone_Id => Infrastructure.Adapter.Windows.Get_System_Timezone_Id);

--  API.Embedded - Wires Embedded adapter to ports
package TZ_Port is new Application.Port.Outbound.System_Timezone
  (Get_System_Timezone_Id => Infrastructure.Adapter.Embedded.Get_System_Timezone_Id);
```

#### 4.5.5 GPR Platform Selection

```ada
--  tzif.gpr
type Platform_Type is ("desktop", "windows", "embedded");
Platform : Platform_Type := external ("TZIF_PLATFORM", "desktop");

case Platform is
   when "desktop" =>
      for Source_Dirs use ("src/**", "src/infrastructure/adapter/desktop");
   when "windows" =>
      for Source_Dirs use ("src/**", "src/infrastructure/adapter/windows");
   when "embedded" =>
      for Source_Dirs use ("src/**", "src/infrastructure/adapter/embedded");
end case;
```

#### 4.5.6 Platform-Specific Implementations

| Platform | System TZ Detection | File Access | RAM FS Support |
|----------|---------------------|-------------|----------------|
| Desktop (POSIX) | `/etc/localtime` symlink | Ada.Directories | N/A |
| Windows | Win32 API + CLDR mapping | Ada.Directories | N/A |
| Embedded | Config file / env var | RAM-based FS | Yes |

**Windows-to-IANA Mapping**:
- Uses CLDR `windowsZones.xml` mapping data
- ~50 common timezone mappings embedded in code
- Returns error for unmapped Windows timezones

**Embedded Timezone Configuration**:
- Environment variable: `TZ=America/New_York`
- Config file: `/etc/timezone` (first line)
- Compile-time constant: `Embedded_Config.Default_Timezone`

#### 4.5.7 Benefits of This Design

| Benefit | Description |
|---------|-------------|
| **Extensibility** | Add new platform without modifying Application layer |
| **Testability** | Mock adapters can implement port signatures for testing |
| **SPARK Verification** | Ports are pure, formally verifiable |
| **Dependency Inversion** | Application depends on abstractions, not implementations |
| **Build-time Selection** | GPR selects adapter; no runtime overhead |

### 4.6 Generic Repository Pattern

**Pattern**: Platform-parameterized repositories via Ada generics
**Purpose**: Achieve Dependency Inversion Principle (DIP) for cross-platform repository implementations
**Implementation**: `Infrastructure.Adapter.File_System.Repository` and `Zone_Repository`

#### 4.6.1 Design Principle

The Generic Repository Pattern extends the Platform Abstraction (§4.5) to repository implementations. Rather than having repositories import platform-specific packages directly, they accept platform operations as generic formal parameters, enabling:

1. **Compile-time platform selection** via GPR Excluded_Source_Files
2. **Type-safe instantiation** guaranteed by Ada's generic system
3. **Testability** via mock Platform_Operations for testing
4. **No runtime overhead** - platform selection is purely compile-time

#### 4.6.2 Platform Operations Interface

```ada
--  Infrastructure.Platform.Platform_Operations
generic
package Platform_Operations is
   function Read_Link (Path : String) return Platform_String_Result;
   function Get_System_Timezone_Id return Zone_Id_Result;
end Platform_Operations;
```

#### 4.6.3 Generic Repository Definition

```ada
--  Infrastructure.Adapter.File_System.Repository (generic)
with TZif.Infrastructure.Platform;

generic
   with package Platform_Ops is new
     TZif.Infrastructure.Platform.Platform_Operations (<>);
package TZif.Infrastructure.Adapter.File_System.Repository is
   function Find_By_Id (...) return Zone_Result;
   function Find_My_Id return Zone_Id_Result;
   --  Implementation uses Platform_Ops.Read_Link
end TZif.Infrastructure.Adapter.File_System.Repository;
```

```ada
--  Infrastructure.Adapter.File_System.Zone_Repository (generic)
with TZif.Infrastructure.Platform;

generic
   with package Platform_Ops is new
     TZif.Infrastructure.Platform.Platform_Operations (<>);
package TZif.Infrastructure.Adapter.File_System.Zone_Repository is
   function Find_By_Id (...) return Zone_Result;
   function Exists (...) return Boolean_Result;
   --  Implementation uses Platform_Ops for platform operations
end TZif.Infrastructure.Adapter.File_System.Zone_Repository;
```

#### 4.6.4 Platform-Specific Instantiations

```ada
--  POSIX instantiation (excluded on Windows via GPR)
with TZif.Infrastructure.Platform.POSIX;
with TZif.Infrastructure.Adapter.File_System.Repository;

package TZif.Infrastructure.Adapter.File_System.POSIX_Repository is new
  TZif.Infrastructure.Adapter.File_System.Repository
    (Platform_Ops => TZif.Infrastructure.Platform.POSIX.Operations);

--  Windows instantiation (excluded on Unix via GPR)
with TZif.Infrastructure.Platform.Windows;
with TZif.Infrastructure.Adapter.File_System.Repository;

package TZif.Infrastructure.Adapter.File_System.Windows_Repository is new
  TZif.Infrastructure.Adapter.File_System.Repository
    (Platform_Ops => TZif.Infrastructure.Platform.Windows.Operations);
```

#### 4.6.5 File Layout

```
src/infrastructure/adapter/file_system/
├── tzif-infrastructure-adapter-file_system-repository.ads      (generic)
├── tzif-infrastructure-adapter-file_system-repository.adb      (generic body)
├── tzif-infrastructure-adapter-file_system-zone_repository.ads (generic)
├── tzif-infrastructure-adapter-file_system-zone_repository.adb (generic body)
├── posix/
│   ├── tzif-infrastructure-adapter-file_system-posix_repository.ads
│   └── tzif-infrastructure-adapter-file_system-posix_zone_repository.ads
└── windows/
    ├── tzif-infrastructure-adapter-file_system-windows_repository.ads
    └── tzif-infrastructure-adapter-file_system-windows_zone_repository.ads
```

#### 4.6.6 GPR Platform Selection

```ada
--  tzif.gpr
type OS_Type is ("unix", "windows");
OS : OS_Type := external ("TZIF_OS", "unix");

case OS is
   when "unix" =>
      for Excluded_Source_Files use
        ("tzif-infrastructure-adapter-file_system-windows_repository.ads",
         "tzif-infrastructure-adapter-file_system-windows_zone_repository.ads",
         "tzif-infrastructure-platform-windows.ads",
         "tzif-infrastructure-platform-windows.adb");
   when "windows" =>
      for Excluded_Source_Files use
        ("tzif-infrastructure-adapter-file_system-posix_repository.ads",
         "tzif-infrastructure-adapter-file_system-posix_zone_repository.ads",
         "tzif-infrastructure-platform-posix.ads",
         "tzif-infrastructure-platform-posix.adb");
end case;
```

#### 4.6.7 Benefits

| Benefit | Description |
|---------|-------------|
| **Dependency Inversion** | Generic Repository depends on abstract Platform_Operations, not concrete POSIX or Windows implementations |
| **Build-time Selection** | GPR Excluded_Source_Files removes wrong-platform instantiations; no runtime overhead |
| **Type Safety** | Ada generics ensure type-safe instantiation; compiler verifies formal parameter compatibility |
| **Testability** | Mock Platform_Operations can be injected for testing without file system access |
| **Single Source of Truth** | Repository logic is written once in the generic; platform-specific code is isolated to instantiation files |

#### 4.6.8 Diagram Reference

See `docs/diagrams/generic_repository_pattern.puml` for a PlantUML visualization of this pattern.

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
| 1.1.0 | 2025-12-06 | Michael Gardner | Platform abstraction refactoring: Added dependency inversion pattern for outbound ports, platform adapter architecture (Desktop/Windows/Embedded), GPR-based platform selection, updated layer diagrams |
| 1.2.0 | 2025-12-06 | Michael Gardner | Generic Repository Pattern: Added §4.6 documenting Repository and Zone_Repository generics with Platform_Ops formal parameters for cross-platform DIP compliance, PlantUML diagram reference |

---

**Document Control**:
- Version: 1.2.0
- Last Updated: 2025-12-06
- Status: Released
- Copyright © 2025 Michael Gardner, A Bit of Help, Inc.
- License: BSD-3-Clause
