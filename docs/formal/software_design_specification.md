# Software Design Specification (SDS)

**Version:** 3.0.3<br>
**Date:** 2025-12-16<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

---

## 1. Introduction

### 1.1 Purpose

This Software Design Specification describes the architecture and design of TZif, an Ada 2022 library for parsing and querying IANA timezone information from TZif binary files (RFC 9636).

### 1.2 Scope

This document covers:
- 4-layer hexagonal architecture (Domain, Application, Infrastructure, API)
- Package structure and dependencies
- Key type definitions
- Design patterns (Result monad, static DI, composition roots)
- Error handling strategy
- Build configuration

### 1.3 References

- Software Requirements Specification (SRS)
- Ada 2022 Language Reference Manual
- Hexagonal Architecture (Alistair Cockburn)
- Domain-Driven Design (Eric Evans)
- Clean Architecture (Robert C. Martin)

---

## 2. Architectural Overview

### 2.1 Layer Architecture

```
+-----------------------------------------------------------------+
|                          API Layer                               |
|  TZif.API (facade) + TZif.API.Desktop/Windows/Embedded (roots)  |
+----------------------------------+------------------------------+
                                   |
                                   | depends on
                                   v
+----------------------------------+------------------------------+
|                      Application Layer                           |
|  Operations (generic) + Inbound Ports + Outbound Ports          |
+----------------------------------+------------------------------+
                                   |
                                   | depends on (port signatures)
                                   v
+----------------------------------+------------------------------+
|                    Infrastructure Layer                          |
|  I/O Adapters + Platform Ops + Cache + ULID                     |
|  (implements outbound ports via formal packages)                 |
+----------------------------------+------------------------------+
                                   |
                                   | depends on
                                   v
+----------------------------------+------------------------------+
|                       Domain Layer                               |
|  Entity (Zone) + Value Objects + Parser + Error + Types         |
+-----------------------------------------------------------------+
```

### 2.2 Dependency Rules

| Layer | Can Depend On | Cannot Depend On |
|-------|---------------|------------------|
| Domain | Standard Ada libraries only | Application, Infrastructure, API |
| Application | Domain | Infrastructure, API |
| Infrastructure | Domain, Application (port signatures) | API |
| API | All layers | (top layer) |

### 2.3 Hexagonal Pattern

**Ports (Application Layer):**
- **Inbound Ports**: Define what the application can do (11 operations)
- **Outbound Ports**: Define what the application needs (Zone_Repository, I/O)

**Adapters (Infrastructure Layer):**
- **Desktop Adapter**: POSIX file system operations
- **Windows Adapter**: Windows file system + Win32 timezone API
- **Embedded Adapter**: Stub for custom implementations

**Composition Roots (API Layer):**
- `TZif.API.Desktop`: Wires Desktop I/O adapter
- `TZif.API.Windows`: Wires Windows I/O adapter
- `TZif.API.Embedded`: Wires Embedded I/O adapter

---

## 3. Package Structure

### 3.1 Directory Layout

```
src/
├── api/                           # Public API (composition roots)
│   ├── tzif-api.ads/adb          # Main facade
│   ├── operations/               # Generic API operations
│   │   └── tzif-api-operations.ads
│   ├── desktop/                  # Desktop composition root
│   │   └── tzif-api-desktop.ads
│   ├── windows/                  # Windows composition root
│   │   └── tzif-api-windows.ads
│   └── embedded/                 # Embedded composition root
│       └── tzif-api-embedded.ads
│
├── application/                   # Use cases and ports
│   ├── tzif-application.ads
│   ├── operations/               # Generic operations
│   │   └── tzif-application-operations.ads
│   ├── command/                  # Command DTOs
│   │   └── tzif-application-command.ads
│   ├── model/                    # Application models
│   │   ├── tzif-application-model.ads
│   │   └── tzif-application-model-unit.ads
│   └── port/
│       ├── inbound/              # 11 inbound ports
│       │   ├── tzif-application-port-inbound-find_by_id.ads
│       │   ├── tzif-application-port-inbound-find_my_id.ads
│       │   ├── tzif-application-port-inbound-get_transition_at_epoch.ads
│       │   └── ... (11 total)
│       └── outbound/             # Outbound port signatures
│           ├── tzif-application-port-outbound-zone_repository.ads
│           └── tzif-application-port-outbound-writer.ads
│
├── infrastructure/                # Platform adapters
│   ├── tzif-infrastructure.ads
│   ├── io/                       # I/O adapters (implements outbound ports)
│   │   ├── desktop/
│   │   │   └── tzif-infrastructure-io-desktop.ads/adb
│   │   ├── windows/
│   │   │   └── tzif-infrastructure-io-windows.ads/adb
│   │   └── embedded/
│   │       └── tzif-infrastructure-io-embedded.ads/adb
│   ├── platform/                 # Platform-specific operations
│   │   ├── posix/
│   │   │   └── tzif-infrastructure-platform-posix.ads/adb
│   │   └── windows/
│   │       └── tzif-infrastructure-platform-windows.ads/adb
│   ├── cache/                    # Zone and source caching
│   │   ├── tzif-infrastructure-cache-zone_cache.ads
│   │   └── tzif-infrastructure-cache-source_cache.ads
│   └── ulid/                     # ULID generation
│       └── tzif-infrastructure-ulid.ads
│
└── domain/                        # Pure business logic
    ├── tzif-domain.ads
    ├── tzif-domain-tzif_data.ads  # TZif binary data structure
    ├── entity/
    │   └── tzif-domain-entity-zone.ads
    ├── value_object/
    │   ├── tzif-domain-value_object-zone_id.ads
    │   ├── tzif-domain-value_object-epoch_seconds.ads
    │   ├── tzif-domain-value_object-source_info.ads
    │   ├── tzif-domain-value_object-transition_info.ads
    │   └── ... (10+ value objects)
    ├── types/
    │   ├── tzif-domain-types-bounded_vector.ads
    │   └── tzif-domain-types-option.ads
    ├── error/
    │   ├── tzif-domain-error.ads
    │   └── tzif-domain-error-result.ads
    └── parser/
        └── tzif-domain-parser.ads
```

### 3.2 Package Descriptions

#### Domain Layer (SPARK_Mode => On)

| Package | Purpose |
|---------|---------|
| `TZif.Domain` | Root package for domain layer |
| `TZif.Domain.Error` | Error_Kind enumeration, Error_Type record |
| `TZif.Domain.Error.Result` | Generic Result monad instantiation |
| `TZif.Domain.Entity.Zone` | Zone aggregate (Zone_Id + TZif_Data) |
| `TZif.Domain.Value_Object.Zone_Id` | Timezone identifier (bounded 256 chars) |
| `TZif.Domain.Value_Object.Epoch_Seconds` | Unix timestamp type |
| `TZif.Domain.Value_Object.Source_Info` | Timezone source metadata |
| `TZif.Domain.Value_Object.Transition_Info` | Transition result record |
| `TZif.Domain.Types.Bounded_Vector` | Generic bounded collection |
| `TZif.Domain.Types.Option` | Optional value wrapper |
| `TZif.Domain.Parser` | TZif binary format parser |
| `TZif.Domain.TZif_Data` | Parsed timezone data structure |

#### Application Layer (SPARK_Mode => On)

| Package | Purpose |
|---------|---------|
| `TZif.Application` | Root package for application layer |
| `TZif.Application.Operations` | Generic operations (parameterized by I/O) |
| `TZif.Application.Port.Inbound.*` | 11 inbound port specifications |
| `TZif.Application.Port.Outbound.*` | Outbound port signatures |
| `TZif.Application.Command` | Command DTOs |
| `TZif.Application.Model` | Application-specific models |

#### Infrastructure Layer (SPARK_Mode => Off)

| Package | Purpose |
|---------|---------|
| `TZif.Infrastructure` | Root package for infrastructure |
| `TZif.Infrastructure.IO.Desktop` | Desktop I/O adapter (POSIX) |
| `TZif.Infrastructure.IO.Windows` | Windows I/O adapter |
| `TZif.Infrastructure.IO.Embedded` | Embedded I/O stub |
| `TZif.Infrastructure.Platform.Posix` | POSIX platform operations |
| `TZif.Infrastructure.Platform.Windows` | Windows platform operations |
| `TZif.Infrastructure.Cache.*` | Zone and source caching |
| `TZif.Infrastructure.ULID` | ULID generation |
| `TZif.Infrastructure.TZif_Parser` | TZif binary parsing wrapper |

#### API Layer (SPARK_Mode => Off)

| Package | Purpose |
|---------|---------|
| `TZif.API` | Public facade (re-exports domain types) |
| `TZif.API.Operations` | Generic facade operations |
| `TZif.API.Desktop` | Desktop composition root |
| `TZif.API.Windows` | Windows composition root |
| `TZif.API.Embedded` | Embedded composition root |

---

## 4. Type Definitions

### 4.1 Domain Types

#### Error Types

```ada
type Error_Kind is
  (Validation_Error,   -- Domain validation failures
   Parse_Error,        -- Malformed data
   Not_Found_Error,    -- Resource not found
   IO_Error,           -- I/O operations
   Resource_Error,     -- Resource exhaustion
   Internal_Error);    -- Precondition violations

type Error_Type is record
   Kind    : Error_Kind;
   Message : Error_Strings.Bounded_String;  -- Max 512 chars
end record;
```

#### Value Objects

```ada
--  Zone ID (bounded 256 characters)
type Zone_Id_Type is private;
function Make_Zone_Id (Id : String) return Zone_Id_Result;
function To_String (Id : Zone_Id_Type) return String;

--  Epoch seconds (signed 64-bit for historical dates)
type Epoch_Seconds_Type is range -(2**63) .. (2**63 - 1);

--  Transition info result
type Transition_Info_Type is record
   Offset       : UTC_Offset_Type;
   Is_DST       : Boolean;
   Abbreviation : Abbreviation_String;
end record;

--  Source metadata
type Source_Info_Type is record
   Id         : ULID_Type;
   Path       : Path_String;
   Version    : Version_String;
   Zone_Count : Natural;
end record;
```

### 4.2 Application Types

```ada
--  Result monad (from functional ^4.0.0)
generic
   type T is private;
   type E is private;
package Functional.Result is
   type Result is record
      Is_Ok       : Boolean;
      Ok_Value    : T;
      Error_Value : E;
   end record;

   function Ok (Value : T) return Result;
   function Error (Kind : Error_Kind; Msg : String) return Result;
   function Is_Ok (R : Result) return Boolean;
   function Value (R : Result) return T;
   function Error_Info (R : Result) return E;
end Functional.Result;
```

### 4.3 API Types

The API layer re-exports domain types for consumer convenience:

```ada
subtype Zone_Id_Type is TZif.Domain.Value_Object.Zone_Id.Zone_Id_Type;
subtype Error_Type is TZif.Domain.Error.Error_Type;
subtype Error_Kind is TZif.Domain.Error.Error_Kind;
subtype Zone_Result is Find_By_Id_Port.Find_By_Id_Result_Type;
```

---

## 5. Design Patterns

### 5.1 Static Dependency Injection

Ada's generic formal packages enable compile-time DI without runtime overhead:

```ada
--  Application layer: generic operations
generic
   type Byte_Array is array (Positive range <>) of Unsigned_8;
   with package Read_File_Result is new Generic_Result (<>);
   with procedure Read_File
     (Id     :     Zone_Id_Input_Type;
      Bytes  : out Byte_Array;
      Length : out Natural;
      Result : out Read_File_Result.Result);
   -- ... more I/O formal parameters
package TZif.Application.Operations.All_Operations is
   procedure Find_By_Id
     (Id : Zone_Id_Input_Type; Result : out Find_By_Id_Result_Type);
end All_Operations;

--  API layer: composition root instantiates with concrete adapter
package Desktop_Ops is new TZif.Application.Operations.All_Operations
  (Byte_Array       => TZif.Infrastructure.IO.Desktop.Byte_Array,
   Read_File_Result => TZif.Infrastructure.IO.Desktop.Read_File_Result,
   Read_File        => TZif.Infrastructure.IO.Desktop.Read_File);
```

**Design Rationale:**
- Decouples business logic from I/O implementation
- Enables platform-specific adapters (Desktop, Windows, Embedded)
- Supports testing with mock I/O
- SPARK-friendly (no access types or tagged types)

### 5.2 Three-Package API Pattern

```
TZif.API            -- Public facade (stable types, functions)
TZif.API.Operations -- Generic operations (parameterized)
TZif.API.Desktop    -- Composition root (wires I/O adapter)
```

Users import `TZif.API` for a simple, stable interface. The composition roots wire platform-specific adapters.

### 5.3 Result Monad Error Handling

All fallible operations return `Result` types:

```ada
function Find_By_Id (Id : Zone_Id_Type) return Zone_Result;
--  Returns Ok(Zone) or Error(Not_Found_Error, "Zone not found")

--  Usage:
Result := Find_By_Id (My_Zone_Id);
if Is_Ok (Result) then
   Process (Value (Result));
else
   Handle_Error (Error_Info (Result));
end if;
```

### 5.4 Smart Constructor Pattern

Value objects use smart constructors that validate on creation:

```ada
function Make_Zone_Id (Id : String) return Zone_Id_Result;
--  Returns Ok(Zone_Id) if valid IANA format
--  Returns Error(Validation_Error, message) if invalid

--  Validation rules:
--  - Non-empty (1-256 characters)
--  - Contains '/' separator (or starts with "Etc/")
--  - Valid characters (alphanumeric, '/', '-', '_')
```

### 5.5 Functional.Try at Boundaries

Infrastructure uses `Functional.Try.Map_To_Result` for exception handling:

```ada
--  Declare raw action that may raise
function Raw_Read_File (Ctx : Read_File_Context) return Read_File_Result;

--  Create error constructor
function Make_Read_Error
  (Kind : Error_Kind; Message : String) return Read_File_Result;

--  Wrap with Map_To_Result_With_Param
package Try_Read is new Functional.Try.Map_To_Result_With_Param
  (Error_Kind_Type    => Error_Kind,
   Param_Type         => Read_File_Context,
   Result_Type        => Read_File_Result,
   Make_Error         => Make_Read_Error,
   Default_Error_Kind => IO_Error,
   Action             => Raw_Read_File);

--  Exception mappings (specific exceptions to error kinds)
Read_Mappings : constant Try_Read.Mapping_Array :=
  [(SIO.Name_Error'Identity, Not_Found_Error),
   (SIO.Use_Error'Identity,  IO_Error),
   (SIO.End_Error'Identity,  Parse_Error)];

--  Public API catches all exceptions
procedure Read_File (Path : String; Result : out Read_File_Result) is
begin
   Result := Try_Read.Run (Context, Read_Mappings);
end Read_File;
```

---

## 6. Error Handling Strategy

### 6.1 Railway-Oriented Programming

All fallible operations return `Result[T, Error_Type]`:

```
                    +-------+
                    | Input |
                    +---+---+
                        |
              +-------- v --------+
              |   Operation 1    |
              +-+--------------+-+
                |              |
             Ok |           Error
                v              v
         +------+------+   +---+---+
         | Operation 2 |   | Error |
         +------+------+   +-------+
                |
             Ok |
                v
         +------+------+
         |   Result    |
         +-------------+
```

### 6.2 No Exceptions Policy

- Domain and Application layers never raise exceptions
- Infrastructure wraps all I/O in `Functional.Try.Map_To_Result`
- All errors propagate via Result monad

### 6.3 Error Propagation

```
User Code
    |
    v
TZif.API.Find_By_Id  -->  returns Zone_Result
    |
    v
Application.Operations.Find_By_Id
    |
    v
Infrastructure.IO.Read_File  -->  catches exceptions, returns Read_File_Result
    |
    v
Ada.Sequential_IO  -->  may raise Name_Error, Use_Error
```

### 6.4 Error Kind Categories

| Category | Recoverable | Typical Cause |
|----------|-------------|---------------|
| Validation_Error | Yes | Invalid user input |
| Parse_Error | Sometimes | Corrupted TZif file |
| Not_Found_Error | Yes | Missing zone/file |
| IO_Error | Sometimes | File permissions, disk full |
| Resource_Error | No | Buffer overflow |
| Internal_Error | No | Bug in library |

---

## 7. Build Configuration

### 7.1 GPR Projects

| Project | Purpose |
|---------|---------|
| `tzif.gpr` | Main library project |
| `tzif_config.gpr` | Build configuration |
| `examples/examples.gpr` | Example programs |
| `test/unit/unit_tests.gpr` | Unit test suite |
| `test/integration/integration_tests.gpr` | Integration tests |

### 7.2 Build Profiles

| Profile | Optimization | Assertions | Debug |
|---------|--------------|------------|-------|
| `development` | None | Enabled | Full symbols |
| `release` | O2 | Disabled | Minimal |
| `validation` | O1 | Enabled | Full symbols |

### 7.3 Platform Selection

Platform selection via environment variable `TZIF_OS`:

```bash
export TZIF_OS=desktop   # Linux, macOS, BSD (default)
export TZIF_OS=windows   # Windows 11
export TZIF_OS=embedded  # Embedded targets
```

### 7.4 Build Outputs

| Artifact | Description |
|----------|-------------|
| `lib/libtzif.a` | Static library |
| `bin/examples/*` | Example programs |
| `test/bin/*_runner` | Test executables |

### 7.5 Dependencies

| Package | Version | Layer |
|---------|---------|-------|
| functional | ^4.0.0 | Infrastructure |
| gnatcoll | ^25.0.0 | Infrastructure |

**Note:** Domain layer has zero external dependencies.

---

## 8. Design Decisions

### 8.1 Bounded Containers

**Decision**: Use bounded containers throughout domain layer.

**Rationale**:
- SPARK verification requires bounded types
- Embedded systems cannot use heap allocation
- Predictable memory usage

**Trade-off**: Fixed limits on strings and collections.

### 8.2 Static Dispatch

**Decision**: Use generics for polymorphism, not tagged types.

**Rationale**:
- Zero runtime overhead
- Full SPARK compatibility
- Simpler verification

**Trade-off**: No runtime plugin swapping.

### 8.3 Result Monad over Exceptions

**Decision**: All errors return Result types, no exceptions in library.

**Rationale**:
- SPARK compatibility (exceptions not verifiable)
- Railway-oriented programming enables composition
- Explicit error handling at call sites

**Trade-off**: More verbose call sites.

### 8.4 Functional.Try at Boundaries

**Decision**: Use `Functional.Try.Map_To_Result` to wrap all I/O.

**Rationale**:
- Single exception boundary pattern
- Declarative exception mapping
- Consistent with CLAUDE.md architecture rules

**Trade-off**: Additional generic instantiation per I/O operation.

---

## 9. Appendices

### 9.1 Package Dependency Graph

```
TZif.API
    |
    +---> TZif.API.Desktop (composition root)
    |         |
    |         +---> TZif.Application.Operations.All_Operations
    |                   |
    |                   +---> TZif.Infrastructure.IO.Desktop
    |                   |         |
    |                   |         +---> TZif.Domain.*
    |                   |
    |                   +---> TZif.Application.Port.Inbound.*
    |                             |
    |                             +---> TZif.Domain.*
    |
    +---> TZif.Domain.Value_Object.Zone_Id
    +---> TZif.Domain.Error
```

### 9.2 File Statistics

| Category | Count |
|----------|-------|
| Ada specification files (.ads) | 92 |
| Ada body files (.adb) | 31 |
| Domain packages | 20 |
| Application packages | 18 |
| Infrastructure packages | 15 |
| API packages | 5 |

---

**Document Control:**
- Version: 99.99.99
- Last Updated: 2025-12-13
- Status: Released

**Change History:**

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 3.0.0 | 2025-12-13 | Michael Gardner | Complete regeneration for v3.0.0; updated for Functional.Try.Map_To_Result pattern; added Resource_Error kind; updated package structure |
| 1.0.0 | 2025-12-07 | Michael Gardner | Initial release |
