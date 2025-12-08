# Software Design Specification (SDS)

**Version:** 1.0.0<br>
**Date:** December 07, 2025<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

---

## 1. Introduction

### 1.1 Purpose

This Software Design Specification describes the architecture and detailed design of TZif, an Ada 2022 library for parsing and querying IANA timezone information.

### 1.2 Scope

This document covers:
- 4-layer hexagonal architecture
- Package structure and dependencies
- Key types and interfaces
- Design patterns and rationale

---

## 2. Architectural Overview

### 2.1 Layered Architecture

TZif implements a 4-layer hexagonal (ports and adapters) architecture:

```
+-----------------------------------------------------------------+
|                          API Layer                               |
|  TZif.API (facade) + TZif.API.Desktop/Windows/Embedded (roots)  |
+----------------------------------+------------------------------+
                                   |
+----------------------------------v------------------------------+
|                      Application Layer                           |
|  TZif.Application.Operations + Port.Inbound.* + Port.Outbound.* |
+----------------------------------+------------------------------+
                                   |
+----------------------------------v------------------------------+
|                    Infrastructure Layer                          |
|  TZif.Infrastructure.IO.* + Adapter.* + Platform.*              |
+----------------------------------+------------------------------+
                                   |
+----------------------------------v------------------------------+
|                       Domain Layer                               |
|  TZif.Domain.Entity.* + Value_Object.* + Parser + Error.Result  |
+-----------------------------------------------------------------+
```

### 2.2 Design Principles

| Principle | Implementation |
|-----------|----------------|
| Dependency Inversion | All layers depend inward toward Domain |
| Interface Segregation | Small, focused ports per operation |
| Single Responsibility | Each package has one purpose |
| Pure Domain | Domain layer has zero external dependencies |
| Generic I/O Plugin | Platform-specific I/O injected via generics |

---

## 3. Domain Layer

### 3.1 Purpose

Pure business logic with zero external dependencies. All types and operations are platform-independent and SPARK-compatible.

### 3.2 Package Structure

```
TZif.Domain
  +-- Entity
  |     +-- Zone                    -- Zone entity with identity
  +-- Value_Object
  |     +-- Zone_Id                 -- Validated timezone identifier
  |     +-- Epoch_Seconds           -- Unix timestamp type
  |     +-- UTC_Offset              -- Timezone offset from UTC
  |     +-- Transition_Info         -- Transition data at epoch
  |     +-- TZif_Header             -- Parsed file header
  |     +-- Timezone_Type           -- Individual type record
  |     +-- Source_Info             -- Source metadata with ULID
  |     +-- IANA_Releases           -- Release version lookup
  +-- Error
  |     +-- Error                   -- Error_Kind and Error_Type
  |     +-- Result                  -- Generic_Result monad
  +-- Types
  |     +-- Bounded_Vector          -- SPARK-safe dynamic array
  |     +-- Option                  -- Option monad (Some/None)
  +-- Parser                        -- TZif binary format parser
  +-- Service
        +-- Timezone_Lookup         -- Transition lookup service
```

### 3.3 Key Types

#### 3.3.1 Zone Entity

```ada
type Zone_Type is record
   Id   : Zone_Id_Type;     -- Validated identifier
   Data : TZif_Data_Type;   -- Parsed binary data
end record;
```

#### 3.3.2 Error Types

```ada
type Error_Kind is
  (Validation_Error,   -- Invalid input
   Parse_Error,        -- Malformed data
   Not_Found_Error,    -- Resource not found
   IO_Error,           -- I/O failures
   Resource_Error,     -- Resource exhaustion
   Internal_Error);    -- Precondition violations

type Error_Type is record
   Kind    : Error_Kind;
   Message : Error_Strings.Bounded_String;
end record;
```

#### 3.3.3 Result Monad

```ada
generic
   type T is private;
package Generic_Result is
   type Result is private;

   function Ok (Value : T) return Result;
   function Error (Kind : Error_Kind; Message : String) return Result;
   function Is_Ok (Self : Result) return Boolean;
   function Is_Error (Self : Result) return Boolean;
   function Value (Self : Result) return T;
   function Error_Info (Self : Result) return Error_Type;

   --  Combinators
   generic with function F (X : T) return Result;
   function And_Then (Self : Result) return Result;

   function Unwrap_Or (Self : Result; Default : T) return T;
   -- ... more combinators
end Generic_Result;
```

### 3.4 Parser Design

The parser (`TZif.Domain.Parser`) processes TZif binary data:

```ada
procedure Parse_From_Bytes
  (Bytes  :     Byte_Array;
   Length :     Natural;
   Result : out Parse_Result_Type)
with
  Pre  => Length <= Bytes'Length and then Length > 0,
  Post => True;
```

**Design Decisions:**
- Pure procedure, no I/O dependencies
- SPARK Mode enabled for formal verification
- All bounds checked via preconditions
- Returns Result monad, never raises exceptions

---

## 4. Application Layer

### 4.1 Purpose

Defines use cases and port interfaces. Orchestrates domain operations with I/O via generic formal parameters.

### 4.2 Package Structure

```
TZif.Application
  +-- Operations                    -- Generic All_Operations package
  +-- Port
  |     +-- Inbound
  |     |     +-- Find_By_Id        -- Zone lookup port
  |     |     +-- Find_By_Pattern   -- Pattern search port
  |     |     +-- Find_By_Region    -- Region search port
  |     |     +-- Find_By_Regex     -- Regex search port
  |     |     +-- Find_My_Id        -- Local timezone port
  |     |     +-- Get_Transition_At_Epoch
  |     |     +-- Get_Version
  |     |     +-- List_All_Order_By_Id
  |     |     +-- Discover_Sources
  |     |     +-- Load_Source
  |     |     +-- Validate_Source
  |     +-- Outbound
  |           +-- Zone_Repository   -- Zone persistence interface
  |           +-- Writer            -- Output interface
  +-- UseCase                       -- Individual use case implementations
```

### 4.3 Generic I/O Plugin Pattern

The `All_Operations` generic package accepts I/O procedures as formal parameters:

```ada
generic
   type Byte_Array is array (Positive range <>) of Unsigned_8;

   with package Read_File_Result is new Generic_Result (<>);

   with procedure Read_File
     (Id     :     Zone_Id_Input_Type;
      Bytes  : out Byte_Array;
      Length : out Natural;
      Result : out Read_File_Result.Result);

   with procedure List_Directory_Sources
     (Search_Paths :     Path_List;
      Result       : out Discovery_Result.Result);

   -- ... more I/O formal parameters

package All_Operations is
   procedure Find_By_Id
     (Id : Zone_Id_Input_Type; Result : out Find_By_Id_Result_Type);

   procedure Discover_Sources
     (Search_Paths :     Path_List;
      Result       : out Discovery_Result_Type);

   -- ... 11 operations total
end All_Operations;
```

**Design Rationale:**
- Decouples business logic from I/O implementation
- Enables platform-specific adapters (Desktop, Windows, Embedded)
- Supports testing with mock I/O
- SPARK-friendly (no access types or tagged types)

---

## 5. Infrastructure Layer

### 5.1 Purpose

Implements platform-specific I/O adapters and repository patterns.

### 5.2 Package Structure

```
TZif.Infrastructure
  +-- IO
  |     +-- Desktop                 -- POSIX filesystem I/O
  |     +-- Windows                 -- Windows filesystem I/O
  |     +-- Embedded                -- Stub for custom adapters
  +-- Adapter
  |     +-- File_System
  |           +-- Repository        -- Generic zone repository
  |           +-- POSIX_Repository  -- POSIX implementation
  |           +-- Windows_Repository -- Windows implementation
  +-- Platform
  |     +-- POSIX                   -- POSIX platform operations
  |     +-- Windows                 -- Windows platform operations
  +-- Cache
  |     +-- Zone_Cache              -- In-memory zone cache
  |     +-- Source_Cache            -- Source metadata cache
  |     +-- Path_Canonical          -- Path canonicalization
  +-- ULID                          -- ULID generation
  +-- TZif_Parser                   -- Infrastructure parser wrapper
```

### 5.3 Desktop I/O Adapter

```ada
package TZif.Infrastructure.IO.Desktop is

   procedure Read_File
     (Id     :     Zone_Id_Input_Type;
      Bytes  : out Byte_Array;
      Length : out Natural;
      Result : out Read_File_Result.Result);

   procedure List_Directory_Sources
     (Search_Paths :     Path_List;
      Result       : out Discovery_Result.Result);

   procedure Read_System_Timezone_Id
     (Result : out Find_My_Id_Result.Result);

   -- ... additional I/O procedures

end TZif.Infrastructure.IO.Desktop;
```

### 5.4 Platform Operations

Platform-specific operations are abstracted via a generic package:

```ada
generic
   with function Read_Symbolic_Link (Path : String)
     return Platform_String_Result;
package Platform_Operations is
   -- Platform-agnostic interface
end Platform_Operations;

-- POSIX instantiation:
package Operations is new Platform_Operations
  (Read_Symbolic_Link => Read_Symbolic_Link);
```

---

## 6. API Layer

### 6.1 Purpose

Provides stable public interface for library consumers. Composition roots wire infrastructure adapters to application operations.

### 6.2 Package Structure

```
TZif.API
  +-- Operations                    -- Generic facade
  +-- Desktop                       -- POSIX composition root
  +-- Windows                       -- Windows composition root
  +-- Embedded                      -- Embedded composition root
```

### 6.3 Three-Package API Pattern

1. **TZif.API** - Public facade, re-exports types and delegates operations
2. **TZif.API.Operations** - Generic facade instantiated per platform
3. **TZif.API.Desktop** - Composition root wiring Desktop I/O

```ada
package TZif.API.Desktop is
   --  Instantiate operations with Desktop I/O
   package Desktop_Ops is new TZif.Application.Operations.All_Operations
     (Byte_Array => TZif.Infrastructure.IO.Desktop.Byte_Array,
      Read_File_Result => TZif.Infrastructure.IO.Desktop.Read_File_Result,
      Read_File => TZif.Infrastructure.IO.Desktop.Read_File,
      -- ... more formal parameters
     );

   --  Instantiate generic facade
   package API is new TZif.API.Operations.Facade (Ops => Desktop_Ops);
end TZif.API.Desktop;
```

### 6.4 Public API Types

The facade re-exports domain types for consumer convenience:

```ada
package TZif.API is
   --  Core types
   subtype Zone_Id_Type is Domain.Value_Object.Zone_Id.Zone_Id_Type;
   subtype Zone_Result is Find_By_Id_Port.Find_By_Id_Result_Type;
   subtype Epoch_Seconds_Type is Domain.Value_Object.Epoch_Seconds.Epoch_Seconds_Type;

   --  Operations
   function Find_By_Id (Id : Zone_Id_Type) return Zone_Result;
   function Find_My_Id return My_Zone_Result;
   function Get_Transition_At_Epoch (...) return Transition_Result;
   -- ... 11 operations
end TZif.API;
```

---

## 7. Error Handling Strategy

### 7.1 Railway-Oriented Programming

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

### 7.2 Error Kinds

| Kind | Usage |
|------|-------|
| `Validation_Error` | Invalid input data |
| `Parse_Error` | Malformed TZif file |
| `Not_Found_Error` | Zone or file not found |
| `IO_Error` | Filesystem failures |
| `Resource_Error` | Memory exhaustion |
| `Internal_Error` | Precondition violations |

---

## 8. Deployment View

### 8.1 Build Outputs

| Artifact | Description |
|----------|-------------|
| `lib/libtzif.a` | Static library |
| `bin/examples/*` | Example programs |
| `test/bin/*_runner` | Test executables |

### 8.2 Dependencies

| Package | Version | Layer |
|---------|---------|-------|
| functional | ^3.0.0 | Infrastructure |
| gnatcoll | ^25.0.0 | Infrastructure |

**Note:** Domain layer has zero external dependencies.

---

## 9. Design Decisions

### 9.1 Why Hexagonal Architecture?

- Clear separation of concerns
- Testable business logic
- Platform portability via adapters
- SPARK verification of domain

### 9.2 Why Generic I/O Plugin?

- SPARK compatible (no access-to-subprogram)
- Compile-time binding (no runtime dispatch)
- Explicit dependencies (all I/O visible in instantiation)

### 9.3 Why Result Monad?

- Explicit error handling
- No exceptions in library code
- Composable via combinators
- SPARK compatible

---

**Document Control:**
- Version: 1.0.0
- Last Updated: December 07, 2025
- Status: Released

**Change History:**

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0.0 | 2025-12-07 | Michael Gardner | Initial release |
