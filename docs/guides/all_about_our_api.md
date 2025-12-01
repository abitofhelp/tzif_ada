# All About Our API Layer

**Version:** 1.0.0  
**Date:** November 30, 2025  
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>  
**Status:** Released  

---

## About This Document

This guide explains the **three-package API pattern** used in projects built on our hybrid architecture reference design. TZif implements this pattern for its timezone operations.

The examples below use `Hybrid_Lib_Ada` with a simplified "Greet" use case to illustrate the architectural concepts clearly. The same patterns apply to TZif's timezone operations (`Find_By_Id`, `Get_Version`, `Find_My_Id`, etc.) - only the domain-specific types and operations differ.

**Key takeaway:** Understand the pattern here, then see it applied in the actual TZif source code.

---

## Table of Contents

1. [Overview](#overview)
2. [The Three-Package Pattern](#the-three-package-pattern)
3. [Creating Platform-Specific Composition Roots](#creating-platform-specific-composition-roots)
4. [Package Details](#package-details)
5. [Architecture Decision Factors](#architecture-decision-factors)
6. [Usage Patterns](#usage-patterns)
7. [Design Rationale](#design-rationale)

---

## Overview

The hybrid architecture uses a **three-package API pattern** that separates concerns between SPARK-verifiable operations, platform-specific wiring, and a convenient public facade. This architecture enables:

- **Formal verification** of core operation logic via SPARK
- **Platform flexibility** through composition roots
- **Simple usage** via a thin public facade
- **Clean hexagonal architecture** compliance

```
┌─────────────────────────────────────────────────────────────┐
│                      Public API Usage                        │
│                                                              │
│   with Hybrid_Lib_Ada.API;                                  │
│   Result := API.Greet (API.Create_Greet_Command ("Alice")); │
│                                                              │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                  Hybrid_Lib_Ada.API                          │
│                  (Thin Facade)                               │
│                                                              │
│   - Re-exports Domain/Application types                      │
│   - Delegates Greet to API.Desktop                          │
│   - SPARK_Mode: Off (wiring)                                │
│   - Not Preelaborate (imports Desktop)                      │
│                                                              │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│              Hybrid_Lib_Ada.API.Desktop                      │
│              (Composition Root)                              │
│                                                              │
│   - Located in: src/api/desktop/                            │
│   - Instantiates API.Operations with Console_Writer          │
│   - SPARK_Mode: Off (I/O wiring)                            │
│   - CAN import Infrastructure (arch_guard exception)         │
│                                                              │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│            Hybrid_Lib_Ada.API.Operations                     │
│            (SPARK-Safe Operations)                           │
│                                                              │
│   - Generic package parameterized by Writer port             │
│   - SPARK_Mode: On (formally verifiable)                    │
│   - Depends ONLY on Application/Domain                       │
│   - No Infrastructure dependencies                           │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

---

## The Three-Package Pattern

### Why Three Packages?

| Package | Purpose | SPARK | Dependencies |
|---------|---------|-------|--------------|
| `API.Operations` | SPARK-safe operations surface | On | Application, Domain only |
| `API.Desktop` | Platform composition root | Off | All layers including Infrastructure |
| `API` | Public facade for users | Off | Delegates to Desktop |

This separation achieves:

1. **SPARK verification** of business logic without I/O pollution
2. **Composition flexibility** for different platforms (desktop, embedded, web)
3. **Clean public interface** that hides internal complexity
4. **Hexagonal architecture compliance** with proper layer boundaries

---

## Creating Platform-Specific Composition Roots

### Understanding API.Desktop

`API.Desktop` is the **default composition root** provided by the library. It's designed for desktop/server environments where output goes to the console via `Ada.Text_IO`. However, this is just one possible wiring - the architecture is designed to support multiple platforms.

### Why Multiple Composition Roots?

Different deployment targets require different infrastructure adapters:

| Target Platform | Composition Root | Writer Adapter |
|-----------------|------------------|----------------|
| Desktop/Server | `API.Desktop` | Console_Writer (Ada.Text_IO) |
| Embedded (STM32F769I) | `API.Embedded` | UART_Writer or LCD_Writer |
| Web/WASM | `API.Web` | Browser console or DOM |
| Testing | `API.Test` | Captured_Writer (mock) |

The key insight: **the business logic in `API.Operations` never changes** - only the wiring in the composition root differs.

### Step-by-Step: Creating an Embedded Composition Root

To create a composition root for an embedded target like STM32F769I:

#### Step 1: Create the Infrastructure Adapter

First, implement a Writer adapter for your hardware:

```ada
--  src/infrastructure/adapter/uart_writer/
--  infrastructure-adapter-uart_writer.ads

pragma Ada_2022;
pragma SPARK_Mode (Off);  --  Hardware I/O

with Application.Port.Outbound.Writer;

package Infrastructure.Adapter.UART_Writer is

   function Write (Message : String)
     return Application.Port.Outbound.Writer.Unit_Result.Result;
   --  Writes message to UART peripheral

end Infrastructure.Adapter.UART_Writer;
```

```ada
--  infrastructure-adapter-uart_writer.adb

pragma Ada_2022;

with STM32.Board;       --  Your BSP
with STM32.USART;
with Domain.Unit;

package body Infrastructure.Adapter.UART_Writer is

   function Write (Message : String)
     return Application.Port.Outbound.Writer.Unit_Result.Result
   is
      use Application.Port.Outbound.Writer;
   begin
      --  Send each character via UART
      for C of Message loop
         STM32.USART.Transmit (STM32.Board.USART_1, Character'Pos (C));
      end loop;

      --  Send newline
      STM32.USART.Transmit (STM32.Board.USART_1, 13);  -- CR
      STM32.USART.Transmit (STM32.Board.USART_1, 10);  -- LF

      return Unit_Result.Ok (Domain.Unit.Unit_Value);

   exception
      when others =>
         return Unit_Result.Error
           (Kind    => Domain.Error.IO_Error,
            Message => "UART transmission failed");
   end Write;

end Infrastructure.Adapter.UART_Writer;
```

#### Step 2: Create the Composition Root

Create a new composition root that wires the embedded adapter:

```ada
--  src/api/embedded/
--  hybrid_lib_ada-api-embedded.ads

pragma Ada_2022;
pragma SPARK_Mode (Off);  --  I/O wiring

with Application.Command.Greet;
with Application.Port.Outbound.Writer;
with Infrastructure.Adapter.UART_Writer;  --  Your embedded adapter
with Hybrid_Lib_Ada.API.Operations;

package Hybrid_Lib_Ada.API.Embedded is

   --  Instantiate Operations with UART writer
   package UART_Ops is new Hybrid_Lib_Ada.API.Operations
     (Writer => Infrastructure.Adapter.UART_Writer.Write);

   --  Re-export the Greet function
   function Greet
     (Cmd : Application.Command.Greet.Greet_Command)
      return Application.Port.Outbound.Writer.Unit_Result.Result
   renames UART_Ops.Greet;

end Hybrid_Lib_Ada.API.Embedded;
```

#### Step 3: Update arch_guard (if used)

If you're using arch_guard for dependency validation, add the new composition root path:

```python
# In your arch_guard configuration
COMPOSITION_ROOT_PATHS = [
    "api/desktop",
    "api/embedded",  # Add new composition root
]
```

#### Step 4: Use in Your Embedded Application

```ada
--  Your embedded application main

with Hybrid_Lib_Ada.API.Embedded;  --  Use embedded composition root
with Application.Command.Greet;

procedure Main is
   use Hybrid_Lib_Ada.API.Embedded;

   Cmd    : constant Application.Command.Greet.Greet_Command :=
     Application.Command.Greet.Create ("STM32");
   Result : constant Application.Port.Outbound.Writer.Unit_Result.Result :=
     Greet (Cmd);
begin
   --  "Hello, STM32!" sent to UART
   if Application.Port.Outbound.Writer.Unit_Result.Is_Ok (Result) then
      null;  --  Success
   end if;
end Main;
```

### Composition Root Checklist

When creating a new composition root, ensure:

| Requirement | Description |
|-------------|-------------|
| **Location** | Under `src/api/<platform>/` directory |
| **SPARK_Mode** | Off (contains I/O wiring) |
| **Imports** | Infrastructure adapter for your platform |
| **Instantiation** | Instantiate `API.Operations` with your adapter |
| **Re-export** | Re-export the operation functions |
| **arch_guard** | Add path to composition root whitelist |

### Diagram: Multiple Composition Roots

```
┌─────────────────────────────────────────────────────────────────┐
│                      API.Operations                              │
│                    (SPARK-Safe Generic)                          │
│                                                                  │
│   generic with function Writer (...) return ...;                 │
│   package API.Operations is                                      │
│      function Greet (Cmd) return Result;                        │
│   end;                                                           │
└───────────────────────────┬─────────────────────────────────────┘
                            │
          ┌─────────────────┼─────────────────┐
          │                 │                 │
          ▼                 ▼                 ▼
┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐
│   API.Desktop   │ │  API.Embedded   │ │    API.Web      │
│                 │ │                 │ │                 │
│ Console_Writer  │ │  UART_Writer    │ │  DOM_Writer     │
└────────┬────────┘ └────────┬────────┘ └────────┬────────┘
         │                   │                   │
         ▼                   ▼                   ▼
┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐
│  Ada.Text_IO    │ │  STM32.USART    │ │  Browser API    │
└─────────────────┘ └─────────────────┘ └─────────────────┘
```

### Benefits of This Pattern

1. **Zero code duplication** - Business logic in `API.Operations` is shared
2. **Platform isolation** - Each composition root handles its own I/O
3. **SPARK verification preserved** - Core logic remains formally verifiable
4. **Easy testing** - Create `API.Test` with a mock writer
5. **Build-time selection** - Choose composition root via GPR configuration

### Complete Directory Structure Example

When adding an embedded composition root, your directory structure should look like:

```
src/
├── api/
│   ├── hybrid_lib_ada-api.ads          # Public facade
│   ├── hybrid_lib_ada-api.adb
│   │
│   ├── operations/                      # SPARK-safe (shared by all)
│   │   └── hybrid_lib_ada-api-operations.ads
│   │
│   ├── desktop/                         # Desktop composition root
│   │   └── hybrid_lib_ada-api-desktop.ads
│   │
│   └── embedded/                        # NEW: Embedded composition root
│       └── hybrid_lib_ada-api-embedded.ads
│
└── infrastructure/
    └── adapter/
        ├── console_writer/              # Desktop adapter
        │   ├── infrastructure-adapter-console_writer.ads
        │   └── infrastructure-adapter-console_writer.adb
        │
        └── uart_writer/                 # NEW: Embedded adapter
            ├── infrastructure-adapter-uart_writer.ads
            └── infrastructure-adapter-uart_writer.adb
```

### GPR Configuration for Platform Selection

You can configure your GPR project to select composition roots at build time:

```ada
--  hybrid_lib_ada.gpr

project Hybrid_Lib_Ada is

   type Platform_Type is ("desktop", "embedded", "web");
   Platform : Platform_Type := external ("PLATFORM", "desktop");

   --  Base source directories (always included)
   Base_Dirs := ("src",
                 "src/domain/**",
                 "src/application/**",
                 "src/api",
                 "src/api/operations");

   --  Platform-specific directories
   case Platform is
      when "desktop" =>
         for Source_Dirs use Base_Dirs &
           ("src/infrastructure/adapter/console_writer",
            "src/api/desktop");

      when "embedded" =>
         for Source_Dirs use Base_Dirs &
           ("src/infrastructure/adapter/uart_writer",
            "src/api/embedded");

      when "web" =>
         for Source_Dirs use Base_Dirs &
           ("src/infrastructure/adapter/dom_writer",
            "src/api/web");
   end case;

   --  ... rest of project configuration

end Hybrid_Lib_Ada;
```

Build for different platforms:

```bash
# Desktop (default)
alr build

# Embedded STM32
alr build -- -XPLATFORM=embedded

# Web/WASM
alr build -- -XPLATFORM=web
```

### Modifying the Public Facade (Optional)

If you want the thin public API facade to automatically use your composition root, you have two options:

**Option A: Conditional Compilation (Recommended)**

Create platform-specific body files:

```ada
--  hybrid_lib_ada-api__desktop.adb (for desktop)
with Hybrid_Lib_Ada.API.Desktop;

package body Hybrid_Lib_Ada.API is
   function Greet (Cmd : Greet_Command) return Unit_Result.Result is
   begin
      return Hybrid_Lib_Ada.API.Desktop.Greet (Cmd);
   end Greet;
end Hybrid_Lib_Ada.API;
```

```ada
--  hybrid_lib_ada-api__embedded.adb (for embedded)
with Hybrid_Lib_Ada.API.Embedded;

package body Hybrid_Lib_Ada.API is
   function Greet (Cmd : Greet_Command) return Unit_Result.Result is
   begin
      return Hybrid_Lib_Ada.API.Embedded.Greet (Cmd);
   end Greet;
end Hybrid_Lib_Ada.API;
```

Configure GPR to select the correct body:

```ada
case Platform is
   when "desktop" =>
      for Body ("Hybrid_Lib_Ada.API") use "hybrid_lib_ada-api__desktop.adb";
   when "embedded" =>
      for Body ("Hybrid_Lib_Ada.API") use "hybrid_lib_ada-api__embedded.adb";
end case;
```

**Option B: Direct Import (Simpler)**

Skip the facade and import the composition root directly in your application:

```ada
--  Embedded application
with Hybrid_Lib_Ada.API.Embedded;  --  Direct import

procedure Main is
   Result : constant ... := Hybrid_Lib_Ada.API.Embedded.Greet (Cmd);
begin
   ...
end Main;
```

### Embedded Systems Considerations

When creating adapters for embedded systems, keep these constraints in mind:

| Constraint | Solution |
|------------|----------|
| **No heap allocation** | Use stack-based bounded strings (already done in Domain) |
| **No Ada.Text_IO** | Use hardware-specific I/O (UART, SPI, I2C) |
| **Ravenscar profile** | Ensure adapter is tasking-safe if needed |
| **Limited stack** | Keep message buffers reasonable (< 256 bytes) |
| **No exceptions** | Use Result monad (already implemented) |
| **Timing constraints** | Consider non-blocking I/O or DMA |

Example with DMA for high-performance UART:

```ada
function Write (Message : String)
  return Application.Port.Outbound.Writer.Unit_Result.Result
is
   use Application.Port.Outbound.Writer;
begin
   --  Start DMA transfer (non-blocking)
   STM32.DMA.Start_Transfer
     (Channel => USART_TX_DMA,
      Source  => Message'Address,
      Length  => Message'Length);

   --  Wait for completion (or use interrupt callback)
   while not STM32.DMA.Transfer_Complete (USART_TX_DMA) loop
      null;  --  Busy wait (or yield to other tasks)
   end loop;

   return Unit_Result.Ok (Domain.Unit.Unit_Value);
end Write;
```

### Testing with Mock Composition Root

For unit testing, create a captured writer that stores output for assertions:

```ada
--  test/common/mock_writer.ads

with Ada.Strings.Unbounded;
with Application.Port.Outbound.Writer;

package Mock_Writer is

   --  Storage for captured messages
   Last_Message : Ada.Strings.Unbounded.Unbounded_String;
   Call_Count   : Natural := 0;
   Should_Fail  : Boolean := False;

   --  Writer implementation
   function Write (Message : String)
     return Application.Port.Outbound.Writer.Unit_Result.Result;

   --  Test helpers
   procedure Reset;
   function Get_Last_Message return String;

end Mock_Writer;
```

Then create a test composition root:

```ada
--  test/common/test_operations.ads

with Hybrid_Lib_Ada.API.Operations;
with Mock_Writer;

package Test_Operations is new Hybrid_Lib_Ada.API.Operations
  (Writer => Mock_Writer.Write);
```

Use in tests:

```ada
procedure Test_Greet_Valid_Name is
   Cmd    : constant Greet_Command := Create ("Alice");
   Result : constant Unit_Result.Result := Test_Operations.Greet (Cmd);
begin
   Assert (Unit_Result.Is_Ok (Result));
   Assert (Mock_Writer.Get_Last_Message = "Hello, Alice!");
   Assert (Mock_Writer.Call_Count = 1);
end Test_Greet_Valid_Name;
```

### Troubleshooting

| Problem | Cause | Solution |
|---------|-------|----------|
| `arch_guard: API importing Infrastructure` | Composition root not in whitelist | Add `api/<platform>/` to composition root paths |
| `undefined reference to Write` | Adapter not in Source_Dirs | Add adapter directory to GPR for selected platform |
| `Body of API not found` | Multiple body files conflict | Use GPR `for Body` clause to select correct one |
| `Preelaborate violation` | Composition root importing non-Preelaborate | SPARK_Mode(Off) is correct; Preelaborate not required |
| `generic formal function mismatch` | Writer signature doesn't match port | Ensure return type is exactly `Unit_Result.Result` |

### Quick Reference: Creating a New Composition Root

```bash
# 1. Create adapter directory
mkdir -p src/infrastructure/adapter/my_writer

# 2. Create adapter spec and body
touch src/infrastructure/adapter/my_writer/infrastructure-adapter-my_writer.ads
touch src/infrastructure/adapter/my_writer/infrastructure-adapter-my_writer.adb

# 3. Create composition root directory
mkdir -p src/api/my_platform

# 4. Create composition root spec
touch src/api/my_platform/hybrid_lib_ada-api-my_platform.ads

# 5. Update GPR project to include new directories
# 6. Update arch_guard whitelist if used
# 7. Build and test
alr build -- -XPLATFORM=my_platform
```

---

## Package Details

### 1. API.Operations

**Location:** `src/api/operations/`

**Purpose:** SPARK-safe, generic operations that depend only on Application and Domain layers.

**Key Characteristics:**
- `pragma SPARK_Mode (On)` - Formally verifiable
- Generic package parameterized by port functions
- No Infrastructure dependencies
- No direct I/O operations

**Code Structure:**

```ada
pragma Ada_2022;
pragma SPARK_Mode (On);

with Application.Command.Greet;
with Application.Port.Outbound.Writer;
with Application.Usecase.Greet;

generic
   with function Writer
     (Message : String)
      return Application.Port.Outbound.Writer.Unit_Result.Result;
package Hybrid_Lib_Ada.API.Operations is

   package Greet_Use_Case is new Application.Usecase.Greet (Writer => Writer);

   function Greet
     (Cmd : Application.Command.Greet.Greet_Command)
      return Application.Port.Outbound.Writer.Unit_Result.Result
   renames Greet_Use_Case.Execute;

end Hybrid_Lib_Ada.API.Operations;
```

**Why Generic?**
- Enables compile-time dependency injection (static polymorphism)
- Different instantiations for different platforms
- SPARK-compatible (no runtime dispatching)

---

### 2. API.Desktop

**Location:** `src/api/desktop/`

**Purpose:** Desktop platform composition root that wires Infrastructure adapters to the generic Operations.

**Key Characteristics:**
- `pragma SPARK_Mode (Off)` - Contains I/O wiring
- Located under `api/desktop/` - recognized as composition root by arch_guard
- Imports Infrastructure adapters
- Instantiates API.Operations with concrete adapter

**Code Structure:**

```ada
pragma Ada_2022;
pragma SPARK_Mode (Off);

with Application.Command.Greet;
with Application.Port.Outbound.Writer;
with Infrastructure.Adapter.Console_Writer;
with Hybrid_Lib_Ada.API.Operations;

package Hybrid_Lib_Ada.API.Desktop is

   package Console_Ops is new Hybrid_Lib_Ada.API.Operations
     (Writer => Infrastructure.Adapter.Console_Writer.Write);

   function Greet
     (Cmd : Application.Command.Greet.Greet_Command)
      return Application.Port.Outbound.Writer.Unit_Result.Result
   renames Console_Ops.Greet;

end Hybrid_Lib_Ada.API.Desktop;
```

**Why a Separate Composition Root?**
- Isolates platform-specific wiring from pure logic
- Enables future platform variants (Web, Embedded, etc.)
- api/desktop serves as the composition root (4-layer library model)

---

### 3. API (Public Facade)

**Location:** `src/api/`

**Purpose:** Thin public facade that re-exports types and delegates operations to Desktop.

**Key Characteristics:**
- Re-exports Domain types
- Re-exports Application types
- Delegates operations to API.Desktop
- Not Preelaborate (body imports Desktop which imports Infrastructure)

**Spec:**

```ada
pragma Ada_2022;

with Domain.Value_Object.Person;
with Domain.Error;
with Domain.Unit;
with Application.Command.Greet;
with Application.Port.Outbound.Writer;

package Hybrid_Lib_Ada.API is

   --  Re-export Domain Types
   subtype Person_Type is Domain.Value_Object.Person.Person;
   function Create_Person (Name : String)
     return Domain.Value_Object.Person.Person_Result.Result
   renames Domain.Value_Object.Person.Create;
   package Person_Result renames Domain.Value_Object.Person.Person_Result;

   --  Re-export Application Types
   subtype Greet_Command is Application.Command.Greet.Greet_Command;
   function Create_Greet_Command (Name : String) return Greet_Command
   renames Application.Command.Greet.Create;
   package Unit_Result renames Application.Port.Outbound.Writer.Unit_Result;

   --  Public Operation
   function Greet (Cmd : Greet_Command) return Unit_Result.Result;

end Hybrid_Lib_Ada.API;
```

**Body:**

```ada
pragma Ada_2022;

with Hybrid_Lib_Ada.API.Desktop;

package body Hybrid_Lib_Ada.API is

   function Greet (Cmd : Greet_Command) return Unit_Result.Result is
   begin
      return Hybrid_Lib_Ada.API.Desktop.Greet (Cmd);
   end Greet;

end Hybrid_Lib_Ada.API;
```

---

## Architecture Decision Factors

### Why Not a Single API Package?

A monolithic API package would:
- Mix SPARK-verifiable code with I/O operations
- Prevent formal verification of business logic
- Violate hexagonal architecture (API directly importing Infrastructure)

### Why Not API_Operations (Sibling) Instead of API.Operations (Child)?

We considered making `API_Operations` a top-level sibling package to enable `Preelaborate`. However:

| Factor | API.Operations (Child) | API_Operations (Sibling) |
|--------|------------------------|--------------------------|
| Naming | Clean hierarchy | Awkward underscore naming |
| Discoverability | Easy (under API.*) | Harder to find |
| Preelaborate | Not possible | Possible |
| SPARK | Works either way | Works either way |

**Decision:** Chose `API.Operations` (child) because:
1. **SPARK is the priority**, and it works either way
2. **Preelaborate adds minimal value** for this use case (consumers are non-Preelaborate anyway)
3. **Clean hierarchy** is more idiomatic Ada

### Why api/desktop/ as Composition Root?

For **libraries** (not applications), the composition root lives under `api/`:
- Libraries use 4-layer model (Domain, Application, Infrastructure, API)
- `api/desktop/` serves as the composition root for desktop platforms
- arch_guard recognizes this pattern and allows Infrastructure imports

---

## Usage Patterns

### Simple Usage (Recommended)

Most users should use the thin API facade:

```ada
with Hybrid_Lib_Ada.API;

procedure Main is
   use Hybrid_Lib_Ada.API;

   Cmd    : constant Greet_Command := Create_Greet_Command ("Alice");
   Result : constant Unit_Result.Result := Greet (Cmd);
begin
   if Unit_Result.Is_Ok (Result) then
      --  Greeting was written to console: "Hello, Alice!"
      null;
   else
      --  Handle error
      declare
         Info : constant Error_Type := Unit_Result.Error_Info (Result);
      begin
         --  Info.Kind, Info.Message available
         null;
      end;
   end if;
end Main;
```

### Custom Platform Usage (Advanced)

For custom platforms, instantiate `API.Operations` directly:

```ada
with Hybrid_Lib_Ada.API.Operations;
with My_Custom_Writer;

procedure Custom_Platform is
   --  Instantiate with custom writer
   package Custom_Ops is new Hybrid_Lib_Ada.API.Operations
     (Writer => My_Custom_Writer.Write);

   Cmd    : constant Application.Command.Greet.Greet_Command := ...;
   Result : constant Application.Port.Outbound.Writer.Unit_Result.Result :=
     Custom_Ops.Greet (Cmd);
begin
   --  Handle result
end Custom_Platform;
```

---

## Design Rationale

### Hexagonal Architecture Compliance

The three-package pattern maintains proper layer boundaries:

```
┌─────────────────────────────────────────────────────────────┐
│                         API Layer                            │
│  ┌─────────────┐  ┌──────────────┐  ┌───────────────────┐  │
│  │    API      │  │ API.Desktop  │  │  API.Operations   │  │
│  │  (facade)   │  │(composition) │  │    (SPARK ops)    │  │
│  └──────┬──────┘  └──────┬───────┘  └─────────┬─────────┘  │
│         │                │                     │            │
└─────────┼────────────────┼─────────────────────┼────────────┘
          │                │                     │
          ▼                ▼                     ▼
┌─────────────────┐ ┌─────────────────┐ ┌─────────────────────┐
│ Application     │ │ Infrastructure  │ │   Application +     │
│ (types only)    │ │ (Console_Writer)│ │   Domain only       │
└─────────────────┘ └─────────────────┘ └─────────────────────┘
```

### SPARK Boundary

The SPARK verification boundary is cleanly defined:

- **Inside SPARK boundary:** API.Operations, Application.Usecase, Domain.*
- **Outside SPARK boundary:** API.Desktop, Infrastructure.*

This allows formal verification of business logic while permitting I/O operations at the edges.

### Static Polymorphism

Ada generics provide static polymorphism (compile-time binding):

```ada
generic
   with function Writer (...) return ...;
package API.Operations is ...
```

This is:
- **SPARK-compatible** (no runtime dispatching)
- **Zero runtime overhead** (monomorphization)
- **Type-safe** at compile time

---

## See Also

- [Architecture Enforcement](architecture_enforcement.md)
- [Error Handling Strategy](error_handling_strategy.md)
- [Build Profiles](build_profiles.md)
