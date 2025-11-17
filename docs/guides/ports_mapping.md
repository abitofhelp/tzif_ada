# Ports Pattern: Go vs Ada

**Version:** 1.0.0  
**Date:** November 16, 2025  
**SPDX-License-Identifier:** BSD-3-Clause  
**License File:** See the LICENSE file in the project root.  
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.  
**Status:** Released  


## Educational Guide: How Ports Work in Both Languages

This document explains how the **Ports and Adapters pattern** (Hexagonal Architecture) is implemented in both Go and Ada, highlighting how the same architectural concept is expressed using different language idioms.

---

## What is a Port?

A **port** is an interface that defines how the application layer communicates with the outside world:

- **Input Port**: Interface through which external actors (UI, CLI, API) call into the application
- **Output Port**: Interface through which the application calls out to external services (database, console, file system)

The key principle: **Application defines the ports, Infrastructure implements them**.

---

## Output Port Pattern: WriterPort

### Go Implementation

**File**: `application/port/writer_port.go`

```go
package port

import (
    "context"
    "github.com/samber/mo"
)

// WriterPort is an OUTPUT PORT interface
// Application defines this interface
// Infrastructure implements it
type WriterPort interface {
    Write(ctx context.Context, message string) mo.Result[Unit]
}
```

**Key Points**:
- Explicit `interface` type
- Runtime polymorphism capability (but used with generics for static dispatch)
- Application layer defines the interface
- Infrastructure layer provides concrete implementation

**Usage in Use Case** (`application/usecase/greet_usecase.go`):

```go
type GreetUseCase[W WriterPort] struct {
    writer W  // Generic type parameter constrained by WriterPort interface
}

func (uc *GreetUseCase[W]) Execute(ctx context.Context, cmd port.GreetCommand) mo.Result[port.Unit] {
    // ... create person ...
    message := person.GreetingMessage()
    return uc.writer.Write(ctx, message)  // Call through port
}
```

**Key**: Generic type `W` is constrained by `WriterPort` interface, giving us **static dispatch** at compile time.

### Ada Implementation

**File**: `application/application-console_port.ads`

```ada
with Domain.Result;
with Application.Unit_Type;

package Application.Console_Port is

   pragma Preelaborate;

   -- Result type for Unit returns
   package Unit_Result is new Domain.Result.Generic_Result
     (T => Application.Unit_Type.Unit);

   -- OUTPUT PORT via generic formal parameter
   -- Application defines this signature
   -- Infrastructure provides the implementation
   generic
      with function Write (Message : String) return Unit_Result.Result;
   package Port is

      -- Rename for clarity at call site
      function Write_Message (Message : String) return Unit_Result.Result
        renames Write;

   end Port;

end Application.Console_Port;
```

**Key Points**:
- No explicit `interface` type (Ada doesn't have Go-style interfaces)
- Port contract expressed as **generic formal parameter** (`with function`)
- Application layer defines the function signature
- Infrastructure layer provides matching function

**Usage in Use Case** (`application/usecase/application-usecase-greet.ads`):

```ada
generic
   with function Console_Write (Message : String)
     return Application.Console_Port.Unit_Result.Result;
package Use_Case is

   function Execute
     (Cmd : Application.Greet_Command.Greet_Command)
      return Application.Console_Port.Unit_Result.Result;

end Use_Case;
```

**Key**: Generic `with function` parameter defines the port contract, giving us **static dispatch** at compile time.

### Comparison: WriterPort

| Aspect | Go | Ada |
|--------|-----|-----|
| **Port Definition** | `interface WriterPort` | `with function Write(...)` |
| **Binding** | Generic type constraint | Generic formal parameter |
| **Dispatch** | Static (via generics) | Static (via generics) |
| **Runtime Overhead** | Zero (static dispatch) | Zero (static dispatch) |
| **Syntax** | Interface + Generic | Generic formal parameter |
| **Wiring** | `NewGreetUseCase[*ConsoleWriter](w)` | `new Use_Case(Console_Write => ...)` |

**Educational Insight**: Both achieve the **same architectural goal** (dependency inversion with static dispatch) using different language features:
- **Go**: Interface + Generic type parameter
- **Ada**: Generic formal parameter (more direct)

---

## Input Port Pattern: GreetPort

### Go Implementation

**File**: `application/port/greet_port.go`

```go
package port

import (
    "context"
    "github.com/samber/mo"
)

// GreetPort is an INPUT PORT interface
// Presentation layer calls this
// Application layer implements this (use case)
type GreetPort interface {
    Execute(ctx context.Context, cmd GreetCommand) mo.Result[Unit]
}
```

**Key Points**:
- Explicit `interface` type for input port
- Presentation calls through this interface
- Use case implements this interface

**Implementation**:

```go
// GreetUseCase implements GreetPort interface
func (uc *GreetUseCase[W]) Execute(ctx context.Context, cmd GreetCommand) mo.Result[Unit] {
    // Use case logic...
}
```

**Usage in Presentation** (`presentation/cli/greet_command.go`):

```go
type GreetCommand struct {
    greetPort port.GreetPort  // Input port interface
}

func (gc *GreetCommand) Execute(ctx context.Context, name string) int {
    cmd := port.GreetCommand{Name: name}
    result := gc.greetPort.Execute(ctx, cmd)  // Call through input port
    // ... handle result ...
}
```

### Ada Implementation

**Conceptual Equivalent**: Ada doesn't have a separate "GreetPort" interface file. Instead, the input port contract is expressed directly as a generic formal parameter in the presentation layer.

**File**: `presentation/presentation-greet_command.ads`

```ada
with Application.Console_Port;
with Application.Greet_Command;

package Presentation.Greet_Command is

   -- INPUT PORT via generic formal parameter
   -- This IS the input port contract
   generic
      with function Execute_Greet_Use_Case
        (Cmd : Application.Greet_Command.Greet_Command)
         return Application.Console_Port.Unit_Result.Result;
   package Command is

      function Run return Integer;

   end Command;

end Presentation.Greet_Command;
```

**Key Points**:
- No separate interface file
- Input port contract is the `with function Execute_Greet_Use_Case` parameter
- Presentation receives the use case function as a generic parameter
- Application provides the implementation

**Implementation** (already shown in use case):

```ada
package body Application.Usecase.Greet is
   package body Use_Case is
      function Execute (Cmd : Application.Greet_Command.Greet_Command)
        return Application.Console_Port.Unit_Result.Result is
         -- Use case logic...
      end Execute;
   end Use_Case;
end Application.Usecase.Greet;
```

**Usage** (wiring in Bootstrap):

```ada
package Greet_Command_Instance is new
  Presentation.Greet_Command.Command
    (Execute_Greet_Use_Case => Greet_Use_Case_Instance.Execute);
```

### Comparison: GreetPort (Input Port)

| Aspect | Go | Ada |
|--------|-----|-----|
| **Port Definition** | `interface GreetPort` in separate file | `with function` parameter (no separate file) |
| **Port Location** | `application/port/greet_port.go` | Embedded in presentation package spec |
| **Binding** | Field in presentation struct | Generic formal parameter |
| **Dispatch** | Static (via generic instantiation) | Static (via generic instantiation) |
| **Runtime Overhead** | Zero | Zero |
| **Wiring** | `NewGreetCommand(greetUseCase)` | `new Command(Execute_Greet_Use_Case => ...)` |

**Educational Insight**:
- **Go**: Explicit input port interface in separate file (`greet_port.go`)
- **Ada**: Input port contract is the generic formal parameter itself
- **Both**: Achieve compile-time binding and zero runtime overhead

**Why no separate file in Ada?**
Ada's generic formal parameters ARE the interface contract. Creating a separate file would be redundant. The `with function` parameter in the presentation package spec serves the same architectural role as Go's `GreetPort` interface.

---

## Complete Flow Comparison

### Go: Presentation → Application → Infrastructure

```
1. Presentation (greet_command.go):
   type GreetCommand struct {
       greetPort port.GreetPort  // Input port
   }

2. Application (greet_port.go):
   type GreetPort interface {
       Execute(ctx, cmd) Result[Unit]
   }

3. Application (greet_usecase.go):
   type GreetUseCase[W WriterPort] struct {
       writer W  // Output port
   }
   func (uc *GreetUseCase[W]) Execute(...) Result[Unit]  // Implements GreetPort

4. Application (writer_port.go):
   type WriterPort interface {
       Write(ctx, message) Result[Unit]
   }

5. Infrastructure (console_writer_adapter.go):
   type ConsoleWriterAdapter struct {}
   func (cwa *ConsoleWriterAdapter) Write(...) Result[Unit]  // Implements WriterPort

6. Bootstrap (cli.go):
   writer := &adapter.ConsoleWriterAdapter{}
   useCase := usecase.NewGreetUseCase[adapter.WriterPort](writer)
   cmd := cli.NewGreetCommand(useCase)
```

### Ada: Presentation → Application → Infrastructure

```
1. Presentation (presentation-greet_command.ads):
   generic
      with function Execute_Greet_Use_Case(...) return Result;  -- Input port
   package Command is
      function Run return Integer;
   end Command;

2. Application (application-usecase-greet.ads):
   generic
      with function Console_Write(...) return Result;  -- Output port
   package Use_Case is
      function Execute(...) return Result;  -- Implements input port contract
   end Use_Case;

3. Application (application-console_port.ads):
   generic
      with function Write(...) return Result;  -- Output port contract
   package Port is
      function Write_Message(...) renames Write;
   end Port;

4. Infrastructure (infrastructure-adapter-console_writer.ads):
   function Write (Message : String) return Result;  -- Implements output port

5. Bootstrap (bootstrap-cli.adb):
   package Console_Port_Instance is new Console_Port.Port
     (Write => Console_Writer.Write);

   package Greet_Use_Case_Instance is new Usecase.Greet.Use_Case
     (Console_Write => Console_Port_Instance.Write_Message);

   package Greet_Command_Instance is new Greet_Command.Command
     (Execute_Greet_Use_Case => Greet_Use_Case_Instance.Execute);
```

---

## Key Architectural Principles (Same in Both)

1. **Dependency Inversion**: Application defines ports, Infrastructure implements them
2. **Static Dispatch**: All binding happens at compile time (zero runtime overhead)
3. **Separation of Concerns**: Business logic isolated from infrastructure details
4. **Testability**: Can substitute different implementations for testing

---

## Summary: Same Architecture, Different Idioms

| Concept | Go Idiom | Ada Idiom |
|---------|----------|-----------|
| **Output Port** | Interface in application/port/ | Generic formal parameter |
| **Input Port** | Interface in application/port/ | Generic formal parameter |
| **Dependency Injection** | Generic type constraint | Generic instantiation |
| **Dispatch** | Static (via generics) | Static (via generics) |
| **Wiring** | Constructor functions | Generic package instantiation |

**Educational Takeaway**: The hexagonal architecture pattern transcends language syntax. Both implementations achieve:
- ✅ Dependency inversion
- ✅ Static type safety
- ✅ Zero runtime overhead
- ✅ Testability
- ✅ Clean separation of layers

The **architecture is identical**, only the **implementation idioms differ**.
