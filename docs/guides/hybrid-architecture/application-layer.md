# Application Layer Guide

**Version:** 1.0.0  
**Date:** November 16, 2025  
**SPDX-License-Identifier:** BSD-3-Clause  
**License File:** See LICENSE file in the project root.  
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.  
**Status:** Released  

## What is the Application Layer?

The Application Layer is the **orchestrator** - it coordinates the flow of data between the Domain Layer (business logic) and the outside world (Infrastructure, Presentation). Think of it as the conductor of an orchestra: it doesn't play the instruments (Domain does that), but it tells everyone when and how to play together.

**Core Principle**: The Application Layer:
- **Depends ONLY on Domain** - never on Infrastructure or Presentation
- **Defines port interfaces** that Infrastructure implements
- **Orchestrates use cases** by calling Domain services
- **Transforms errors** between layer boundaries

![Use Case Flow](diagrams/use-case-flow.svg)

## Key Responsibilities

### 1. Use Case Implementation

Each use case is a single business operation from the user's perspective:

```ada
-- application/src/service/hybrid-application-service-create_greeting.ads

package Hybrid.Application.Service.Create_Greeting is

   -- Use case: Create a greeting and output it
   procedure Execute
      (Name : Domain.Value.Person_Name;
       Output : in out Port.Output_Port'Class);

end Hybrid.Application.Service.Create_Greeting;
```

**What's a Use Case?**
- A specific task a user wants to accomplish
- Examples: "Create Greeting", "Process Payment", "Generate Report"
- Encapsulates the business flow, not the business rules

### 2. Port Definitions (Dependency Inversion)

The Application Layer defines **what** it needs, not **how** it's implemented:

```ada
-- application/src/port/hybrid-application-port-output.ads

package Hybrid.Application.Port.Output is

   -- Interface (port) for outputting messages
   type Output_Port is interface;

   procedure Write_Output
      (Self    : Output_Port;
       Message : String) is abstract;

end Hybrid.Application.Port.Output;
```

**Why interfaces?**
- Application says "I need something that can write output"
- Infrastructure provides "Here's a console writer" or "Here's a file writer"
- Application doesn't care which - Dependency Inversion!

### 3. Error Transformation

Convert Domain errors to Application-level errors:

```ada
-- application/src/error/hybrid-application-error.ads

package Hybrid.Application.Error is

   type Application_Error is (
      Domain_Error_Occurred,
      Output_Port_Failed,
      Invalid_Input
   );

   -- Transform Domain errors to Application errors
   function From_Domain_Error
      (Error : Domain.Error.Domain_Error) return Application_Error;

end Hybrid.Application.Error;
```

## Application Layer Structure

```
application/
├── application.gpr                     -- Depends ONLY on domain.gpr
└── src/
    ├── hybrid-application.ads          -- Root package
    ├── service/
    │   ├── hybrid-application-service.ads
    │   ├── hybrid-application-service-create_greeting.ads
    │   ├── hybrid-application-service-create_greeting.adb
    │   ├── hybrid-application-service-create_greeting_concurrent.ads
    │   └── hybrid-application-service-create_greeting_concurrent.adb
    ├── port/
    │   ├── hybrid-application-port.ads
    │   └── hybrid-application-port-output.ads
    └── error/
        ├── hybrid-application-error.ads
        └── hybrid-application-error.adb
```

## Complete Use Case Example

Let's walk through the `Create_Greeting` use case step by step:

### Step 1: Define the Port (Interface)

```ada
-- application/src/port/hybrid-application-port-output.ads

package Hybrid.Application.Port.Output is

   type Output_Port is interface;

   procedure Write_Output
      (Self    : Output_Port;
       Message : String) is abstract;

end Hybrid.Application.Port.Output;
```

This says: "I need something that can write output messages."

### Step 2: Implement the Use Case

```ada
-- application/src/service/hybrid-application-service-create_greeting.adb

procedure Execute
   (Name   : Domain.Value.Person_Name;
    Output : in out Port.Output_Port'Class) is

   -- Call Domain service to create greeting
   Greeting_Result : constant Domain.Service.Greeting.Result :=
      Domain.Service.Greeting.Create_Greeting (Name);

begin
   -- Pattern match on Result
   if Greeting_Result.Is_Success then
      -- Success: write greeting to output
      declare
         Message : constant String := Greeting_Result.Get_Value;
      begin
         Output.Write_Output (Message);
      end;

   else
      -- Failure: handle error
      declare
         Error : constant Domain.Error.Domain_Error := Greeting_Result.Get_Error;
         Error_Message : constant String := "Error: " & Error'Image;
      begin
         Output.Write_Output (Error_Message);
      end;
   end if;

end Execute;
```

**Notice what Application Layer does:**  
1. Calls Domain service (`Create_Greeting`)
2. Handles Result (success/failure)
3. Uses Output_Port to write result
4. **Does NOT implement business logic** - that's Domain's job
5. **Does NOT know about console/file/database** - that's Infrastructure's job

### Step 3: Infrastructure Implements the Port

```ada
-- infrastructure/src/adapter/console/...

type Console_Output is new Output_Port with null record;

procedure Write_Output
   (Self    : Console_Output;
    Message : String) is
begin
   Ada.Text_IO.Put_Line (Message);  -- Concrete implementation
end Write_Output;
```

### Step 4: Bootstrap Wires Everything Together

```ada
-- bootstrap/src/hybrid-bootstrap-main.adb

procedure Main is
   Console : aliased Console_Output;  -- Concrete adapter
   Name    : Person_Name;
begin
   -- Wire the concrete adapter to the use case
   Create_Greeting_Service.Execute (Name, Console);
end Main;
```

## Synchronous vs Concurrent Use Cases

The Application Layer supports both execution modes:

### Synchronous (Simple)

```ada
-- hybrid-application-service-create_greeting.ads
procedure Execute
   (Name   : Domain.Value.Person_Name;
    Output : in out Port.Output_Port'Class);
```

Use when:
- Simple, sequential workflows
- No concurrency needed
- Easier debugging

### Concurrent (With Tasks)

```ada
-- hybrid-application-service-create_greeting_concurrent.ads
procedure Execute_Concurrent
   (Name   : Domain.Value.Person_Name;
    Output : in out Port.Output_Port'Class);
```

Implementation:
```ada
procedure Execute_Concurrent
   (Name   : Domain.Value.Person_Name;
    Output : in out Port.Output_Port'Class) is

   task Greeting_Task is
      entry Start (N : Domain.Value.Person_Name);
      entry Get_Result (R : out String);
   end Greeting_Task;

   task body Greeting_Task is
      Person_Name : Domain.Value.Person_Name;
      Result      : Unbounded_String;
   begin
      accept Start (N : Domain.Value.Person_Name) do
         Person_Name := N;
      end Start;

      -- Call Domain service (potentially long-running)
      declare
         Greeting_Result : constant Domain.Service.Greeting.Result :=
            Domain.Service.Greeting.Create_Greeting (Person_Name);
      begin
         if Greeting_Result.Is_Success then
            Result := To_Unbounded_String (Greeting_Result.Get_Value);
         end if;
      end;

      accept Get_Result (R : out String) do
         R := To_String (Result);
      end Get_Result;
   end Greeting_Task;

   Result_Message : String (1 .. 256);
begin
   Greeting_Task.Start (Name);
   Greeting_Task.Get_Result (Result_Message);
   Output.Write_Output (Result_Message);
end Execute_Concurrent;
```

Use when:
- Concurrent operations needed
- Parallel processing
- Async I/O

**Key Point**: Domain Layer stays the same! Concurrency is an Application concern, not a business logic concern.

## Testing Application Layer

Application Layer tests verify orchestration, not business logic:

```ada
-- tests/integration/src/test_integration_create_greeting.adb

procedure Test_Create_Greeting_Success is
   -- Mock output port for testing
   type Mock_Output is new Output_Port with record
      Last_Message : Unbounded_String;
   end record;

   procedure Write_Output
      (Self    : in out Mock_Output;
       Message : String) is
   begin
      Self.Last_Message := To_Unbounded_String (Message);
   end Write_Output;

   Mock   : aliased Mock_Output;
   Name   : constant Person_Name := Person_Name.Create ("Alice").Get_Value;
begin
   -- Execute use case
   Create_Greeting_Service.Execute (Name, Mock);

   -- Verify output
   Assert (To_String (Mock.Last_Message) = "Hello, Alice!",
           "Should output correct greeting");
end Test_Create_Greeting_Success;
```

**What we test:**  
- Use case correctly calls Domain service
- Use case handles Result properly
- Use case interacts with port correctly

**What we DON'T test:**
- Business logic (tested in Domain unit tests)
- Actual console/file I/O (tested in Infrastructure tests)

## Design Patterns

### 1. Dependency Inversion (Ports & Adapters)

```ada
-- Application defines the port (interface)
type Output_Port is interface;

-- Infrastructure implements the port
type Console_Output is new Output_Port with null record;
type File_Output is new Output_Port with null record;
type Database_Output is new Output_Port with null record;

-- Application doesn't know or care which implementation is used!
```

**Benefits:**  
- Swap implementations without changing Application code
- Easy to test with mock adapters
- Infrastructure depends on Application, not vice versa

### 2. Single Responsibility (Use Cases)

Each use case does ONE thing:

```ada
-- ✓ GOOD: One responsibility
package Create_Greeting_Service is
   procedure Execute (Name : Person_Name; Output : Output_Port'Class);
end Create_Greeting_Service;

-- ✗ BAD: Multiple responsibilities
package User_Service is
   procedure Create_Greeting (...);
   procedure Send_Email (...);
   procedure Update_Profile (...);
   procedure Delete_Account (...);
end User_Service;
```

### 3. Command Pattern

Use cases are commands - they encapsulate a request:

```ada
-- Use case = Command
procedure Execute (Name : Person_Name; Output : Output_Port'Class);

-- Caller doesn't know implementation details
-- Can queue, log, undo, etc.
```

## Common Mistakes to Avoid

### ❌ Don't: Put Business Logic in Application Layer

```ada
-- WRONG: Business logic in Application
procedure Execute (Name : Person_Name; Output : Output_Port'Class) is
begin
   if Name.To_String'Length > 100 then  -- Business rule! Belongs in Domain
      Output.Write_Output ("Name too long");
      return;
   end if;

   -- ...
end Execute;
```

**Why?** Business rules belong in Domain. Application only orchestrates.

### ❌ Don't: Depend on Infrastructure

```ada
-- WRONG: Application depending on Infrastructure
with Hybrid.Infrastructure.Adapter.Console_Output;  -- NO!

procedure Execute (Name : Person_Name) is
   Console : Console_Output;  -- Concrete type!
begin
   Console.Write_Output ("...");
end Execute;
```

**Why?** Application should depend on ports (interfaces), not adapters (concrete types).

### ❌ Don't: Return Infrastructure Types

```ada
-- WRONG: Returning infrastructure type
function Execute (Name : Person_Name) return Ada.Text_IO.File_Type;  -- NO!

-- RIGHT: Return domain type or use port
procedure Execute (Name : Person_Name; Output : Output_Port'Class);  -- YES!
```

**Why?** Application shouldn't know about infrastructure types.

### ❌ Don't: Create Multiple Use Cases in One

```ada
-- WRONG: Multiple use cases in one procedure
procedure Process_User
   (Name : Person_Name;
    Output : Output_Port'Class) is
begin
   Create_Greeting (Name, Output);
   Send_Email (Name);
   Update_Database (Name);
   Log_Activity (Name);
end Process_User;
```

**Why?** One use case = one business operation. Keep them separate and composable.

## Summary

The Application Layer is the **orchestration layer**:

- **Coordinates Domain and Infrastructure** - doesn't implement business logic or I/O
- **Defines ports** - interfaces for Infrastructure to implement
- **Implements use cases** - single business operations
- **Depends only on Domain** - uses Dependency Inversion for Infrastructure
- **Transforms errors** - converts between layer boundaries
- **Supports concurrency** - optional concurrent implementations

**Remember**: Application orchestrates, Domain implements business rules, Infrastructure handles I/O.

## Next Steps

- **[Domain Layer Guide](domain-layer.md)** - Understand the business logic layer
- **[Infrastructure Layer Guide](infrastructure-layer.md)** - Learn about adapters and I/O
- **[Ports & Adapters Pattern](https://alistair.cockburn.us/hexagonal-architecture/)** - Deep dive

## References

- `application/` - Application layer source code
- `tests/integration/src/` - Integration tests
