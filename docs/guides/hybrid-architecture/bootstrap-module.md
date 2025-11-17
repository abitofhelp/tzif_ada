# Bootstrap Layer Guide

**Version:** 1.0.0-rc1  
**Date:** November 16, 2025  
**SPDX-License-Identifier:** BSD-3-Clause  
**License File:** See LICENSE file in the project root.  
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.  
**Status:** Unreleased  

## What is the Bootstrap Layer?

The Bootstrap Layer is the **composition root** - the single place where all dependencies are wired together. Think of it as the "main" function of your application, but organized as a complete architectural layer.

**Core Principle**: The Bootstrap Layer:
- **Depends on ALL layers** - It's the only layer that can do this
- **Creates concrete instances** - Instantiates adapters, services, use cases
- **Wires dependencies** - Connects ports to adapters via dependency injection
- **Manages lifecycle** - Handles startup, signals, shutdown
- **Contains NO business logic** - Pure composition and wiring

![Component View](diagrams/component-view.svg)

## Key Responsibilities

### 1. Dependency Wiring (Composition Root)

The Bootstrap layer creates all concrete instances and wires them together:

```ada
-- Create domain services
Greeting_Service : aliased Greeting.Default_Greeting_Service;

-- Create infrastructure adapters
Output_Adapter : aliased Console_Output.Console_Output_Adapter :=
                   Console_Output.Create;

-- Create application use cases (wiring domain + infrastructure)
Use_Case : aliased Create_Greeting.Create_Greeting_Use_Case :=
             Create_Greeting.Create
               (Greeting_Service => Greeting_Service'Access,
                Output_Port      => Output_Adapter'Access);

-- Create presentation layer (wiring use case + adapter)
App : constant CLI.CLI_Application :=
        CLI.Create
          (Use_Case    => Use_Case'Access,
           Output_Port => Output_Adapter'Access);
```

**Notice:**  
- Bootstrap knows about **concrete types** (`Console_Output_Adapter`)
- Other layers only know about **interfaces** (`Output_Port`)
- All dependencies flow **through the Bootstrap layer**

### 2. Application Lifecycle

Manages the complete lifecycle:

1. **Startup** - Initialize loggers, install signal handlers
2. **Execution** - Run the application
3. **Shutdown** - Clean up resources, uninstall handlers
4. **Exit** - Map to appropriate exit code

### 3. Signal Handling

Gracefully handle operating system signals:

```ada
-- Signal handling (TODO: Fix accessibility issue)
Signals_Handler.Install (Handle_Shutdown'Access);

procedure Handle_Shutdown is
begin
   Logger.Info ("Received shutdown signal...");
   -- Clean shutdown logic
end Handle_Shutdown;
```

Supports:
- **SIGTERM (15)** - Graceful termination request
- **SIGINT (2)** - Interrupt (Ctrl+C)
- **Custom handlers** - For graceful cleanup

### 4. Exit Code Mapping

Converts application results to Unix exit codes:

```ada
Ada.Command_Line.Set_Exit_Status
  (Ada.Command_Line.Exit_Status (App_Exit_Code));
```

Uses standardized BSD exit codes (0-78).

## Bootstrap Layer Structure

```
bootstrap/
├── bootstrap.gpr                          -- Depends on ALL layers
└── src/
    ├── hybrid-bootstrap.ads               -- Root package
    ├── hybrid-bootstrap.adb
    ├── hybrid-bootstrap-main.adb          -- Synchronous entry point
    ├── hybrid-bootstrap-main_concurrent.adb -- Concurrent entry point
    ├── hybrid-bootstrap-signals.ads       -- Signal handling
    └── hybrid-bootstrap-signals.adb
```

## Two Execution Modes

The Bootstrap layer provides **two entry points**:

### Mode 1: Synchronous (Simple)

**Location:** `bootstrap/src/hybrid-bootstrap-main.adb`  

```ada
procedure Hybrid.Bootstrap.Main is
   -- Dependencies
   Main_Logger      : aliased Logger.Bootstrap_Logger := Logger.Create;
   Output_Adapter   : aliased Console_Output.Console_Output_Adapter :=
                        Console_Output.Create;
   Greeting_Service : aliased Greeting.Default_Greeting_Service;

   -- Use case
   Use_Case : aliased Create_Greeting.Create_Greeting_Use_Case :=
                Create_Greeting.Create
                  (Greeting_Service => Greeting_Service'Access,
                   Output_Port      => Output_Adapter'Access);

   -- CLI Application
   App : constant CLI.CLI_Application :=
           CLI.Create
             (Use_Case    => Use_Case'Access,
              Output_Port => Output_Adapter'Access);

   -- Exit code
   App_Exit_Code : Exit_Code.Exit_Code_Type;

begin
   -- Log startup
   Main_Logger.Info ("Starting " & Ada.Command_Line.Command_Name);

   -- Run application
   App_Exit_Code := App.Run;

   -- Log completion
   Main_Logger.Info ("Application completed with exit code:" & App_Exit_Code'Image);

   -- Set exit code
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Exit_Status (App_Exit_Code));

exception
   when E : others =>
      Main_Logger.Error ("Fatal error in main: " &
                        Ada.Exceptions.Exception_Message (E));
      Ada.Command_Line.Set_Exit_Status
        (Ada.Command_Line.Exit_Status (Exit_Code.Software));
end Hybrid.Bootstrap.Main;
```

**When to use:**  
- Simple applications
- Sequential workflows
- Easier debugging
- No concurrency needed

### Mode 2: Concurrent (With Tasks)

**Location:** `bootstrap/src/hybrid-bootstrap-main_concurrent.adb`  

```ada
procedure Hybrid.Bootstrap.Main_Concurrent is
   -- Main application task that runs separately from main thread
   task type Application_Runner is
      entry Start;
      entry Get_Exit_Code (Code : out Exit_Code.Exit_Code_Type);
   end Application_Runner;

   task body Application_Runner is
      -- Dependencies with concurrent implementations
      Logger           : aliased Logger.Concurrent.Concurrent_Logger :=
                           Logger.Concurrent.Create;
      Output_Adapter   : aliased Console_Output.Console_Output_Adapter :=
                           Console_Output.Create;
      Greeting_Service : aliased Greeting.Default_Greeting_Service;

      -- Concurrent use case
      Use_Case : aliased Create_Greeting_Concurrent.Concurrent_Create_Greeting_Use_Case :=
                   Create_Greeting_Concurrent.Create
                     (Greeting_Service => Greeting_Service'Access,
                      Output_Port      => Output_Adapter'Access);

      -- Signal handling
      Signals_Handler : Signals.System_Signals := Signals.Create;

      -- Exit code
      Exit_Code_Value : Exit_Code.Exit_Code_Type;

      -- Active task counter
      Active_Tasks : Concurrent.Counter;

      -- Shutdown handler
      procedure Handle_Shutdown is
      begin
         Logger.Info ("Received shutdown signal...");
         Active_Tasks.Decrement;
      end Handle_Shutdown;

   begin
      accept Start;

      Active_Tasks.Increment;

      -- Log startup on separate thread
      Logger.Info ("Starting " & Ada.Command_Line.Command_Name & " (concurrent mode)");

      -- Install signal handler
      Signals_Handler.Install (Handle_Shutdown'Access);

      -- Run application
      Exit_Code_Value := App.Run;

      -- Clean shutdown
      Signals_Handler.Uninstall;
      Use_Case.Shutdown;

      Active_Tasks.Wait_For_Zero;

      -- Return exit code
      accept Get_Exit_Code (Code : out Exit_Code.Exit_Code_Type) do
         Code := Exit_Code_Value;
      end Get_Exit_Code;
   end Application_Runner;

   -- Main thread variables
   Runner : Application_Runner;
   Final_Exit_Code : Exit_Code.Exit_Code_Type;

begin
   -- Start application on separate task
   Runner.Start;

   -- Wait for completion and get exit code
   Runner.Get_Exit_Code (Final_Exit_Code);

   -- Set exit status
   Ada.Command_Line.Set_Exit_Status
     (Ada.Command_Line.Exit_Status (Final_Exit_Code));
end Hybrid.Bootstrap.Main_Concurrent;
```

**When to use:**  
- Concurrent operations needed
- Long-running background tasks
- Signal handling with task coordination
- Production environments with graceful shutdown

**Key Differences:**  
- Uses `Concurrent_Logger` instead of `Bootstrap_Logger`
- Uses `Create_Greeting_Concurrent` use case
- Application runs in separate task
- Tracks active tasks with `Counter`
- Coordinates shutdown across tasks

## Complete Dependency Wiring Example

Let's trace how everything connects in the synchronous mode:

### Step 1: Domain Layer (Pure Business Logic)

```ada
-- Domain service (no dependencies on other layers)
Greeting_Service : aliased Greeting.Default_Greeting_Service;
```

This is the **pure business logic** - no I/O, no framework dependencies.

### Step 2: Infrastructure Layer (Output Adapter)

```ada
-- Concrete adapter implementing Output_Port interface
Output_Adapter : aliased Console_Output.Console_Output_Adapter :=
                   Console_Output.Create;
```

This **implements** the `Output_Port` interface defined by Application layer.

### Step 3: Application Layer (Use Case)

```ada
-- Use case receives domain service + output port
Use_Case : aliased Create_Greeting.Create_Greeting_Use_Case :=
             Create_Greeting.Create
               (Greeting_Service => Greeting_Service'Access,
                Output_Port      => Output_Adapter'Access);
```

The use case:
- Calls `Greeting_Service` for business logic
- Uses `Output_Port` interface (doesn't know it's Console!)

### Step 4: Presentation Layer (CLI)

```ada
-- CLI receives use case + output port
App : constant CLI.CLI_Application :=
        CLI.Create
          (Use_Case    => Use_Case'Access,
           Output_Port => Output_Adapter'Access);
```

The CLI:
- Handles user input
- Calls the use case
- Maps results to exit codes

### Step 5: Bootstrap Runs Everything

```ada
-- Run the wired application
App_Exit_Code := App.Run;
```

**Result**: All layers work together, but each layer only knows about its immediate dependencies.

## Signal Handling Deep Dive

The `Hybrid.Bootstrap.Signals` package provides robust signal handling:

### Signal Types

```ada
type Signal_Type is (Interrupt, Term, Kill, User1, User2, Alarm);
```

Supports common Unix signals:
- **Interrupt** - SIGINT (Ctrl+C)
- **Term** - SIGTERM (graceful shutdown request)
- **Kill** - SIGKILL (forced termination)
- **User1/User2** - SIGUSR1/SIGUSR2 (custom signals)
- **Alarm** - SIGALRM (timer expiration)

### Thread-Safe Signal Handling

```ada
protected type Signal_State is
   procedure Set_Handler (Handler : Signal_Handler_With_Context);
   function Get_Handler return Signal_Handler_With_Context;
   procedure Signal_Received (Signal : Signal_Type);
   function Last_Signal return Signal_Type;
   procedure Set_Cancelled (Value : Boolean);
   function Is_Cancelled return Boolean;
   entry Wait_For_Signal (Signal : out Signal_Type);
private
   Handler : Signal_Handler_With_Context;
   Last_Sig : Signal_Type := Interrupt;
   Has_Signal : Boolean := False;
   Cancelled : Boolean := False;
end Signal_State;
```

**Benefits:**  
- **Thread-safe** - Protected object ensures safe concurrent access
- **Cancellation** - Check if shutdown was requested
- **Signal history** - Track last signal received
- **Blocking wait** - `Wait_For_Signal` entry for coordinated shutdown

### Signal Handler Pattern

```ada
-- Enhanced handler with context
type Signal_Handler_With_Context is access procedure
  (Signal : Signal_Type; Cancelled : out Boolean);

-- Legacy handler (simpler)
type Signal_Handler is access procedure;

-- Install handler
Signals_Handler : System_Signals := Signals.Create;
Signals_Handler.Install (Handle_Shutdown'Access);

-- Handler implementation
procedure Handle_Shutdown is
begin
   Logger.Info ("Shutting down gracefully...");
   -- Clean up resources
   Active_Tasks.Decrement;
end Handle_Shutdown;
```

### Error Handling with Result

Signal operations return `Result` for explicit error handling:

```ada
package Signal_Result is new Hybrid.Domain.Model.Result
  (Ok_Type  => Boolean,
   Err_Type => Signal_Error);

-- Use it
Install_Result : Signal_Result.Result;
Install_Result := Signals_Handler.Install (Handler'Access);

if Install_Result.Is_Success then
   -- Handler installed successfully
else
   -- Handle error
   Logger.Error (Install_Result.Get_Error.Message);
end if;
```

## Exit Code Strategy

The Bootstrap layer maps all errors to standardized Unix exit codes.

### Exit Code Types (Presentation Layer)

```ada
-- Standard exit codes following BSD sysexits.h
type Exit_Code_Type is new Integer range 0 .. 255;

Success    : constant Exit_Code_Type := 0;   -- Successful termination
Error      : constant Exit_Code_Type := 1;   -- General error
Usage      : constant Exit_Code_Type := 64;  -- Command line usage error
Data_Err   : constant Exit_Code_Type := 65;  -- Data format error
No_Input   : constant Exit_Code_Type := 66;  -- Cannot open input
Software   : constant Exit_Code_Type := 70;  -- Internal software error
OS_Err     : constant Exit_Code_Type := 71;  -- System error (OS)
IO_Err     : constant Exit_Code_Type := 74;  -- Input/output error
No_Perm    : constant Exit_Code_Type := 77;  -- Permission denied
Config     : constant Exit_Code_Type := 78;  -- Configuration error
```

### Exit Code Mapping

```ada
-- Map layer errors to exit codes
function From_Domain_Error return Exit_Code_Type is (Data_Err);
function From_Application_Error return Exit_Code_Type is (Software);
function From_Infrastructure_Error return Exit_Code_Type is (IO_Err);
```

### Bootstrap Exception Handling

```ada
begin
   App_Exit_Code := App.Run;
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Exit_Status (App_Exit_Code));

exception
   when E : others =>
      Main_Logger.Error ("Fatal error in main: " &
                        Ada.Exceptions.Exception_Message (E));
      Ada.Command_Line.Set_Exit_Status
        (Ada.Command_Line.Exit_Status (Exit_Code.Software));
end Hybrid.Bootstrap.Main;
```

**Strategy:**  
- Application returns exit code normally
- Bootstrap catches any uncaught exceptions
- Maps exception to `Software` error code (70)
- Logs the error before exiting

## Testing Bootstrap Layer

Bootstrap testing focuses on **wiring correctness**, not business logic:

### Integration Tests

```ada
-- tests/integration/src/test_integration_bootstrap.adb

procedure Test_Bootstrap_Wiring is
   -- Create all components
   Greeting_Service : aliased Greeting.Default_Greeting_Service;
   Mock_Output      : aliased Mock_Output_Adapter;

   Use_Case : aliased Create_Greeting.Create_Greeting_Use_Case :=
                Create_Greeting.Create
                  (Greeting_Service => Greeting_Service'Access,
                   Output_Port      => Mock_Output'Access);

   App : CLI.CLI_Application :=
           CLI.Create
             (Use_Case    => Use_Case'Access,
              Output_Port => Mock_Output'Access);

   Exit_Code : Exit_Code.Exit_Code_Type;
begin
   -- Verify wiring by running application
   Exit_Code := App.Run;

   -- Verify result
   Assert (Exit_Code = Exit_Code.Success, "Should complete successfully");
   Assert (Mock_Output.Message_Count > 0, "Should have written output");
end Test_Bootstrap_Wiring;
```

### Signal Handler Tests

```ada
procedure Test_Signal_Handler is
   Signals : System_Signals := Signals.Create;
   Handler_Called : Boolean := False;

   procedure Test_Handler is
   begin
      Handler_Called := True;
   end Test_Handler;

begin
   -- Install handler
   Signals.Install (Test_Handler'Access);

   -- Simulate signal
   Signals.Request_Cancellation;

   -- Verify
   Assert (Signals.Is_Cancelled, "Should be cancelled");

   -- Cleanup
   Signals.Uninstall;
end Test_Signal_Handler;
```

**What to test:**  
- ✓ Dependency wiring is correct
- ✓ Signal handlers install/uninstall
- ✓ Exit codes map correctly
- ✓ Exception handling works

**What NOT to test:**  
- ✗ Business logic (test in Domain layer)
- ✗ Adapter behavior (test in Infrastructure layer)
- ✗ Use case orchestration (test in Application layer)

## Logging in Bootstrap

Bootstrap uses a **separate logger** from application logging:

```ada
-- Bootstrap logger for lifecycle events
Main_Logger : aliased Logger.Bootstrap_Logger := Logger.Create;

-- Log startup
Log_Result := Main_Logger.Info ("Starting " & Ada.Command_Line.Command_Name);

-- Log completion
Log_Result := Main_Logger.Info ("Application completed with exit code:" &
                                App_Exit_Code'Image);

-- Log errors
Log_Result := Main_Logger.Error ("Fatal error in main: " &
                                 Ada.Exceptions.Exception_Message (E));
```

**Why separate logger?**
- Bootstrap events are **lifecycle**, not business events
- Different log level/format needs
- Can be routed to different output (stderr vs file)
- Simpler - no complex logging infrastructure needed at bootstrap

## Design Patterns

### 1. Composition Root

Bootstrap is the **only place** where concrete types meet:

```ada
-- ✓ GOOD: Composition happens in Bootstrap
Output_Adapter : aliased Console_Output.Console_Output_Adapter;  -- Concrete!

-- ✗ BAD: Composition in Application layer
-- Application layer should only know interfaces, not concrete types
```

### 2. Manual Dependency Injection

No framework needed - just Ada's `'Access`:

```ada
-- Create components
Greeting_Service : aliased Greeting.Default_Greeting_Service;
Output_Adapter   : aliased Console_Output.Console_Output_Adapter;

-- Wire them together
Use_Case : aliased Create_Greeting.Create_Greeting_Use_Case :=
             Create_Greeting.Create
               (Greeting_Service => Greeting_Service'Access,  -- Inject!
                Output_Port      => Output_Adapter'Access);   -- Inject!
```

**Benefits:**  
- No magic - explicit wiring
- Compile-time checking
- Zero runtime overhead
- Easy to understand and debug

### 3. Dual Entry Points

Provide both synchronous and concurrent modes:

```ada
-- Synchronous entry point
procedure Hybrid.Bootstrap.Main;

-- Concurrent entry point
procedure Hybrid.Bootstrap.Main_Concurrent;
```

User chooses which to compile/run based on needs.

## Common Mistakes to Avoid

### ❌ Don't: Put Business Logic in Bootstrap

```ada
-- WRONG: Business logic in Bootstrap
procedure Main is
   Name : String := Get_Name_From_Args;
begin
   if Name'Length > 100 then  -- Business rule! Belongs in Domain
      Ada.Text_IO.Put_Line ("Name too long");
      return;
   end if;
end Main;
```

**Why?** Business rules belong in Domain. Bootstrap only wires and runs.

### ❌ Don't: Let Other Layers Depend on Bootstrap

```ada
-- WRONG: Application depending on Bootstrap
with Hybrid.Bootstrap;  -- NO! Bootstrap is the top of dependency tree

package Hybrid.Application.Service.Something is
   -- ...
end Hybrid.Application.Service.Something;
```

**Why?** Dependencies flow inward. Bootstrap depends on everything, nothing depends on Bootstrap.

### ❌ Don't: Create Multiple Composition Roots

```ada
-- WRONG: Multiple places creating and wiring components
procedure Main is
   Output : Console_Output := Create_Console;
   -- ... wire everything
end Main;

procedure Some_Other_Entry is
   Output : Console_Output := Create_Console;  -- Duplicate wiring!
   -- ... wire everything again
end Some_Other_Entry;
```

**Why?** Single composition root keeps wiring consistent and maintainable.

### ❌ Don't: Ignore Signal Handler Errors

```ada
-- WRONG: Ignoring signal handler errors
Signals_Handler.Install (Handle_Shutdown'Access);
-- What if installation failed?

-- RIGHT: Check Result
Install_Result := Signals_Handler.Install (Handle_Shutdown'Access);
if Install_Result.Is_Failure then
   Logger.Error ("Failed to install signal handler");
   -- Fall back to simpler shutdown or warn user
end if;
```

**Why?** Signal handling might fail on some platforms. Handle gracefully.

## Performance Considerations

### Synchronous Mode

```ada
-- Simple, straightforward
App_Exit_Code := App.Run;
```

**Characteristics:**  
- Lower memory overhead
- Simpler debugging
- Deterministic execution
- Best for simple applications

### Concurrent Mode

```ada
-- Application runs in separate task
task type Application_Runner is
   entry Start;
   entry Get_Exit_Code (Code : out Exit_Code.Exit_Code_Type);
end Application_Runner;
```

**Characteristics:**  
- Better signal responsiveness
- Parallel I/O operations
- Graceful shutdown coordination
- Best for long-running services

**Trade-off:**  
- Concurrent mode has higher overhead but better responsiveness
- Synchronous mode is simpler but blocks on I/O
- Choose based on application needs

## Summary

The Bootstrap Layer is the **composition root** of the application:

- **Wires all dependencies** - Creates concrete instances and connects them
- **Manages lifecycle** - Startup, execution, shutdown
- **Handles signals** - Graceful shutdown on SIGTERM/SIGINT
- **Maps exit codes** - Standardized Unix exit codes
- **Two modes** - Synchronous (simple) and Concurrent (tasks)
- **No business logic** - Pure composition and wiring
- **Only layer depending on all others** - Top of dependency hierarchy

**Remember**: Bootstrap wires the application together. Everything else focuses on its specific responsibility.

## Next Steps

- **[Architecture Overview](architecture-overview.md)** - Understand the full architecture
- **[Testing Guide](testing.md)** - Test the complete application
- **[Deployment Guide](deployment-guide.md)** - Deploy your application

## References

- `bootstrap/` - Bootstrap layer source code
- `tests/integration/src/` - Integration tests showing wiring
