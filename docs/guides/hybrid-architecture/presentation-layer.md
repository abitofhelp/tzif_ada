# Presentation Layer Guide

**Version:** 1.0.0-rc1  
**Date:** November 16, 2025  
**SPDX-License-Identifier:** BSD-3-Clause  
**License File:** See LICENSE file in the project root.  
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.  
**Status:** Unreleased  

## What is the Presentation Layer?

The Presentation Layer is the **user-facing boundary** of your application - it's how users interact with your system. Whether through a CLI, REST API, GUI, or any other interface, this layer handles input, translates it to use case calls, and presents results back to the user.

**Core Principle**: The Presentation Layer:
- **Depends ONLY on Application** - never on Infrastructure or Domain directly
- **Acts as input adapter** - translates user actions to use case calls
- **Handles user input validation** - format, parsing, basic validation
- **Maps errors to user-friendly output** - exit codes, error messages

![Use Case Flow](diagrams/use-case-flow.svg)

## Key Responsibilities

### 1. Accept User Input

Handle various input sources:
- **Command-line arguments** - flags, parameters
- **Interactive prompts** - user input during runtime
- **HTTP requests** - REST API endpoints
- **GUI events** - button clicks, form submissions

### 2. Translate to Use Case Calls

Convert user actions into Application layer use cases:

```ada
-- User provides command-line argument
-- CLI parses it and calls the use case

procedure Handle_CLI (Args : Argument_Array) is
   Name_Str : constant String := Args (1);  -- User input
   Name_Result : constant Result := Person_Name.Create (Name_Str);
begin
   if Name_Result.Is_Success then
      -- Translate to use case call
      Create_Greeting_Service.Execute
         (Name   => Name_Result.Get_Value,
          Output => Console);
   else
      -- Handle validation error
      Display_Error (Name_Result.Get_Error);
   end if;
end Handle_CLI;
```

### 3. Map Errors to Exit Codes

Convert application errors to user-friendly signals:

```ada
package Hybrid.Presentation.Exit_Code is

   type Exit_Code is (
      Success,
      Invalid_Arguments,
      Invalid_Input,
      Application_Error,
      System_Error
   );

   function To_Integer (Code : Exit_Code) return Integer;

end Hybrid.Presentation.Exit_Code;
```

## Presentation Layer Structure

```
presentation/
├── presentation.gpr                  -- Depends ONLY on application.gpr
└── src/
    ├── hybrid-presentation.ads       -- Root package
    ├── cli/
    │   ├── hybrid-presentation-cli.ads
    │   └── hybrid-presentation-cli.adb
    └── hybrid-presentation-exit_code.ads
```

## Complete CLI Example

Let's build a complete command-line interface:

### Step 1: Define Exit Codes

```ada
-- presentation/src/hybrid-presentation-exit_code.ads

package Hybrid.Presentation.Exit_Code is

   type Exit_Code is (
      Success,              -- 0
      Invalid_Arguments,    -- 1
      Invalid_Input,        -- 2
      Application_Error,    -- 3
      System_Error          -- 4
   );

   function To_Integer (Code : Exit_Code) return Integer;

end Hybrid.Presentation.Exit_Code;
```

Implementation:
```ada
function To_Integer (Code : Exit_Code) return Integer is
begin
   case Code is
      when Success           => return 0;
      when Invalid_Arguments => return 1;
      when Invalid_Input     => return 2;
      when Application_Error => return 3;
      when System_Error      => return 4;
   end case;
end To_Integer;
```

### Step 2: Implement CLI Handler

```ada
-- presentation/src/cli/hybrid-presentation-cli.ads

package Hybrid.Presentation.CLI is

   procedure Run
      (Args   : Ada.Command_Line.Argument_Array;
       Output : in out Application.Port.Output_Port'Class)
      return Exit_Code.Exit_Code;

   procedure Display_Usage;

   procedure Display_Error (Error : Domain.Error.Domain_Error);

end Hybrid.Presentation.CLI;
```

Implementation:
```ada
-- presentation/src/cli/hybrid-presentation-cli.adb

procedure Run
   (Args   : Ada.Command_Line.Argument_Array;
    Output : in out Application.Port.Output_Port'Class)
   return Exit_Code.Exit_Code is
begin
   -- Validate arguments
   if Args'Length = 0 then
      Display_Usage;
      return Exit_Code.Invalid_Arguments;
   end if;

   -- Parse input
   declare
      Name_Str : constant String := Args (Args'First);
   begin
      -- Create value object (validates input)
      declare
         Name_Result : constant Result :=
            Domain.Value.Person_Name.Create (Name_Str);
      begin
         if Name_Result.Is_Failure then
            -- Input validation failed
            Display_Error (Name_Result.Get_Error);
            return Exit_Code.Invalid_Input;
         end if;

         -- Call use case
         Application.Service.Create_Greeting.Execute
            (Name   => Name_Result.Get_Value,
             Output => Output);

         return Exit_Code.Success;
      end;
   end;

exception
   when E : others =>
      Ada.Text_IO.Put_Line ("Unexpected error: " & Exception_Message (E));
      return Exit_Code.System_Error;
end Run;

procedure Display_Usage is
begin
   Ada.Text_IO.Put_Line ("Usage: hybrid-app <name>");
   Ada.Text_IO.Put_Line ("  Creates a greeting for the given name");
   Ada.Text_IO.Put_Line ("Example: hybrid-app Alice");
end Display_Usage;

procedure Display_Error (Error : Domain.Error.Domain_Error) is
begin
   case Error is
      when Domain.Error.Empty_Person_Name =>
         Ada.Text_IO.Put_Line ("Error: Name cannot be empty");
      when Domain.Error.Person_Name_Too_Long =>
         Ada.Text_IO.Put_Line ("Error: Name is too long (max 100 characters)");
      when Domain.Error.Invalid_Person_Name =>
         Ada.Text_IO.Put_Line ("Error: Invalid name format");
      when others =>
         Ada.Text_IO.Put_Line ("Error: " & Error'Image);
   end case;
end Display_Error;
```

### Step 3: Wire It Together in Main

```ada
-- bootstrap/src/hybrid-bootstrap-main.adb

with Ada.Command_Line;
with Hybrid.Presentation.CLI;
with Hybrid.Presentation.Exit_Code;
with Hybrid.Infrastructure.Adapter.Console_Output;

procedure Main is
   Console    : aliased Infrastructure.Adapter.Console_Output.Console_Output;
   Args       : constant Ada.Command_Line.Argument_Array := ...;
   Exit_Status : Presentation.Exit_Code.Exit_Code;
begin
   Exit_Status := Presentation.CLI.Run (Args, Console);

   Ada.Command_Line.Set_Exit_Status
      (Presentation.Exit_Code.To_Integer (Exit_Status));
end Main;
```

## Input Validation Strategy

### User Input Validation (Presentation)

Basic format/parsing validation:

```ada
-- ✓ GOOD: Format validation in Presentation
if Args'Length = 0 then
   Display_Usage;
   return Invalid_Arguments;
end if;

-- Parse JSON, XML, etc.
declare
   JSON : constant JSON_Value := Parse (Input_String);
exception
   when Parse_Error =>
      return Invalid_Input;
end;
```

### Business Rule Validation (Domain)

Business rules validation happens in Domain:

```ada
-- ✓ GOOD: Business rule validation in Domain
function Create (Name : String) return Result is
begin
   if Name'Length = 0 then
      return Failure (Empty_Person_Name);  -- Business rule
   elsif Name'Length > 100 then
      return Failure (Person_Name_Too_Long);  -- Business rule
   else
      return Success (Person_Name'(Value => Name));
   end if;
end Create;
```

**Key Distinction:**  
- **Presentation validates FORMAT** (is it valid JSON? Are required fields present?)
- **Domain validates BUSINESS RULES** (is the name too long? Does it meet business requirements?)

## Multiple Interface Types

### CLI Interface

```ada
package Hybrid.Presentation.CLI is
   procedure Run
      (Args   : Argument_Array;
       Output : in out Output_Port'Class)
      return Exit_Code;
end Hybrid.Presentation.CLI;
```

### HTTP API Interface (Example)

```ada
package Hybrid.Presentation.HTTP is
   procedure Handle_Request
      (Request  : HTTP_Request;
       Response : in out HTTP_Response;
       Service  : in out Create_Greeting_Service);
end Hybrid.Presentation.HTTP;
```

Implementation:
```ada
procedure Handle_Request
   (Request  : HTTP_Request;
    Response : in out HTTP_Response;
    Service  : in out Create_Greeting_Service) is

   Name_Param : constant String := Request.Get_Parameter ("name");
begin
   declare
      Name_Result : constant Result := Person_Name.Create (Name_Param);
   begin
      if Name_Result.Is_Success then
         Service.Execute (Name_Result.Get_Value, HTTP_Output);
         Response.Set_Status (200);
      else
         Response.Set_Status (400);  -- Bad Request
         Response.Set_Body ("Invalid name: " & Name_Result.Get_Error'Image);
      end if;
   end;

exception
   when E : others =>
      Response.Set_Status (500);  -- Internal Server Error
      Response.Set_Body ("Error: " & Exception_Message (E));
end Handle_Request;
```

### GUI Interface (Example)

```ada
package Hybrid.Presentation.GUI is
   procedure Handle_Button_Click
      (Name_Field : in out Text_Entry;
       Output     : in out Output_Port'Class);
end Hybrid.Presentation.GUI;
```

## Testing Presentation Layer

### CLI Tests

```ada
-- tests/unit/src/presentation/test_presentation_cli.adb

procedure Test_CLI_Valid_Input is
   Mock : aliased Mock_Output;
   Args : constant Argument_Array := (1 => To_Unbounded_String ("Alice"));
   Exit_Status : Exit_Code;
begin
   Exit_Status := CLI.Run (Args, Mock);

   Assert (Exit_Status = Success, "Should return success");
   Assert (Contains (Mock.Last_Message, "Alice"), "Should output greeting");
end Test_CLI_Valid_Input;

procedure Test_CLI_No_Arguments is
   Mock : aliased Mock_Output;
   Args : constant Argument_Array (1 .. 0) := (others => <>);
   Exit_Status : Exit_Code;
begin
   Exit_Status := CLI.Run (Args, Mock);

   Assert (Exit_Status = Invalid_Arguments, "Should return error");
end Test_CLI_No_Arguments;

procedure Test_CLI_Empty_Name is
   Mock : aliased Mock_Output;
   Args : constant Argument_Array := (1 => To_Unbounded_String (""));
   Exit_Status : Exit_Code;
begin
   Exit_Status := CLI.Run (Args, Mock);

   Assert (Exit_Status = Invalid_Input, "Should return invalid input");
end Test_CLI_Empty_Name;
```

## Design Patterns

### 1. Adapter Pattern (Input Adapters)

Presentation Layer acts as **input adapters**:

```
User Input → [Presentation] → Application Use Cases
```

Just like Infrastructure provides **output adapters**, Presentation provides **input adapters**.

### 2. Dependency Injection

Presentation depends on interfaces, not concrete types:

```ada
-- ✓ GOOD: Depends on interface
procedure Run
   (Args   : Argument_Array;
    Output : in out Output_Port'Class);  -- Interface!

-- ✗ BAD: Depends on concrete type
procedure Run
   (Args    : Argument_Array;
    Console : in out Console_Output);  -- Concrete type!
```

### 3. Error Translation

Convert between error domains:

```ada
Domain Error → Application Error → Exit Code / HTTP Status / GUI Message
```

## Common Mistakes to Avoid

### ❌ Don't: Put Business Logic in Presentation

```ada
-- WRONG: Business logic in Presentation
procedure Run (Args : Argument_Array; Output : in out Output_Port'Class) is
   Name : constant String := Args (1);
begin
   if Name'Length > 100 then  -- Business rule! Belongs in Domain
      Output.Write_Output ("Name too long");
      return Invalid_Input;
   end if;
end Run;
```

**Why?** Business rules belong in Domain, not Presentation.

### ❌ Don't: Depend on Infrastructure Directly

```ada
-- WRONG: Presentation depending on Infrastructure
with Hybrid.Infrastructure.Adapter.Console_Output;  -- NO!

procedure Run (Args : Argument_Array) is
   Console : Console_Output;  -- Concrete type!
begin
   Console.Write_Output ("...");
end Run;
```

**Why?** Presentation should depend on Application ports, not Infrastructure adapters.

### ❌ Don't: Depend on Domain Directly

```ada
-- WRONG: Calling Domain directly, skipping Application
with Hybrid.Domain.Service.Greeting;  -- NO! Go through Application

procedure Run (Args : Argument_Array) is
begin
   Result := Domain.Service.Greeting.Create_Greeting (...);  -- Skip Application!
end Run;
```

**Why?** Presentation should call Application use cases, which then call Domain.

### ❌ Don't: Mix Multiple Input Types

```ada
-- WRONG: Mixing CLI and HTTP in one package
package Presentation is
   procedure Handle_CLI (Args : Argument_Array);
   procedure Handle_HTTP (Request : HTTP_Request);
end Presentation;
```

**Why?** Separate concerns. One package per input type.

## User Experience Considerations

### 1. Clear Error Messages

```ada
-- ✗ BAD: Cryptic error
Ada.Text_IO.Put_Line ("Error: 2");

-- ✓ GOOD: Helpful error
Ada.Text_IO.Put_Line ("Error: Name cannot be empty. Please provide a name.");
Ada.Text_IO.Put_Line ("Example: hybrid-app Alice");
```

### 2. Helpful Usage Information

```ada
procedure Display_Usage is
begin
   Ada.Text_IO.Put_Line ("Usage: hybrid-app <name>");
   Ada.Text_IO.Put_Line ("");
   Ada.Text_IO.Put_Line ("Creates a personalized greeting.");
   Ada.Text_IO.Put_Line ("");
   Ada.Text_IO.Put_Line ("Arguments:");
   Ada.Text_IO.Put_Line ("  <name>   Name to greet (1-100 characters)");
   Ada.Text_IO.Put_Line ("");
   Ada.Text_IO.Put_Line ("Examples:");
   Ada.Text_IO.Put_Line ("  hybrid-app Alice");
   Ada.Text_IO.Put_Line ("  hybrid-app \"Bob Smith\"");
end Display_Usage;
```

### 3. Consistent Exit Codes

```ada
-- Use standard Unix conventions
Success           => 0
General Error     => 1
Invalid Usage     => 2
```

## Summary

The Presentation Layer is the **user interface boundary**:

- **Input adapters** - translate user actions to use cases
- **Depends only on Application** - never Infrastructure or Domain
- **Handles format validation** - parsing, required fields
- **Maps errors to user output** - exit codes, HTTP status, error messages
- **Multiple interfaces supported** - CLI, HTTP, GUI, etc.

**Remember**: Presentation translates between user world and application world.

## Next Steps

- **[Application Layer Guide](application-layer.md)** - Understand use cases and ports
- **[Bootstrap Guide](bootstrap-module.md)** - Learn how everything wires together
- **[Testing Guide](testing.md)** - Test user interfaces effectively

## References

- `presentation/` - Presentation layer source code
- `tests/unit/src/presentation/` - Presentation unit tests
