# Enterprise Starter Library with Hybrid DDD/Clean/Hexagonal Architecture

[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE) [![Ada](https://img.shields.io/badge/Ada-2022-blue.svg)](https://ada-lang.io)

**Version:** 1.0.0<br>
**Date:** November 29, 2025<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>  
**Status:** Released   

> A canonical Ada 2022 library demonstrating hexagonal architecture with functional error handling, SPARK-compatible design, and embedded-safe patterns.

## Overview

Hybrid Lib Ada is a demonstration library showcasing **hybrid DDD/Clean/Hexagonal architecture** with dependency inversion, ports & adapters, and Result monad error handling in Ada 2022. This is a library-only crate designed to be embedded in applications, with support for both desktop and embedded platforms.

## Features

- ✅ 4-layer hexagonal architecture (Domain, Application, Infrastructure, API)
- ✅ Public API facade with stable interface
- ✅ Generic I/O plugin pattern for platform portability
- ✅ Result monad error handling (via `functional` crate)
- ✅ Embedded safety restrictions (no implicit heap allocations)
- ✅ Static dispatch via generics (zero runtime overhead)
- ✅ Desktop platform support (Console I/O)
- ✅ Library_Standalone with explicit Library_Interface

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Tzif.API                       │
│              (Public Facade - Stable Interface)             │
├─────────────────────────────────────────────────────────────┤
│  API.Operations     │     API.Desktop     │   (API.Embedded)│
│  (Generic I/O)      │ (Console_Writer DI) │   (Future UART) │
├─────────────────────┼─────────────────────┼─────────────────┤
│                    Application Layer                        │
│     Use Cases  │  Ports (Writer)  │  Commands (Greet)       │
├─────────────────────────────────────────────────────────────┤
│                   Infrastructure Layer                      │
│              Adapters (Console_Writer)                      │
├─────────────────────────────────────────────────────────────┤
│                      Domain Layer                           │
│   Value Objects (Person) │ Errors │ Unit │ Result Monad    │
└─────────────────────────────────────────────────────────────┘
```

## Quick Start

### Building

```bash
# Build debug library
make build

# Build release library
make build-release

# Using Alire directly
alr build
```

### Using in Your Project

Add to your `alire.toml`:

```toml
[[depends-on]]
tzif = "*"
```

In your Ada code:

```ada
with Tzif.API;

procedure Main is
   use Tzif.API;

   --  Create a greet command
   Cmd : constant Greet_Command := Create_Greet_Command ("World");

   --  Execute the greeting operation
   Result : constant Unit_Result.Result := Greet (Cmd);
begin
   if Unit_Result.Is_Ok (Result) then
      --  Success! Message was printed to console
      null;
   else
      --  Handle error
      declare
         Err : constant Error_Type := Unit_Result.Error_Info (Result);
      begin
         --  Process error...
         null;
      end;
   end if;
end Main;
```

## Usage

### Creating a Person

```ada
with Tzif.API;

declare
   use Tzif.API;

   --  Create a person (validated)
   Person_Res : constant Person_Result.Result := Create_Person ("Alice");
begin
   if Person_Result.Is_Ok (Person_Res) then
      declare
         P : constant Person_Type := Person_Result.Value (Person_Res);
      begin
         --  Use the person
         Put_Line ("Name: " & Get_Name (P));
      end;
   end if;
end;
```

### Custom I/O Adapter

For embedded or custom platforms, instantiate `API.Operations` with your own writer:

```ada
with Tzif.API.Operations;
with Application.Port.Outbound.Writer;

--  Your custom writer function
function UART_Write (Message : String)
   return Application.Port.Outbound.Writer.Unit_Result.Result;

--  Instantiate operations with your writer
package My_Ops is new Tzif.API.Operations
  (Writer => UART_Write);
```

## Testing

```bash
# Run all tests (98 tests: 88 unit + 10 integration)
make test-all

# Build tests
make build-tests

# Run unit tests only
./test/bin/unit_runner

# Run integration tests only
./test/bin/integration_runner
```

## Documentation

- 📚 **[Documentation Index](docs/index.md)** - Full documentation
- 🚀 **[Quick Start Guide](docs/quick_start.md)** - Get started in minutes
- 🏗️ **[All About Our API](docs/guides/all_about_our_api.md)** - API architecture and platform customization
- 📋 **[Software Requirements](docs/formal/software_requirements_specification.md)** - Formal requirements
- 📐 **[Software Design](docs/formal/software_design_specification.md)** - Architecture details
- 🧪 **[Software Test Guide](docs/formal/software_test_guide.md)** - Testing strategy
- 📝 **[CHANGELOG](CHANGELOG.md)** - Release history

## Code Standards

This project follows:
- **Ada Agent** (`~/.claude/agents/ada.md`) - Ada 2022 standards
- **Architecture Agent** (`~/.claude/agents/architecture.md`) - DDD/Clean/Hexagonal
- **Functional Agent** (`~/.claude/agents/functional.md`) - Result/Option patterns
- **SPARK Agent** (`~/.claude/agents/spark.md`) - Embedded safety patterns

## Contributing

This project is not open to external contributions at this time.

## AI Assistance & Authorship

This project — including its source code, tests, documentation, and other deliverables — is designed, implemented, and maintained by human developers, with Michael Gardner as the Principal Software Engineer and project lead.

We use AI coding assistants (such as OpenAI GPT models and Anthropic Claude Code) as part of the development workflow to help with:

- drafting and refactoring code and tests,
- exploring design and implementation alternatives,
- generating or refining documentation and examples,
- and performing tedious and error-prone chores.

AI systems are treated as tools, not authors. All changes are reviewed, adapted, and integrated by the human maintainers, who remain fully responsible for the architecture, correctness, and licensing of this project.

## License

Copyright © 2025 Michael Gardner, A Bit of Help, Inc.

Licensed under the BSD-3-Clause License. See [LICENSE](LICENSE) for details.

## Author

Michael Gardner
A Bit of Help, Inc.
https://github.com/abitofhelp

## Project Status

**Status**: Released (v1.0.0)

- ✅ Core library structure
- ✅ 4-layer hexagonal architecture
- ✅ Public API facade with three-package pattern
- ✅ Desktop platform support (Console_Writer)
- ✅ Full test suite (98 tests)
- ✅ Comprehensive documentation
- ✅ SPARK_Mode boundaries defined
- ⬜ Embedded platform composition roots (documented, not yet implemented)
- ⬜ Alire publication
