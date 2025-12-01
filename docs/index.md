# Tzif Documentation

**Version:** 1.0.0  
**Date:** November 30, 2025  
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>  
**Status:** Released  

---

## Welcome

Welcome to the documentation for **Tzif**, a canonical Ada 2022 library demonstrating hexagonal architecture (DDD/Clean/Hex) with:

- **Functional error handling** via Result monad
- **SPARK-compatible design** for formal verification
- **Static dependency injection** via Ada generics
- **Embedded-safe patterns** (no heap allocation)

---

## Quick Navigation

### Getting Started

- **[Quick Start Guide](quick_start.md)** - Get up and running in minutes
- **[Build Profiles](guides/build_profiles.md)** - Profile configuration for different targets

### Formal Documentation

- **[Software Requirements Specification](formal/software_requirements_specification.md)** - What the library must do
- **[Software Design Specification](formal/software_design_specification.md)** - How the library is structured
- **[Software Test Guide](formal/software_test_guide.md)** - Testing strategy and execution

### Developer Guides

- **[All About Our API](guides/all_about_our_api.md)** - API layer architecture and usage

### Reference

- **[CHANGELOG](../CHANGELOG.md)** - Release history
- **[README](../README.md)** - Project overview

---

## Architecture Overview

Tzif follows a **4-layer library architecture**:

```
┌─────────────────────────────────────────────────────────┐
│                        API Layer                         │
│  Public facade + composition roots + SPARK operations   │
│  - Tzif.API (facade)                          │
│  - Tzif.API.Desktop (composition root)        │
│  - Tzif.API.Operations (SPARK-safe)           │
└─────────────────────────┬───────────────────────────────┘
                          │
┌─────────────────────────┼───────────────────────────────┐
│              Infrastructure Layer                        │
│  Concrete adapters implementing ports                    │
│  - Infrastructure.Adapter.Console_Writer                 │
└─────────────────────────┬───────────────────────────────┘
                          │
┌─────────────────────────┼───────────────────────────────┐
│               Application Layer                          │
│  Use cases, commands, ports                              │
│  - Application.Usecase.Greet                            │
│  - Application.Command.Greet                            │
│  - Application.Port.Outbound.Writer                     │
└─────────────────────────┬───────────────────────────────┘
                          │
┌─────────────────────────┼───────────────────────────────┐
│                 Domain Layer                             │
│  Pure business logic, value objects, errors              │
│  - Domain.Value_Object.Person                           │
│  - Domain.Error + Domain.Error.Result                   │
│  - Domain.Unit                                          │
└─────────────────────────────────────────────────────────┘
```

**Key Principles:**
- Dependencies flow **inward** (toward Domain)
- Domain has **zero dependencies**
- Infrastructure implements **ports** defined in Application
- API is the **public surface** for library consumers

---

## Test Summary

| Category | Tests | Status |
|----------|-------|--------|
| Unit Tests (Domain) | 41 | ✅ Pass |
| Unit Tests (Application) | 33 | ✅ Pass |
| Unit Tests (API.Operations) | 14 | ✅ Pass |
| Integration Tests (API) | 10 | ✅ Pass |
| **Total** | **98** | **✅ All Pass** |

Run tests: `make test-all`

---

## Quick Example

```ada
with Tzif.API;

procedure Main is
   use Tzif.API;

   Cmd    : constant Greet_Command := Create_Greet_Command ("World");
   Result : constant Unit_Result.Result := Greet (Cmd);
begin
   if Unit_Result.Is_Ok (Result) then
      --  Output: "Hello, World!"
      null;
   end if;
end Main;
```

---

## Document Conventions

- **MUST/SHALL** - Absolute requirement
- **SHOULD** - Recommended but not required
- **MAY** - Optional feature
- **Code blocks** - Can be copied and executed
- **Diagrams** - PlantUML source in `docs/diagrams/`

---

## Contributing to Documentation

Documentation follows the standards defined in the [Documentation Agent](~/.claude/agents/documentation.md):

- All files use metadata headers
- Code examples must compile and run
- Diagrams kept as .puml source + .svg rendered

---

## Need Help?

- Check the [Quick Start Guide](quick_start.md) for common issues
- Review the [Software Test Guide](formal/software_test_guide.md) for testing help
- See [All About Our API](guides/all_about_our_api.md) for API architecture details
