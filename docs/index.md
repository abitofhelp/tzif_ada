# TZif Documentation Index

**Version:** 1.0.0-rc1  
**Date:** November 16, 2025  
**SPDX-License-Identifier:** BSD-3-Clause  
**License File:** See the LICENSE file in the project root.  
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.  
**Status:** Unreleased  

---

## Quick Start

🚀 **[Quick Start Guide](quick_start.md)** - Get up and running with TZif in minutes

---

## Formal Documentation

Comprehensive specifications and design documents for the TZif library:

### [Software Requirements Specification (SRS)](formal/software_requirements_specification.md)
Complete requirements documentation including:
- 13 functional requirements (Find By ID, Find My ID, Get Transition, List All, Find By Pattern, Find By Region, Find By Regex, Get Version, Discover Sources, Load Source, Validate Source, Import Cache, Export Cache)
- 5 non-functional requirements (Performance, Memory, Portability, Error Handling, Testing)
- Cache management requirements (REQ-CACHE-001 through REQ-CACHE-009)
- System constraints and dependencies
- Test coverage requirements (217 tests - 100% passing)

### [Software Design Specification (SDS)](formal/software_design_specification.md)
Detailed design documentation covering:
- Hexagonal architecture (Ports and Adapters pattern)
- Layer organization and responsibilities (Domain/Application/Infrastructure)
- Design patterns (Railway-Oriented Programming, Repository, Adapter, I/O Plugin)
- SPARK-compatible I/O plugin architecture with formal packages
- Data flow and error handling (Result monad, no exceptions)
- Concurrency design (thread-safe operations)

### [Software Test Guide](formal/software_test_guide.md)
Complete testing documentation including:
- Test strategy, infrastructure, and organization
- Running tests (unit, integration, API validation, full-scale)
- Writing new tests with Test_Framework utilities
- Test coverage analysis procedures
- 217 total tests: 118 integration + 86 unit + 13 examples (100% passing)

---

## Development Guides

Detailed guides for developers:

📁 **[Development Guides](guides/index.md)** - Architecture, implementation, and development practices

**Key Guides:**
- **Architecture Guides** - Hexagonal/Clean architecture, layers, SPARK I/O plugins
- **Error Handling Strategy** - Result monad pattern, railway-oriented programming
- **Build Profiles** - Standard, embedded, concurrent, bare metal, STM32 profiles
- **Ports & Adapters** - Generic-based port pattern (vs inheritance)
- **Architecture Enforcement** - Automated layer dependency validation

---

## Visual Documentation

Architecture and sequence diagrams:

📁 **[UML Diagrams](diagrams/index.md)** - PlantUML diagrams showing system architecture and data flows

**Available Diagrams:**  
- Find Zone By ID - Core use case flow
- Discover Sources (Parallel) - Multi-source discovery with concurrency
- Import/Export Cache - Cache serialization flows
- Select Source and List Zones - Source selection with zone enumeration
- Startup with Cache - Application initialization flow
- Validate Source - Source integrity verification

---

## Quick Links

- 📖 [Main README](../README.md) - Project overview and installation
- 📝 [CHANGELOG](../CHANGELOG.md) - Release history and changes (v1.0.0)
- 🗺️ [ROADMAP](roadmap.md) - Future development plans
- 📋 [Examples](../examples/) - 13 working code examples
- 🧪 [Tests](../test/) - Complete test suite (217 tests, 100% passing)
- 🔧 [Makefile](../Makefile) - Build automation (build, test, coverage, examples)

---

## Project Status

**Version:** 1.0.0-rc1 - Production Ready  
**Release Date:** November 16, 2025  
**Status:** ✅ Production Release  

**Highlights:**
- ✅ All 217 tests passing (118 integration + 86 unit + 13 examples)
- ✅ All 13 examples building and running
- ✅ Zero compiler warnings
- ✅ Zero style violations
- ✅ SPARK-compatible I/O plugin architecture
- ✅ Comprehensive documentation
- ✅ Ready for production use

---

## Documentation Updates

All documentation is maintained for each release:
- Ada source file docstrings follow Ada 2022 standards
- Formal documentation reflects current architecture
- Guide metadata updated with current version/date
- Diagrams maintained in PlantUML format (.puml + .svg)

For documentation issues or suggestions, please file an issue on GitHub.

---

## Support

- 📧 **Email:** support@abitofhelp.com
- 🐛 **Issues:** [GitHub Issues](https://github.com/abitofhelp/tzif/issues)
- 📖 **Documentation:** This directory
- 💬 **Discussions:** [GitHub Discussions](https://github.com/abitofhelp/tzif/discussions)
