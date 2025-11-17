# TZif Development Guides

**Version:** 1.0.0-rc1  
**Date:** November 16, 2025  
**SPDX-License-Identifier:** BSD-3-Clause  
**License File:** See the LICENSE file in the project root.  
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.  
**Status:** Unreleased  

---

## Overview

This directory contains development guides for understanding and working with the TZif library architecture, build system, and development practices.

---

## Architecture Guides

### 📚 [Hexagonal Architecture (Hybrid Architecture)](hybrid-architecture/index.md)
**Comprehensive guide to Hexagonal Architecture principles** - Start here for understanding the architectural foundations:
- Architecture overview and core principles
- Domain, Application, and Infrastructure layers
- Bootstrap and Presentation patterns (for library consumers)
- Complete with 16 visual diagrams
- Generic patterns applicable to any Ada 2022 project

### [Architecture Enforcement](architecture_enforcement.md)
How the hexagonal architecture is enforced and validated in TZif:
- Layer dependency rules
- Architecture validation tools
- Compile-time enforcement
- Common violations and how to avoid them

### [Architecture Mapping](architecture_mapping.md)
Complete mapping of TZif packages to architecture layers:
- Domain layer packages
- Application layer packages
- Infrastructure layer packages
- Cross-cutting concerns

### [Ports Mapping](ports_mapping.md)
Detailed guide to TZif inbound and outbound ports:
- Port definitions and responsibilities
- Port implementations
- Adapter pattern usage
- Use case orchestration

---

## Design and Implementation Guides

### [Error Handling Strategy](error_handling_strategy.md)
Comprehensive error handling approach:
- Railway-Oriented Programming
- Result monad patterns
- Error type design
- Error propagation best practices

---

## Development Process Guides

### [Build Profiles](build_profiles.md)
Build system configuration and profiles:
- Development vs release builds
- Optimization settings
- Platform-specific configurations
- Alire build profiles

### [Release Checklist](release_checklist.md)
Complete release preparation procedures:
- Version updates
- Documentation generation
- Testing requirements
- Release validation steps

---

## Related Documentation

- 📖 [Formal Documentation](../index.md) - SRS, SDS, Test Guide
- 🎨 [UML Diagrams](../diagrams/index.md) - Visual architecture documentation
- 📝 [Main README](../../README.md) - Project overview

---

## Contributing

When adding new guides:
1. Follow the naming convention: `TOPIC_NAME.md`
2. Add metadata header (version, date, copyright)
3. Link from this index
4. Keep focused on one topic
5. Include examples where applicable

For questions or improvements to these guides, please file an issue on GitHub.
