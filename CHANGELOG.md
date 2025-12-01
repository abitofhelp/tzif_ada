# Changelog

**Version:** 1.0.0  
**Date:** November 30, 2025  
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>  
**Status:** Released  

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.0.0] - 2025-11-29

### Added
- Initial library structure based on hybrid_app_ada
- 4-layer hexagonal architecture (Domain, Application, Infrastructure, API)
- Public API facade at `Tzif.API`
- Three-package API pattern (Operations, Desktop, facade)
- Generic I/O plugin pattern via `Tzif.API.Operations`
- Desktop platform instantiation via `Tzif.API.Desktop`
- Embedded safety restrictions in root package
- Result monad error handling via `functional` crate
- Person value object with bounded string validation
- Greet use case with dependency injection via generics
- Console writer adapter for desktop platforms
- Version package for runtime version queries
- Comprehensive Makefile for build automation
- 98 tests (88 unit + 10 integration)
- Comprehensive documentation (SRS, SDS, STG)
- 6 UML diagrams with SVG output
- Build profile support (standard, embedded, baremetal, STM32)

### Architecture
- Domain layer: Pure business logic with zero dependencies
- Application layer: Use cases, ports, commands
- Infrastructure layer: Adapters (Console_Writer)
- API layer: Public facade with platform-specific instantiations
- Library_Standalone mode with explicit Library_Interface
- Static dispatch via generics for zero-overhead DI
- SPARK_Mode boundaries defined for future formal verification

### Dependencies
- functional ^2.1.1 (Result/Option/Try monads)
