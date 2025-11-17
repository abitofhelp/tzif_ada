# TZif UML Diagrams

**Version:** 1.0.0-rc1  
**Date:** November 16, 2025  
**SPDX-License-Identifier:** BSD-3-Clause  
**License File:** See the LICENSE file in the project root.  
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.  
**Status:** Unreleased  

---

## Overview

This directory contains PlantUML diagrams documenting the TZif library architecture and use case flows. Each diagram is available in both source (`.puml`) and rendered (`.svg`) formats.

---

## v1.0.0 Use Case Sequence Diagrams

### [Discover Sources (Sequential)](discover_sources_sequential.svg)
Sequential discovery of timezone sources from dev-provided paths:
- Developer-specified search paths
- Recursive directory traversal
- Infinite loop protection (canonical paths + depth limit)
- Source validation and metadata collection
- [View Source](discover_sources_sequential.puml)

### [Find Zone By ID](find_zone_by_id.svg)
Look up timezone information by zone ID:
- Zone ID validation
- Repository query
- Zone retrieval
- Error handling
- [View Source](find_zone_by_id.puml)

### [Select Source and List Zones](select_source_and_list_zones.svg)
Select timezone source and enumerate available zones:
- Source selection
- Zone enumeration
- Zone listing
- [View Source](select_source_and_list_zones.puml)

### [Validate Source](validate_source.svg)
Validate timezone source directory structure:
- Directory existence checks
- File format validation
- Source metadata verification
- [View Source](validate_source.puml)

---

## Future Features (See Roadmap)

**Note**: The following diagrams represent potential future items under investigation. See [roadmap.md](../roadmap.md) for details.

### [Discover Sources (Parallel)](discover_sources_parallel.svg)
Parallel discovery of timezone sources from filesystem:
- Multi-threaded source scanning
- Platform-specific path detection
- Source validation
- [View Source](discover_sources_parallel.puml)

### [Export Cache](export_cache.svg)
Export timezone cache to persistent storage:
- Cache serialization
- Zone data export
- File I/O operations
- [View Source](export_cache.puml)

### [Import Cache](import_cache.svg)
Import timezone cache from persistent storage:
- Cache deserialization
- Zone data validation
- Cache reconstruction
- [View Source](import_cache.puml)

### [Startup With Cache](startup_with_cache.svg)
Application startup sequence using persisted cache:
- Cache initialization
- Repository setup
- Performance optimization
- [View Source](startup_with_cache.puml)

---

## Generating Diagrams

All diagrams are automatically regenerated during the release process from their `.puml` source files. To manually regenerate:

```bash
# Install PlantUML (if not already installed)
brew install plantuml  # macOS
# or
apt-get install plantuml  # Linux

# Generate all SVG diagrams
cd docs/diagrams
plantuml -tsvg *.puml
```

---

## Creating New Diagrams

1. Create a new `.puml` file in this directory
2. Follow PlantUML sequence diagram syntax
3. Run `plantuml -tsvg yourdiagram.puml`
4. Add entry to this index
5. Commit both `.puml` and `.svg` files

For PlantUML syntax reference, see: https://plantuml.com/sequence-diagram

---

## Related Documentation

- 📖 [Formal Documentation](../index.md) - SRS, SDS, Test Guide
- 📚 [Development Guides](../guides/index.md) - Architecture and implementation guides
- 📝 [Main README](../../README.md) - Project overview

---

For diagram improvements or new diagram requests, please file an issue on GitHub.
