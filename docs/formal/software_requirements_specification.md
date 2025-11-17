# Software Requirements Specification (SRS)

**Project**: TZif - IANA Timezone Information Library for Ada 2022
**Version**: 1.0.0
**Date**: 2025-11-15
**Author**: Michael Gardner, A Bit of Help, Inc.
**Status**: Released

---

## 1. Introduction

### 1.1 Purpose

This Software Requirements Specification (SRS) describes the functional and non-functional requirements for TZif, a production-ready Ada 2022 library for parsing and querying IANA timezone information from TZif binary files.

### 1.2 Scope

TZif provides:
- Parsing of TZif binary format (versions 1, 2, and 3)
- Query operations for timezone data by ID, region, and pattern
- Timezone transition lookups for specific epochs
- Source discovery and validation
- Cache export/import functionality
- Thread-safe operations
- Railway-oriented error handling

### 1.3 Definitions and Acronyms

- **TZif**: Timezone Information Format (IANA standard binary format)
- **IANA**: Internet Assigned Numbers Authority
- **SRS**: Software Requirements Specification
- **API**: Application Programming Interface
- **UTC**: Coordinated Universal Time

### 1.4 References

- IANA Time Zone Database: https://www.iana.org/time-zones
- TZif Format Specification: RFC 8536
- Ada 2022 Language Reference Manual

---

## 2. Overall Description

### 2.1 Product Perspective

TZif is a standalone Ada library implementing hexagonal (ports and adapters) architecture with clean separation between domain logic, application use cases, and infrastructure adapters.

**Architecture Layers**:
- **Domain Layer**: Pure business logic, value objects, entities
- **Application Layer**: Use cases, ports (interfaces)
- **Infrastructure Layer**: Adapters for file system, parsing, caching

### 2.2 Product Features

1. **TZif Parsing**: Parse TZif v1, v2, v3 binary files
2. **Timezone Queries**: Find by ID, region, pattern, regex
3. **Transition Lookups**: Get timezone info for specific epoch
4. **Source Management**: Discover and validate timezone sources
5. **Caching**: Export/import zone caches for performance
6. **Error Handling**: Railway-oriented programming with Result monads

### 2.3 User Classes

- **Application Developers**: Integrate timezone functionality
- **System Administrators**: Configure timezone data sources
- **Library Maintainers**: Extend and maintain the codebase

### 2.4 Operating Environment

- **Platforms**: POSIX-compliant systems (Linux, macOS, BSD), Windows
- **Ada Compiler**: GNAT FSF 14.2+ or GNAT Pro 25.0+
- **Ada Version**: Ada 2022
- **Dependencies**: functional ^1.0.0 (Result/Option monads)

---

## 3. Functional Requirements

### 3.1 TZif Parsing (FR-01)

**Priority**: High
**Description**: Parse TZif binary files in all versions.

**Requirements**:
- FR-01.1: Parse TZif version 1 (legacy 32-bit)
- FR-01.2: Parse TZif version 2 (64-bit)
- FR-01.3: Parse TZif version 3 (with extensions)
- FR-01.4: Validate file format and magic numbers
- FR-01.5: Handle malformed files gracefully

### 3.2 Timezone Query Operations (FR-02)

**Priority**: High
**Description**: Provide query operations for timezone data.

**Requirements**:
- FR-02.1: Find timezone by exact ID (e.g., "America/New_York")
- FR-02.2: Find timezones by region (e.g., "America")
- FR-02.3: Find timezones by pattern matching
- FR-02.4: Find timezones by regex
- FR-02.5: List all available timezones
- FR-02.6: Get local timezone ID

### 3.3 Transition Lookups (FR-03)

**Priority**: High
**Description**: Retrieve timezone information for specific points in time.

**Requirements**:
- FR-03.1: Get transition info for given epoch seconds
- FR-03.2: Return UTC offset at specific time
- FR-03.3: Return timezone abbreviation (e.g., "PST", "PDT")
- FR-03.4: Handle times before/after transition data

### 3.4 Source Management (FR-04)

**Priority**: Medium
**Description**: Discover and validate timezone data sources.

**Requirements**:
- FR-04.1: Scan filesystem paths for timezone sources
- FR-04.2: Validate source directory structure
- FR-04.3: Check for required VERSION file
- FR-04.4: Count available zone files
- FR-04.5: Generate unique IDs for sources (ULID)

### 3.5 Cache Management (FR-05)

**Priority**: Medium
**Description**: Export and import zone caches for performance.

**Requirements**:
- FR-05.1: Export zone cache to JSON format
- FR-05.2: Import zone cache from JSON format
- FR-05.3: Validate cache integrity
- FR-05.4: Handle cache versioning

### 3.6 Error Handling (FR-06)

**Priority**: High
**Description**: Railway-oriented error handling without exceptions.

**Requirements**:
- FR-06.1: Use Result monad for all fallible operations
- FR-06.2: Provide descriptive error messages
- FR-06.3: Error codes for all failure modes
- FR-06.4: No exceptions in library code

---

## 4. Non-Functional Requirements

### 4.1 Performance (NFR-01)

- NFR-01.1: Parse TZif file in < 10ms
- NFR-01.2: Zone lookup in < 1ms (cached)
- NFR-01.3: Transition lookup in < 100μs

### 4.2 Reliability (NFR-02)

- NFR-02.1: Handle all malformed inputs gracefully
- NFR-02.2: No memory leaks
- NFR-02.3: Thread-safe repository operations

### 4.3 Portability (NFR-03)

- NFR-03.1: Support POSIX platforms (Linux, macOS, BSD)
- NFR-03.2: Support Windows
- NFR-03.3: No platform-specific code in domain/application layers

### 4.4 Maintainability (NFR-04)

- NFR-04.1: Hexagonal architecture with clear boundaries
- NFR-04.2: Comprehensive documentation (docstrings)
- NFR-04.3: > 90% test coverage
- NFR-04.4: Zero compiler warnings

### 4.5 Usability (NFR-05)

- NFR-05.1: Clear, intuitive API
- NFR-05.2: Working examples for all use cases
- NFR-05.3: Comprehensive error messages

---

## 5. System Requirements

### 5.1 Hardware Requirements

- **Minimum**:
  - CPU: Any modern processor
  - RAM: 64 MB
  - Disk: 10 MB

### 5.2 Software Requirements

- **Operating System**: Linux, macOS, BSD, or Windows
- **Compiler**: GNAT FSF 14.2+ or GNAT Pro 25.0+
- **Build System**: Alire 2.0+

---

## 6. Cache Management Requirements

### 6.1 Cache Architecture (REQ-CACHE-001)

The system SHALL maintain two separate in-memory caches:

**Source_Cache**: Maps ULID → Source_Info
- Stores metadata about discovered timezone sources (directories)
- Key: ULID (26-char unique identifier)
- Value: Source_Info (path, version, zone_count)

**Zone_Cache**: Maps (ULID, Zone_Id) → TZif_Data
- Stores parsed timezone data
- Key: Composite of Source ULID + Zone ID
- Value: Parsed TZif_Data (transitions, types, leap seconds, POSIX TZ)

**Rationale**: Separating source metadata from zone data allows:
- Efficient source validation without loading all zone data
- Tracking which zones came from which sources
- Removing invalid sources and their zones together

### 6.2 Export Format Structure (REQ-CACHE-002)

Export file SHALL use JSON format with the following structure:

```json
{
  "header": {
    "magic": "TZIF_CACHE",
    "version": 1,
    "created_at": "2025-01-09T12:34:56Z",
    "platform": "darwin",
    "library_version": "1.0.0"
  },
  "sources": [
    {
      "ulid": "01HQZX3J7K9M2N4P5Q6R7S8T9V",
      "path": "/usr/share/zoneinfo",
      "version": "2025b",
      "zone_count": 600
    }
  ],
  "zones": [
    {
      "source_ulid": "01HQZX3J7K9M2N4P5Q6R7S8T9V",
      "zone_id": "America/Los_Angeles",
      "tzif_data": { /* parsed data */ }
    }
  ]
}
```

**Fields**:
- `magic`: String literal "TZIF_CACHE" for file type identification
- `version`: Integer format version (start at 1)
- `created_at`: ISO 8601 timestamp
- `platform`: OS identifier (darwin/linux/windows)
- `library_version`: TZif library version that created the cache

### 6.3 Export Function (REQ-CACHE-003)

```ada
function Export_Cache
  (Path      : Path_String;
   Overwrite : Boolean := False)
  return Export_Cache_Result;
```

**Behavior**:
1. Collect all Source_Info from Source_Cache
2. Collect all (ULID, Zone_Id, TZif_Data) from Zone_Cache
3. Serialize to JSON format per REQ-CACHE-002
4. Write to file at `Path`
5. Return Export_Stats_Type with counts

**Return Value**: `Result[Export_Stats_Type]`
```ada
type Export_Stats_Type is record
   Sources_Exported : Natural := 0;
   Zones_Exported   : Natural := 0;
end record;
```

**Error Cases**:
- `Err(FileExists)` - File exists and Overwrite = False
- `Err(NotWritable)` - Insufficient permissions
- `Err(IOError)` - Filesystem error during write
- `Err(SerializationError)` - Failed to convert data to JSON

### 6.4 Import Validation (REQ-CACHE-004)

Import operation SHALL validate:

1. **File Existence**: Cache file exists at specified path
2. **JSON Validity**: File contains valid JSON
3. **Magic Header**: `header.magic` equals "TZIF_CACHE"
4. **Version Compatibility**: `header.version` is supported (currently: version 1)
5. **Source Paths**: Each source's filesystem path still exists
6. **Data Integrity**: All required fields present and correctly typed

**Source Validation Process**:
- For each source in cache file:
  - Check if `path` exists on current filesystem
  - If path missing: Mark source as REMOVED
  - If path exists: Mark source as VALID
- Only load zones from VALID sources
- Report removed source count in Import_Stats

### 6.5 Import Function (REQ-CACHE-005)

```ada
function Import_Cache
  (Path : Path_String)
  return Import_Cache_Result;
```

**Behavior**:
1. Read JSON file from `Path`
2. Parse and validate JSON structure
3. Validate header (magic, version)
4. Validate each source's filesystem path
5. Deserialize valid sources into Source_Cache
6. Deserialize zones (only from valid sources) into Zone_Cache
7. Return Import_Stats_Type with counts

**Return Value**: `Result[Import_Stats_Type]`
```ada
type Import_Stats_Type is record
   Sources_Loaded  : Natural := 0;  -- Successfully loaded
   Zones_Loaded    : Natural := 0;  -- Successfully loaded
   Sources_Removed : Natural := 0;  -- Paths no longer exist
end record;
```

**Error Cases**:
- `Err(NotFound)` - Cache file doesn't exist
- `Err(InvalidFormat)` - JSON parse error or wrong structure
- `Err(VersionMismatch)` - Unsupported cache format version
- `Err(IOError)` - Filesystem error during read
- `Err(DeserializationError)` - Failed to convert JSON to Ada types

### 6.6 Thread Safety (REQ-CACHE-006)

The cache SHALL support concurrent access with these patterns:
- **Multiple simultaneous reads**: Many tasks reading different zones concurrently
- **Reads during writes**: Reads continue while cache manager adds new entries
- **Cache updates**: Cache manager adds newly parsed zones without blocking readers

**Solution**: Protected object with entry-less functions for lock-free reads
```ada
protected type Zone_Cache is
   -- Lock-free read (no suspension)
   function Get (ULID : ULID_Type; Zone_Id : Zone_Id_Type)
      return Option[TZif_Data];

   -- Synchronized write (entries suspend during update)
   procedure Insert (ULID : ULID_Type; Zone_Id : Zone_Id_Type;
      Data : TZif_Data);

   -- Lock-free read for export
   function Get_All return Zone_Map;
private
   Cache : Zone_Map_Type;
end Zone_Cache;
```

### 6.7 Cache Behavior (REQ-CACHE-007)

**Lazy Loading**: Zone files parsed on first access, not at discovery

**Cache Purging**: Only 2 operations purge cache:
- `import_cache()`: Purges, then loads from file
- `discover_sources()`: Always purges (forces fresh directory scan)

**Cache Population**:
- Sources added by: `discover_sources()`, `load_source()`, `import_cache()`
- Zones added on-demand by: `find_by_id()`, `find_by_pattern()`, etc. (cache miss → parse → store)

**Cache Reads** (no writes):
- `export_cache()`: Dumps in-memory cache to file
- `validate_source()`: Read-only validation check

### 6.8 Performance Targets (REQ-CACHE-008)

- Cache hit lookup: ≤ 5 microseconds (p95)
- Cache miss + parse: ≤ 25 microseconds (p99)
- Target hit rate: ≥ 80%
- LRU eviction: O(1) insertion and lookup
- Import time: < 100ms for 600 zones

### 6.9 Design Decisions

**JSON vs Binary Format**: Use JSON format initially
- **Rationale**:
  - Human-readable for debugging and disaster recovery
  - Can inspect/edit cache files with standard tools (jq, text editors)
  - Performance measured with realistic data (~600 zones) before considering binary
  - Cache can always be regenerated from TZif files if corrupted
  - Easier to implement and debug during initial development

**JSON Library**: GNATCOLL.JSON (AdaCore)
- Industry standard from AdaCore
- Proven in production environments
- License compatible (GPL + GCC exception)
- Well-documented and maintained

---

## 7. Verification and Validation

### 7.1 Test Coverage

- Unit tests: 86 tests
- Integration tests: 118 tests
- Examples: 13 working examples

### 7.2 Verification Methods

- **Code Review**: All code reviewed before merge
- **Static Analysis**: Zero compiler warnings
- **Dynamic Testing**: All tests must pass
- **Coverage Analysis**: > 90% line coverage

---

## 8. Appendices

### 8.1 TZif Format Overview

TZif (Timezone Information Format) is a binary format defined by IANA for storing timezone data. The format includes:
- Header with version and counts
- Transition times
- Transition types
- Timezone abbreviations
- Leap second information
- Standard/wall indicators
- UTC/local indicators

### 8.2 Project Statistics

- Ada specification files: 85
- Ada implementation files: 26
- Total lines of code: ~15,000 (estimated)
- Architecture layers: domain, application, infrastructure

---

**Document Control**:
- Version: 1.0.0
- Last Updated: 2025-11-16
- Status: Released
- Copyright © 2025 Michael Gardner, A Bit of Help, Inc.
- License: BSD-3-Clause
