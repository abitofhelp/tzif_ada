# Software Test Guide (STG)

**Version:** 3.0.0<br>
**Date:** 2025-12-13<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

---

## 1. Introduction

### 1.1 Purpose

This Software Test Guide describes the testing strategy, test organization, and execution procedures for the TZif library.

### 1.2 Scope

This document covers:
- Test architecture and organization
- Unit, integration, and example tests
- Test execution commands
- Test framework usage
- SPARK formal verification
- Continuous integration

### 1.3 Test Summary

| Category | Test Count | Status |
|----------|------------|--------|
| Unit Tests | 424 | Passing |
| Integration Tests | 134 | Passing |
| Example Programs | 11 | Passing |
| **Total** | **569** | **All Passing** |

---

## 2. Test Organization

### 2.1 Directory Structure

```
test/
├── unit/                      # Unit tests (domain, value objects)
│   ├── unit_runner.adb       # Unit test runner
│   ├── unit_tests.gpr        # Unit test project file
│   ├── test_bounded_vector.adb
│   ├── test_iana_releases.adb
│   ├── test_option_combinators.adb
│   ├── test_result_combinators.adb
│   ├── test_source_cache.adb
│   ├── test_timezone_lookup.adb
│   ├── test_tzif_data.adb
│   ├── test_ulid.adb
│   ├── test_value_object_accessors.adb
│   ├── test_version.adb
│   ├── test_zone_cache.adb
│   ├── test_zone_entity.adb
│   └── test_zone_id.adb
├── integration/               # Integration tests (cross-layer)
│   ├── integration_runner.adb
│   ├── integration_tests.gpr
│   ├── test_api.adb
│   ├── test_api_callbacks.adb
│   ├── test_discover_sources.adb
│   ├── test_find_by_id.adb
│   ├── test_find_by_pattern.adb
│   ├── test_find_by_regex.adb
│   ├── test_find_by_region.adb
│   ├── test_find_my_id.adb
│   ├── test_get_transition_at_epoch.adb
│   ├── test_get_version.adb
│   ├── test_list_all_order_by_id.adb
│   ├── test_load_source.adb
│   ├── test_platform_stubs.adb
│   ├── test_query_timezone_info.adb
│   ├── test_validate_source.adb
│   ├── test_windows_platform.adb    # Windows-only
│   └── windows_platform_tests.gpr   # Windows GPR
├── common/                    # Shared test utilities
├── support/                   # Test framework
├── data/                      # Test fixtures
└── python/                    # Python test scripts (submodule)

examples/
├── examples.gpr               # Examples project file
├── discover_sources.adb
├── find_by_id.adb
├── find_by_pattern.adb
├── find_by_regex.adb
├── find_by_region.adb
├── find_my_id.adb
├── get_transition_at_epoch.adb
├── get_version.adb
├── list_all_zones.adb
├── load_source.adb
└── validate_source.adb
```

### 2.2 Test Categories

#### Unit Tests (424 tests)

Test individual domain components in isolation:
- Value object constructors and validators
- Result monad operations and combinators
- Option monad operations
- Bounded vector operations
- Parser logic (with byte arrays)
- Cache operations
- ULID generation

#### Integration Tests (134 tests)

Test cross-layer interactions with real infrastructure:
- API operations end-to-end
- Filesystem I/O with real TZif files
- Parser with system timezone data
- Repository operations
- Error propagation across layers
- Platform-specific operations

#### Example Programs (11 programs)

Demonstrate API usage and serve as acceptance tests:
- Each example exercises one primary operation
- Validates that public API works correctly
- Documents expected usage patterns

---

## 3. Test Framework

### 3.1 Test_Framework Package

Tests use a custom lightweight framework in `test/support/`:

```ada
package Test_Framework is
   procedure Reset;
   procedure Run_Test (Name : String; Passed : Boolean);
   procedure Register_Results (Total : Natural; Passed : Natural);
   function Grand_Total_Tests return Natural;
   function Grand_Total_Passed return Natural;
   function Print_Category_Summary (...) return Integer;
end Test_Framework;
```

### 3.2 Test Pattern

```ada
procedure Test_Something is
begin
   --  Arrange
   declare
      Input : constant String := "America/New_York";
      Result : Zone_Id_Result;
   begin
      --  Act
      Result := Make_Zone_Id (Input);

      --  Assert
      Test_Framework.Run_Test
        ("Make_Zone_Id with valid input returns Ok",
         Is_Ok (Result));

      if Is_Ok (Result) then
         Test_Framework.Run_Test
           ("Zone_Id has correct value",
            To_String (Value (Result)) = Input);
      end if;
   end;
end Test_Something;
```

---

## 4. Test Execution

### 4.1 Running All Tests

```bash
make test-all
```

Runs unit tests, integration tests, and examples in sequence.

### 4.2 Running Specific Suites

```bash
# Unit tests only
make test-unit

# Integration tests only
make test-integration

# Examples only
make test-examples
```

### 4.3 Running Individual Test Runners

```bash
# Unit tests
./test/bin/unit_runner

# Integration tests
./test/bin/integration_runner

# Examples (individual)
./bin/examples/find_by_id
./bin/examples/get_transition_at_epoch
```

### 4.4 Running Python Tests

```bash
# Architecture enforcement tests
make test-python

# Or directly
python3 -m pytest test/python/ -v
```

### 4.5 Running SPARK Verification

```bash
# SPARK legality check
make spark-check

# SPARK proof
make spark-prove
```

---

## 5. Unit Test Details

### 5.1 Test Files

| File | Description |
|------|-------------|
| test_bounded_vector.adb | Bounded vector operations, capacity, indexing |
| test_iana_releases.adb | IANA release metadata lookup |
| test_option_combinators.adb | Option monad (Some/None, Map, And_Then) |
| test_result_combinators.adb | Result monad (Ok/Error, combinators) |
| test_source_cache.adb | Source cache add, find, eviction |
| test_timezone_lookup.adb | Timezone lookup service |
| test_tzif_data.adb | TZif data structure operations |
| test_ulid.adb | ULID generation and ordering |
| test_value_object_accessors.adb | Value object getters |
| test_version.adb | Library version queries |
| test_zone_cache.adb | Zone cache operations |
| test_zone_entity.adb | Zone entity operations |
| test_zone_id.adb | Zone ID validation, formatting |

### 5.2 Coverage Focus

- Constructor validation (valid and invalid inputs)
- Accessor correctness
- Combinator behavior (And_Then, Map, Map_Error, Or_Else)
- Edge cases (empty strings, boundary values, max capacity)
- Error path verification (Validation_Error, Parse_Error)

---

## 6. Integration Test Details

### 6.1 Test Files

| File | Description |
|------|-------------|
| test_api.adb | Public API facade operations |
| test_api_callbacks.adb | Callback-based API operations |
| test_discover_sources.adb | Source discovery on filesystem |
| test_find_by_id.adb | Zone lookup by exact ID |
| test_find_by_pattern.adb | Pattern matching search |
| test_find_by_regex.adb | Regex-based search |
| test_find_by_region.adb | Geographic region filtering |
| test_find_my_id.adb | Local timezone detection |
| test_get_transition_at_epoch.adb | Transition queries at epoch |
| test_get_version.adb | Database version queries |
| test_list_all_order_by_id.adb | Zone enumeration (asc/desc) |
| test_load_source.adb | Source loading operations |
| test_platform_stubs.adb | Platform adapter stubs |
| test_query_timezone_info.adb | Timezone info queries |
| test_validate_source.adb | Source validation |
| test_windows_platform.adb | Windows platform tests (Win32 API) |

### 6.2 Test Fixtures

Integration tests use:
- System timezone data (platform-specific paths)
- Test fixtures in `test/data/`

| Platform | Timezone Data Path |
|----------|-------------------|
| Linux | `/usr/share/zoneinfo` |
| macOS | `/var/db/timezone/zoneinfo` |
| BSD | `/usr/share/zoneinfo` |
| Windows | `TZIF_DATA_PATH` environment variable |

---

## 7. Example Programs

### 7.1 Program List

| Example | Primary Operation | Success Criteria |
|---------|-------------------|------------------|
| discover_sources | Discover_Sources | Finds timezone sources |
| find_by_id | Find_By_Id | Finds America/Phoenix |
| find_by_pattern | Find_By_Pattern | Matches substring |
| find_by_region | Find_By_Region | Filters by region |
| find_by_regex | Find_By_Regex | Matches regex |
| find_my_id | Find_My_Id | Returns local zone |
| get_transition_at_epoch | Get_Transition_At_Epoch | Returns transition info |
| get_version | Get_Version | Returns version |
| list_all_zones | List_All_Zones | Enumerates zones |
| load_source | Load_Source | Loads source |
| validate_source | Validate_Source | Validates path |

### 7.2 Running Examples

```bash
# Build examples
make build-examples

# Run individual example
./bin/examples/find_by_id
./bin/examples/get_transition_at_epoch
./bin/examples/discover_sources
```

---

## 8. SPARK Formal Verification

### 8.1 Verification Scope

| Layer | SPARK_Mode | Status |
|-------|-----------|--------|
| Domain | On | Verified |
| Application | On | Verified |
| Infrastructure | Off | Not verified (I/O) |
| API | Off | Not verified (facade) |

### 8.2 SPARK Statistics

| Metric | Count |
|--------|-------|
| Total VCs | 1350 |
| Proved | 1179 |
| Unproved | 171 |
| **Proof Rate** | **87%** |

### 8.3 Running SPARK Analysis

```bash
# Legality check only
make spark-check

# Full proof analysis
make spark-prove
```

### 8.4 Unproved VCs

The 171 unproved VCs are in generic instantiation sites within bounded container operations. These represent:
- Loop invariants in generic Bounded_Vector operations
- Range checks on generic index types
- Precondition propagation through generics

These are not logic errors but limitations of SPARK prover with complex generics.

---

## 9. Continuous Integration

### 9.1 CI Workflows

| Workflow | Trigger | Tests |
|----------|---------|-------|
| ci.yml | Push, PR | Unit, Integration, Examples |
| windows-ci.yml | Push, PR | Windows platform tests |
| windows-release.yml | Manual | Windows release validation |

### 9.2 CI Steps

```yaml
- Build library
- Run unit tests (424)
- Run integration tests (134)
- Run examples (11)
- Check style/warnings (zero warnings)
- SPARK legality check
```

### 9.3 Platform Matrix

| Platform | CI Status | Test Coverage |
|----------|-----------|---------------|
| Linux (Ubuntu) | Full CI | All tests |
| macOS | Full CI | All tests |
| Windows | Full CI | Platform tests + validation |

---

## 10. Test Maintenance

### 10.1 Adding New Tests

1. Create test file in appropriate directory (`test/unit/` or `test/integration/`)
2. Import `Test_Framework`
3. Write test procedures following Arrange-Act-Assert pattern
4. Add test procedure call to runner (unit_runner.adb or integration_runner.adb)
5. Update GPR file if new file added
6. Run and verify

### 10.2 Test Naming Conventions

| Element | Convention | Example |
|---------|------------|---------|
| File | `test_<component>.adb` | `test_zone_id.adb` |
| Procedure | Descriptive name | `Test_Valid_Zone_Id_Construction` |
| Run_Test name | "Action_Condition_Expected" | "Make_Zone_Id_Valid_ReturnsOk" |

### 10.3 Test Patterns

**Value Object Tests:**
```ada
--  Test valid construction
Test_Framework.Run_Test
  ("Make_Zone_Id with valid returns Ok",
   Is_Ok (Make_Zone_Id ("America/New_York")));

--  Test invalid construction
Test_Framework.Run_Test
  ("Make_Zone_Id with empty returns Validation_Error",
   Is_Error (Make_Zone_Id ("")) and then
   Error_Info (Make_Zone_Id ("")).Kind = Validation_Error);
```

**Integration Tests:**
```ada
--  Test end-to-end operation
Result := Find_By_Id (Zone_Id);
Test_Framework.Run_Test
  ("Find_By_Id returns zone data",
   Is_Ok (Result) and then
   To_String (Value (Result).Id) = "America/Phoenix");
```

---

## 11. Appendices

### 11.1 Make Targets

| Target | Description |
|--------|-------------|
| `test-all` | Run all tests (unit + integration + examples) |
| `test-unit` | Run unit tests only |
| `test-integration` | Run integration tests only |
| `test-examples` | Run example programs |
| `test-python` | Run Python tests |
| `test-windows` | Run Windows platform tests |
| `spark-check` | SPARK legality check |
| `spark-prove` | SPARK proof analysis |

### 11.2 Test Statistics Summary

| Metric | Value |
|--------|-------|
| Total test files | 28 |
| Unit test files | 14 |
| Integration test files | 14 |
| Example programs | 11 |
| Total tests | **569** |
| Unit tests | 424 |
| Integration tests | 134 |
| Example tests | 11 |

---

**Document Control:**
- Version: 99.99.99
- Last Updated: 2025-12-13
- Status: Released

**Change History:**

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 3.0.0 | 2025-12-13 | Michael Gardner | Complete regeneration for v3.0.0; updated test counts (424 unit + 134 integration + 11 examples = 569 total); added SPARK verification section; updated Windows CI status to full |
| 1.0.0 | 2025-12-07 | Michael Gardner | Initial release |
