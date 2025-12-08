# Software Test Guide (STG)

**Version:** 2.0.0  
**Date:** December 07, 2025  
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** 2025 Michael Gardner, A Bit of Help, Inc.<br>  
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

---

## 2. Test Organization

### 2.1 Directory Structure

```
test/
  +-- unit/                      -- Unit tests (domain, value objects)
  |     +-- unit_runner.adb      -- Unit test runner
  |     +-- unit_tests.gpr       -- Unit test project file
  |     +-- test_bounded_vector.adb
  |     +-- test_option_combinators.adb
  |     +-- test_result_combinators.adb
  |     +-- test_timezone_lookup.adb
  |     +-- test_tzif_data.adb
  |     +-- test_zone_id.adb
  |     +-- test_value_object_accessors.adb
  |     +-- test_iana_releases.adb
  |     +-- test_zone_entity.adb
  |     +-- test_source_cache.adb
  |     +-- test_ulid.adb
  |     +-- test_version.adb
  |     +-- test_zone_cache.adb
  +-- integration/               -- Integration tests (cross-layer)
  |     +-- integration_runner.adb
  |     +-- integration_tests.gpr
  |     +-- test_api.adb
  |     +-- test_discover_sources.adb
  |     +-- test_find_by_id.adb
  |     +-- test_find_by_pattern.adb
  |     +-- test_find_by_regex.adb
  |     +-- test_find_by_region.adb
  |     +-- test_find_my_id.adb
  |     +-- test_get_transition_at_epoch.adb
  |     +-- test_get_version.adb
  |     +-- test_list_all_order_by_id.adb
  |     +-- test_load_source.adb
  |     +-- test_query_timezone_info.adb
  |     +-- test_tzif_parser_errors.adb
  |     +-- test_validate_source.adb
  |     +-- test_zone_repository_errors.adb
  +-- common/                    -- Shared test utilities
  +-- support/                   -- Test framework
  +-- data/                      -- Test fixtures
  +-- python/                    -- Python test scripts (submodule)
examples/
  +-- examples.gpr               -- Examples project file
  +-- find_by_id.adb
  +-- find_my_id.adb
  +-- find_by_pattern.adb
  +-- find_by_region.adb
  +-- find_by_regex.adb
  +-- get_transition_at_epoch.adb
  +-- list_all_zones.adb
  +-- discover_sources.adb
  +-- load_source.adb
  +-- validate_source.adb
  +-- get_version.adb
```

### 2.2 Test Categories

#### Unit Tests

Test individual domain components in isolation:
- Value object constructors and validators
- Result monad operations and combinators
- Option monad operations
- Bounded vector operations
- Parser logic (with byte arrays)
- ULID generation

#### Integration Tests

Test cross-layer interactions with real infrastructure:
- API operations end-to-end
- Filesystem I/O with real TZif files
- Parser with system timezone data
- Repository operations
- Error propagation across layers

#### Example Programs

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
   begin
      --  Act
      Result : constant Zone_Result := Find_By_Id (Make_Zone_Id (Input));

      --  Assert
      Test_Framework.Run_Test
        ("Find valid zone returns Ok",
         Is_Ok (Result));
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

### 4.3 Running Individual Test Files

```bash
# Build tests
cd test && alr build

# Run unit tests
./bin/unit_runner

# Run integration tests
./bin/integration_runner
```

### 4.4 Running Python Tests

```bash
# Architecture enforcement tests
make test-python
```

---

## 5. Unit Test Details

### 5.1 Test Files

| File | Tests | Description |
|------|-------|-------------|
| test_bounded_vector.adb | ~30 | Bounded vector operations |
| test_option_combinators.adb | ~25 | Option monad combinators |
| test_result_combinators.adb | ~40 | Result monad combinators |
| test_timezone_lookup.adb | ~20 | Timezone lookup service |
| test_tzif_data.adb | ~25 | TZif data structures |
| test_zone_id.adb | ~15 | Zone ID validation |
| test_value_object_accessors.adb | ~20 | Value object getters |
| test_iana_releases.adb | ~10 | IANA release metadata |
| test_zone_entity.adb | ~15 | Zone entity operations |
| test_source_cache.adb | ~10 | Source cache operations |
| test_ulid.adb | ~15 | ULID generation |
| test_version.adb | ~5 | Version queries |
| test_zone_cache.adb | ~10 | Zone cache operations |

### 5.2 Coverage Focus

- Constructor validation (valid and invalid inputs)
- Accessor correctness
- Combinator behavior (And_Then, Map, etc.)
- Edge cases (empty, boundary values)
- Error path verification

---

## 6. Integration Test Details

### 6.1 Test Files

| File | Tests | Description |
|------|-------|-------------|
| test_api.adb | ~15 | Public API operations |
| test_discover_sources.adb | ~10 | Source discovery |
| test_find_by_id.adb | ~15 | Zone lookup by ID |
| test_find_by_pattern.adb | ~12 | Pattern matching |
| test_find_by_regex.adb | ~15 | Regex search |
| test_find_by_region.adb | ~10 | Region filtering |
| test_find_my_id.adb | ~8 | Local timezone detection |
| test_get_transition_at_epoch.adb | ~12 | Transition queries |
| test_get_version.adb | ~8 | Version queries |
| test_list_all_order_by_id.adb | ~10 | Zone enumeration |
| test_load_source.adb | ~8 | Source loading |
| test_query_timezone_info.adb | ~10 | Timezone info queries |
| test_tzif_parser_errors.adb | ~29 | Parser error handling |
| test_validate_source.adb | ~8 | Source validation |
| test_zone_repository_errors.adb | ~14 | Repository error paths |

### 6.2 Fixtures

Integration tests use:
- System timezone data (`/usr/share/zoneinfo` or `/var/db/timezone/zoneinfo`)
- Test fixtures in `test/data/`

---

## 7. Example Programs

### 7.1 Program List

| Example | Primary Operation | Success Criteria |
|---------|-------------------|------------------|
| find_by_id | Find_By_Id | Finds America/Phoenix |
| find_my_id | Find_My_Id | Returns local zone |
| find_by_pattern | Find_By_Pattern | Matches substring |
| find_by_region | Find_By_Region | Filters by region |
| find_by_regex | Find_By_Regex | Matches regex |
| get_transition_at_epoch | Get_Transition_At_Epoch | Returns transition info |
| list_all_zones | List_All_Zones | Enumerates zones |
| discover_sources | Discover_Sources | Finds sources |
| load_source | Load_Source | Loads source |
| validate_source | Validate_Source | Validates path |
| get_version | Get_Version | Returns version |

### 7.2 Running Examples

```bash
# Build examples
make build-examples

# Run individual example
./bin/examples/find_by_id
./bin/examples/get_transition_at_epoch
```

---

## 8. Continuous Integration

### 8.1 CI Workflow

```yaml
# Simplified CI steps
- Build library
- Run unit tests
- Run integration tests
- Run examples
- Check style/warnings
```

### 8.2 Platform Matrix

| Platform | Status |
|----------|--------|
| Linux (Ubuntu) | Full CI |
| macOS | Full CI |
| Windows | Partial (unit tests) |

---

## 9. Test Maintenance

### 9.1 Adding New Tests

1. Create test file in appropriate directory
2. Import Test_Framework
3. Write test procedures following Arrange-Act-Assert
4. Add test procedure call to runner
5. Run and verify

### 9.2 Test Naming

- File: `test_<component>.adb`
- Procedure: Descriptive name of what is being tested
- Run_Test name: "Operation_Condition_ExpectedResult"

---

**Document Control:**
- Version: 1.0.0
- Last Updated: December 07, 2025
- Status: Released

**Change History:**

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0.0 | 2025-12-07 | Michael Gardner | Initial release |
