# Software Test Guide

**Project**: TZif - IANA Timezone Information Library for Ada 2022
**Version**: 1.0.0
**Date**: 2025-11-16
**Author**: Michael Gardner, A Bit of Help, Inc.
**Status**: Released

---

## 1. Introduction

### 1.1 Purpose

This Software Test Guide describes the testing approach, test organization, and procedures for the TZif library.

### 1.2 Scope

This document covers:
- Test strategy and levels
- Test organization
- Running tests
- Writing new tests
- Test coverage analysis

---

## 2. Test Strategy

### 2.1 Testing Levels

**Unit Tests**:
- Test individual packages in isolation
- Mock dependencies via test spies
- Focus on domain and value objects
- Count: 86 tests

**Integration Tests**:
- Test full stack with real data
- Test use cases end-to-end
- Test error conditions and edge cases
- Count: 118 tests

**Examples as Tests**:
- Working examples that demonstrate usage
- Validate real-world scenarios
- Count: 13 examples

**Total**: 217 tests (86 unit + 118 integration + 13 examples)

### 2.2 Testing Approach

- **Test-Driven**: Tests written alongside or before code
- **Railway-Oriented**: Test both success and error paths
- **Comprehensive**: Cover normal, edge, and error cases
- **Automated**: All tests runnable via `make test-all`

---

## 3. Test Organization

### 3.1 Directory Structure

```
test/
├── unit/              # Unit tests
│   ├── test_zone_id.adb
│   ├── test_iana_releases.adb
│   └── unit_runner.adb
├── integration/       # Integration tests
│   ├── test_find_by_id.adb
│   ├── test_discover_sources.adb
│   └── integration_runner.adb
├── support/           # Test utilities
│   └── test_spies/    # Test spies for ports
└── common/            # Shared test framework
    └── test_framework.ads
```

### 3.2 Test Naming Convention

- **Pattern**: `test_<component>.adb`
- **Example**: `test_zone_id.adb` tests `Domain.Value_Object.Zone_Id`
- **Runner**: Each level has a runner executable

---

## 4. Running Tests

### 4.1 Quick Start

```bash
# Run all tests (unit + integration)
make test-all

# Run only unit tests
make test-unit

# Run only integration tests
make test-integration

# Run with coverage
make test-coverage
```

### 4.2 Individual Test Execution

```bash
# Run specific unit test
./test/bin/test_zone_id

# Run specific integration test
./test/bin/test_find_by_id
```

### 4.3 Test Output

**Success**:
```
Test: Zone ID Creation
  [PASS] Make_Zone_Id creates valid zone
  [PASS] Zone ID length is correct
====================================================
  Results: 2 / 2 passed
  Status: ALL TESTS PASSED
====================================================
```

**Failure**:
```
Test: Zone ID Creation
  [PASS] Make_Zone_Id creates valid zone
  [FAIL] Zone ID length is incorrect
====================================================
  Results: 1 / 2 passed
  Status: TESTS FAILED
====================================================
```

---

## 5. Writing Tests

### 5.1 Unit Test Structure

```ada
pragma Ada_2022;
with Test_Framework;
with Domain.Value_Object.Zone_Id;

procedure Test_Zone_Id is
   use Domain.Value_Object.Zone_Id;
   use Test_Framework;

   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;

   procedure Assert(Condition : Boolean; Test_Name : String) is
   begin
      Test_Count := Test_Count + 1;
      if Condition then
         Pass_Count := Pass_Count + 1;
         Put_Line("  [PASS] " & Test_Name);
      else
         Put_Line("  [FAIL] " & Test_Name);
      end if;
   end Assert;

begin
   Put_Line("Test: Zone ID Creation");

   -- Test case
   declare
      Zone : constant Zone_Id_Type := Make_Zone_Id("America/New_York");
   begin
      Assert(To_String(Zone) = "America/New_York",
             "Make_Zone_Id creates valid zone");
   end;

   -- Final summary
   Report_Results(Test_Count, Pass_Count);
end Test_Zone_Id;
```

### 5.2 Integration Test Structure

```ada
pragma Ada_2022;
with Test_Framework;
with Application.Usecase.Find_By_Id;
with Infrastructure.Adapter.File_System.Repository;

procedure Test_Find_By_Id is
   use Test_Framework;

begin
   Put_Line("Test: Find By ID - Basic Lookup");

   -- Setup real repository
   declare
      Repo : Infrastructure.Adapter.File_System.Repository.Repository_Type;
      UC   : Application.Usecase.Find_By_Id.Use_Case_Type(Repo'Access);
      Result : constant Find_By_Id_Result := UC.Execute("America/New_York");
   begin
      Assert(Result.Is_Ok, "Should find valid zone");
      Assert(Result.Value.Get_Id = "America/New_York", "Zone ID matches");
   end;

   Report_Results(Test_Count, Pass_Count);
end Test_Find_By_Id;
```

### 5.3 Using Test Spies

```ada
-- Test use case without real infrastructure
with Test_Spies.Find_By_Id_Spy;

procedure Test_Use_Case is
   Spy : Test_Spies.Find_By_Id_Spy.Spy_Type;
   UC  : Application.Usecase.SomeCase.Use_Case_Type(Spy'Access);
begin
   -- Execute use case
   UC.Execute(...);

   -- Verify spy was called correctly
   Assert(Spy.Was_Called, "Repository was called");
   Assert(Spy.Call_Count = 1, "Called exactly once");
end Test_Use_Case;
```

---

## 6. Test Coverage

### 6.1 Coverage Goals

- **Target**: > 90% line coverage
- **Critical Code**: 100% coverage for error handling
- **Domain Layer**: Near 100% coverage

### 6.2 Running Coverage Analysis

```bash
# Generate coverage report
make test-coverage

# View HTML report
open coverage/report/index.html
```

### 6.3 Coverage Reports

Coverage reports show:
- Lines executed vs total
- Branches taken
- Functions covered
- Per-file statistics

---

## 7. Test Data

### 7.1 Test Timezone Data

**Location**: `/usr/share/zoneinfo` (system default)

**Test Zones Used**:
- `America/New_York`: Standard US timezone
- `Europe/London`: European timezone with DST
- `UTC`: Special timezone with no transitions
- `America/Los_Angeles`: West coast timezone

### 7.2 Test Cache Data

Test caches are generated during tests and cleaned up after.

---

## 8. Continuous Integration

### 8.1 CI Pipeline

```yaml
steps:
  - name: Build
    run: alr build

  - name: Unit Tests
    run: make test-unit

  - name: Integration Tests
    run: make test-integration

  - name: Coverage
    run: make test-coverage

  - name: Examples
    run: make test-examples
```

### 8.2 Success Criteria

All must pass:
- ✅ Zero build warnings
- ✅ All unit tests pass
- ✅ All integration tests pass
- ✅ All examples execute successfully
- ✅ Coverage > 90%

---

## 9. Test Maintenance

### 9.1 Adding New Tests

1. Create test file: `test/unit/test_<component>.adb`
2. Write test procedure
3. Add to runner if needed
4. Update Makefile if required

### 9.2 Updating Tests

- Update tests when requirements change
- Keep tests in sync with code
- Refactor tests alongside code

### 9.3 Test Documentation

- Document test purpose in header
- Comment complex test scenarios
- Explain expected vs actual behavior

---

## 10. Test Infrastructure

### 10.1 Overview

Comprehensive test suite for the TZif timezone library targeting 95%+ code coverage using AUnit framework.

### 10.2 Test Philosophy

- **Integration tests first** - Test real TZif file parsing with actual timezone data
- **Unit tests for calculations** - Domain services and value object operations
- **Minimal mocking** - Use real filesystem adapter with test data
- **AUnit framework** - Ada's standard testing framework
- **Coverage target: 95%+**

### 10.3 Test Structure

```
test/
├── unit/                          # Unit tests (isolated, no I/O)
│   ├── test_*.ads/adb            # Individual test suites
│   ├── test_suite_unit.ads/adb   # Test suite aggregator
│   ├── unit_runner.adb           # Test runner
│   └── unit_tests.gpr            # GPR file
│
├── integration/                   # Integration tests (real files, parser + services)
│   ├── test_*.adb                # Integration test files
│   ├── integration_runner.adb    # Test runner
│   └── integration_tests.gpr     # GPR file
│
└── fixtures/                      # Test data (real TZif files)
    └── zoneinfo/                  # Minimal test database
        ├── +VERSION               # Version file
        ├── America/
        │   ├── New_York           # Real TZif file
        │   ├── Los_Angeles
        │   └── Phoenix
        ├── Europe/
        │   ├── London
        │   └── Paris
        └── UTC                    # Simple zone
```

### 10.4 Test Coverage Breakdown

**Domain Layer (Unit Tests)** - Pure logic
- Zone_Id, Source_Info, Epoch_Seconds validation
- Result monad operations
- Value object invariants

**Infrastructure Repository (Integration Tests)** - Real I/O
- All repository functions with real TZif files
- Parse TZif v1/v2/v3 formats
- Handle missing files (ZoneNotFound errors)
- Handle corrupted files (ParseError errors)

**Use Cases (Integration)** - End-to-end
- Each use case tested via integration tests
- Full stack validation with real data

**Error Paths** - Comprehensive
- File not found scenarios
- Parse errors with malformed TZif data
- Invalid zone IDs
- Missing version files
- Cache import/export failures

### 10.5 Running Tests

**Build Tests:**
```bash
make build-tests
```

**Run All Tests:**
```bash
make test-all
```

**Run Specific Suites:**
```bash
make test-unit         # Unit tests only (86 tests)
make test-integration  # Integration tests only (118 tests)
make test-examples     # Example E2E tests (13 tests)
```

**With Coverage:**
```bash
make test-coverage
```

### 10.6 Coverage Goals

- **95%+ Overall Coverage**
  - Domain layer: 100% (pure logic, fully testable)
  - Application layer: 95%+ (use cases are thin wrappers)
  - Infrastructure layer: 90%+ (adapter code, error paths)

### 10.7 Test Data Requirements

**Fixtures Needed:**
- Real TZif files from system (`/usr/share/zoneinfo`)
- Test version file (`+VERSION`)
- Minimal zone set: UTC, America/New_York, America/Los_Angeles, America/Phoenix, Europe/London, Europe/Paris
- Malformed TZif file for parse error tests
- Empty directory for negative tests

**Environment:**
- Tests use system timezone database if available
- Falls back to test fixtures if system DB not found
- Tests are deterministic (no time-dependent behavior)

---

## 11. Test Organization

### 11.1 Directory Structure

The TZif library follows Ada testing best practices with proper separation of unit, integration, and E2E tests using AUnit framework.

**Framework**: AUnit 24.0.0+
**Coverage Target**: 90%+

### 11.2 Test Types

**Unit Tests** (`test/unit/`)

**Purpose**: Test individual components in isolation with no external dependencies

**Characteristics**:
- ✅ No file I/O
- ✅ No database access
- ✅ Uses constructed test data
- ✅ Fast execution (< 1 second)
- ✅ AUnit framework
- ✅ 90%+ code coverage target

**Integration Tests** (`test/integration/`)

**Purpose**: Test multiple components working together with real I/O

**Characteristics**:
- ✅ Reads real TZif files from filesystem
- ✅ Tests Infrastructure.TZif_Parser
- ✅ Tests Domain.Service.Timezone_Lookup with real data
- ✅ Validates against system timezone files
- ✅ Moderate execution time (1-5 seconds)
- ✅ Custom test framework with assertions

**E2E Tests** (`test/e2e/`)

**Purpose**: Test complete workflows from user perspective

**Implementation**: Example programs serve as E2E tests
- 13 working examples demonstrating all API operations
- Each example tests a specific use case
- Executed via `make test-examples`

### 11.3 Test Execution Strategy

**Fast Suite (PRs, Development)**
```bash
# Unit tests only - runs in < 1 second
make test-unit
```

**Full Suite (Pre-merge, CI)**
```bash
# All tests
make test-all
```

### 11.4 Test Naming Conventions

**Files:**
- Unit tests: `test_<component>_unit.ads/adb`
- Integration tests: `test_<feature>.adb`
- Test suites: `test_suite_<type>.ads/adb`
- Runners: `<type>_runner.adb`

**Procedures:**
- AUnit tests: `Test_<What>_<Scenario>`
- Integration tests: `Test_<Feature>`

**Examples:**
```ada
-- Unit test procedure
procedure Test_Find_Offset_No_Transitions;

-- Integration test procedure
procedure Test_Los_Angeles;
```

### 11.5 Mocking Strategy

**Never Mock:**
- Domain layer (pure business logic)
- Value objects
- Domain services

**Mock Only At Boundaries:**
- File I/O (for unit tests)
- External APIs (future)
- Time/clock (for deterministic tests)

**Current Approach:**
- Unit tests: Construct test data in memory
- Integration tests: Use real files from filesystem
- No mocks needed (domain is pure)

### 11.6 Adding New Tests

**Unit Test Checklist:**
1. Create test case file: `test/unit/test_<component>.adb`
2. Implement test procedures with AUnit.Test_Cases.Test_Case'Class
3. Add to test suite in `test_suite_unit.adb`
4. Build: `make build-tests`
5. Run: `make test-unit`
6. Verify 90%+ coverage

**Integration Test Checklist:**
1. Create test file: `test/integration/test_<feature>.adb`
2. Add to integration test suite
3. Build: `make build-tests`
4. Run: `make test-integration`
5. Verify real I/O works

### 11.7 Test Maintenance

**When Code Changes:**
1. **Domain changes**: Update unit tests first
2. **Infrastructure changes**: Update integration tests
3. **API changes**: Update both unit and integration tests
4. **Bug fixes**: Add regression test

**Coverage Monitoring:**
```bash
make test-coverage
```

---

## 12. Known Issues

None at this time. All 217 tests pass.

---

## 13. Appendices

### 13.1 Test Statistics

- Total Tests: 217
  - Unit: 86
  - Integration: 118
  - Examples (E2E): 13
- Test Framework: AUnit 24.0.0+ (unit) + Custom framework (integration)
- Coverage Tool: gnatcov
- Full-scale Validation: 13,156 checks (598 zones × 22 epochs)

### 13.2 Test Commands Reference

```bash
make test-all          # Run all tests
make test-unit         # Unit tests only
make test-integration  # Integration tests only
make test-coverage     # With coverage
make clean-coverage    # Remove coverage data
```

---

**Document Control**:
- Version: 1.0.0
- Last Updated: 2025-11-16
- Status: Released
- Copyright © 2025 Michael Gardner, A Bit of Help, Inc.
- License: BSD-3-Clause
