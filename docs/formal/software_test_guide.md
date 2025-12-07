# Software Test Guide

**Version:** 1.1.0<br>
**Date:** 2025-12-06<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

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
- Focus on domain value objects and services
- Count: 200 tests

**Integration Tests**:
- Test full stack with real TZif data
- Test use cases end-to-end
- Test error conditions and edge cases
- Count: 116 tests

**Examples as Tests**:
- Working examples that demonstrate usage
- Validate real-world scenarios
- Count: 11 examples

### 2.2 Testing Approach

- **Integration First**: Test real TZif file parsing with actual timezone data
- **Unit Tests for Calculations**: Domain services and value object operations
- **Railway-Oriented**: Test both success and error paths
- **Comprehensive**: Cover normal, edge, and error cases
- **Automated**: All tests runnable via `make test-all`

---

## 3. Test Organization

### 3.1 Directory Structure

```
test/
├── unit/                          # Unit tests
│   ├── unit_tests.gpr             # GPR project
│   ├── unit_runner.adb            # Main test runner
│   ├── test_zone_id.adb           # Zone ID validation tests
│   ├── test_iana_releases.adb     # IANA release parsing tests
│   ├── test_ulid.adb              # ULID generation/validation tests
│   ├── test_tzif_data.adb         # TZif data structure tests
│   ├── test_timezone_lookup.adb   # Timezone lookup service tests
│   ├── test_zone_entity.adb       # Zone entity tests
│   └── test_value_object_accessors.adb
│
├── integration/                   # Integration tests
│   ├── integration_tests.gpr      # GPR project
│   ├── integration_runner.adb     # Main test runner
│   ├── test_find_by_id.adb        # Find zone by ID tests
│   ├── test_find_by_pattern.adb   # Pattern search tests
│   ├── test_find_by_regex.adb     # Regex search tests
│   ├── test_find_by_region.adb    # Region search tests
│   ├── test_find_my_id.adb        # Local timezone detection tests
│   ├── test_get_transition_at_epoch.adb  # Transition lookup tests
│   ├── test_get_version.adb       # Version query tests
│   ├── test_list_all_order_by_id.adb     # Zone listing tests
│   ├── test_discover_sources.adb  # Source discovery tests
│   ├── test_load_source.adb       # Source loading tests
│   ├── test_validate_source.adb   # Source validation tests
│   ├── test_tzif_parser_errors.adb       # Parser error handling
│   └── test_zone_repository_errors.adb   # Repository error handling
│
├── common/                        # Shared test infrastructure
│   ├── test_framework.ads         # Result tracking, summaries
│   └── test_framework.adb
│
└── python/                        # Python-based tests
    └── test_arch_guard_ada.py     # Architecture boundary validation
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
```

### 4.2 Individual Test Execution

```bash
# Run unit test runner
./test/bin/unit_runner

# Run integration test runner
./test/bin/integration_runner
```

### 4.3 Test Output

**Success**:
```
========================================
        GRAND TOTAL - ALL UNIT TESTS
========================================
Total tests:   200
Passed:        200
Failed:        0

########################################
###                                  ###
###    UNIT TESTS: SUCCESS
###    All  200 tests passed!
###                                  ###
########################################
```

**Failure**:
```
========================================
  Results: 125 / 126 passed
  Status: TESTS FAILED
========================================
```

---

## 5. Unit Tests

### 5.1 Domain Layer Tests

#### test_zone_id.adb
**Package Under Test:** `TZif.Domain.Value_Object.Zone_Id`

| Test Category | Description |
|--------------|-------------|
| Valid zone IDs | Test creation with valid IANA zone IDs |
| Invalid zone IDs | Test rejection of malformed zone IDs |
| Zone ID comparison | Test equality and ordering |
| Zone ID string conversion | Test To_String and Make_Zone_Id |

#### test_ulid.adb
**Package Under Test:** `TZif.Infrastructure.ULID`

| Test Category | Description |
|--------------|-------------|
| ULID generation | Unique ID generation |
| ULID parsing | Parse ULID strings |
| ULID validation | Validate ULID format |
| ULID ordering | Lexicographic ordering |

#### test_timezone_lookup.adb
**Package Under Test:** `TZif.Domain.Service.Timezone_Lookup`

| Test Category | Description |
|--------------|-------------|
| Transition lookup | Find transition at epoch |
| Edge cases | Before/after transition data |
| DST handling | Daylight saving transitions |

#### test_tzif_data.adb
**Package Under Test:** TZif data structures

| Test Category | Description |
|--------------|-------------|
| Data construction | Create TZif data records |
| Data accessors | Read transition data |
| Data validation | Validate data integrity |

---

## 6. Integration Tests

### 6.1 Use Case Tests

Each use case has a corresponding integration test file:

| Test File | Use Case | Description |
|-----------|----------|-------------|
| test_find_by_id.adb | Find_By_Id | Lookup zones by exact ID |
| test_find_by_pattern.adb | Find_By_Pattern | Pattern matching search |
| test_find_by_regex.adb | Find_By_Regex | Regex search |
| test_find_by_region.adb | Find_By_Region | Region-based search |
| test_find_my_id.adb | Find_My_Id | Local timezone detection |
| test_get_transition_at_epoch.adb | Get_Transition_At_Epoch | Transition lookup |
| test_get_version.adb | Get_Version | Database version query |
| test_list_all_order_by_id.adb | List_All_Order_By_Id | Zone enumeration |
| test_discover_sources.adb | Discover_Sources | Source discovery |
| test_load_source.adb | Load_Source | Source loading |
| test_validate_source.adb | Validate_Source | Source validation |

### 6.2 Error Handling Tests

| Test File | Description |
|-----------|-------------|
| test_tzif_parser_errors.adb | Parser error scenarios |
| test_zone_repository_errors.adb | Repository error scenarios |

---

## 7. Platform Adapter Testing

### 7.1 Platform Abstraction Test Strategy

The TZif library uses hexagonal architecture with platform-specific adapters. This requires a multi-tier testing approach:

| Test Tier | Purpose | Runs On |
|-----------|---------|---------|
| Unit Tests (Mock) | Test port contracts with mock adapters | All platforms |
| Platform Integration | Test real platform adapters | Native platform only |
| CI Validation | Validate platform support in CI | GitHub Actions runners |

### 7.2 Platform Adapter Components

Each platform adapter implements the same outbound port contracts:

| Port | Desktop Adapter | Windows Adapter | Embedded Adapter |
|------|-----------------|-----------------|------------------|
| `Get_System_Timezone_Id` | POSIX symlink resolution | Win32 API + CLDR mapping | Config/env variable |
| File I/O | Standard Ada.Direct_IO | Standard Ada.Direct_IO | RAM filesystem |

### 7.3 Windows Platform Tests

**Location**: `test/integration/windows_platform_tests.gpr`

**Purpose**: Standalone test project for Windows CI that validates:
- Win32 `GetDynamicTimeZoneInformation` API binding
- Windows-to-IANA timezone mapping via CLDR data
- Platform adapter compilation on Windows

**Test File**: `test_windows_platform.adb`

| Test Category | Description |
|--------------|-------------|
| Win32 API binding | Verify FFI to Windows timezone API |
| CLDR mapping | Windows timezone name → IANA zone ID |
| Error handling | Invalid/unknown timezone mapping |

**CI Workflow**: `.github/workflows/windows-release.yml`

```yaml
# Windows CI runs standalone platform tests
- name: Build Windows platform tests
  run: alr exec -- gprbuild -P test/integration/windows_platform_tests.gpr

- name: Run Windows platform tests
  run: alr exec -- ./test/bin/test_windows_platform
```

### 7.4 Desktop (POSIX) Platform Tests

**Location**: Integrated in standard integration tests

**Purpose**: Validate POSIX platform adapter functionality:
- `/etc/localtime` symlink resolution
- Canonical path normalization
- Standard zoneinfo directory traversal

**Tested By**: `test_find_my_id.adb` (detects local system timezone)

### 7.5 Embedded Platform Tests

**Purpose**: Validate embedded platform adapter for STM32F769I and similar boards.

**Test Strategy**:

| Approach | Description |
|----------|-------------|
| Mock Testing | Use mock adapter returning configured zone ID |
| QEMU Emulation | Run on ARM Cortex-M emulator with RAM filesystem |
| Hardware-in-Loop | Run on actual STM32F769I-DK board (manual) |

**Mock Adapter Test Pattern**:

```ada
--  Test using mock adapter that returns configured value
package Mock_System_Timezone is
   Configured_Zone : constant String := "America/New_York";

   function Get_System_Timezone_Id return Zone_Id_Result is
     (Zone_Id_Result.Make_Ok (Make_Zone_Id (Configured_Zone)));
end Mock_System_Timezone;

--  Instantiate port with mock adapter
package Test_TZ_Port is new Application.Port.Outbound.System_Timezone
  (Get_System_Timezone_Id => Mock_System_Timezone.Get_System_Timezone_Id);
```

### 7.6 Cross-Platform Test Matrix

| Test Suite | Desktop/POSIX | Windows | Embedded |
|------------|---------------|---------|----------|
| Unit Tests | ✓ | ✓ | ✓ |
| Integration Tests | ✓ | ✗ (platform tests only) | ✓ (mock) |
| Platform Tests | ✓ | ✓ | ✓ (QEMU/hardware) |
| Examples | ✓ | ✗ | ✓ (selected) |

### 7.7 GPR Scenario Variables for Platform Testing

Platform selection is controlled via GPR scenario variables:

```bash
# Test on POSIX (default)
gprbuild -P integration_tests.gpr -XTZIF_OS=unix

# Test on Windows
gprbuild -P windows_platform_tests.gpr -XTZIF_OS=windows

# Test for embedded (with mock adapters)
gprbuild -P embedded_tests.gpr -XTZIF_PROFILE=embedded
```

---

## 8. Test Data

### 8.1 Test Timezone Data

**Location**: `/usr/share/zoneinfo` (system default)

**Test Zones Used**:
- `America/New_York`: Standard US timezone
- `America/Los_Angeles`: West coast timezone
- `Europe/London`: European timezone with DST
- `UTC`: Special timezone with no transitions
- `America/Phoenix`: No DST timezone

### 8.2 Test Fixtures

Integration tests use the system timezone database when available, with fallback to test fixtures for deterministic testing.

---

## 9. Writing Tests

### 9.1 Unit Test Structure

```ada
pragma Ada_2022;
with Test_Framework;
with TZif.Domain.Value_Object.Zone_Id;

procedure Test_Zone_Id is
   use TZif.Domain.Value_Object.Zone_Id;
   use Test_Framework;

   Total_Tests  : Natural := 0;
   Passed_Tests : Natural := 0;

   procedure Run_Test (Name : String; Passed : Boolean) is
   begin
      Total_Tests := Total_Tests + 1;
      if Passed then
         Passed_Tests := Passed_Tests + 1;
         Put_Line ("  [PASS] " & Name);
      else
         Put_Line ("  [FAIL] " & Name);
      end if;
   end Run_Test;

begin
   Put_Line ("Test: Zone_Id");

   --  Test case
   Run_Test ("Valid zone ID creates successfully",
             Is_Ok (Make_Zone_Id ("America/New_York")));

   --  Register with framework
   Test_Framework.Register_Results (Total_Tests, Passed_Tests);
end Test_Zone_Id;
```

### 9.2 Integration Test Structure

```ada
pragma Ada_2022;
with Test_Framework;
with TZif.API;

procedure Test_Find_By_Id is
   use TZif.API;
   use Test_Framework;

begin
   Put_Line ("Test: Find_By_Id - Real Timezone Data");

   --  Test with real filesystem
   declare
      Result : constant Zone_Result := Find_By_Id (Make_Zone_Id ("America/New_York"));
   begin
      Run_Test ("Find America/New_York succeeds", Is_Ok (Result));
   end;

   Test_Framework.Register_Results (Total_Tests, Passed_Tests);
end Test_Find_By_Id;
```

---

## 10. Test Coverage

### 10.1 Coverage Goals

| Layer | Target |
|-------|--------|
| Domain | 100% (pure logic, fully testable) |
| Application | 95%+ (use cases are orchestration) |
| Infrastructure | 90%+ (adapter code, error paths) |
| Overall | 90%+ |

### 10.2 Running Coverage Analysis

```bash
# Generate coverage report
make test-coverage

# View HTML report
open coverage/report/index.html
```

---

## 11. Continuous Integration

### 11.1 CI Pipeline

```yaml
steps:
  - name: Build
    run: alr build

  - name: Unit Tests
    run: make test-unit

  - name: Integration Tests
    run: make test-integration

  - name: Examples
    run: make test-examples
```

### 11.2 Windows CI Workflow

Windows platform validation runs via `.github/workflows/windows-release.yml`:

```yaml
steps:
  - name: Build Windows platform tests
    run: alr exec -- gprbuild -P test/integration/windows_platform_tests.gpr

  - name: Run Windows platform tests
    run: alr exec -- ./test/bin/test_windows_platform
```

**Trigger**: Manual dispatch (called by `release.py prepare` or GitHub Actions UI)

**Purpose**: Pre-flight validation of Windows compatibility before release.

### 11.3 Success Criteria

All must pass:
- Zero build warnings
- All unit tests pass (200 tests)
- All integration tests pass (116 tests)
- All examples execute successfully (11 examples)

---

## 12. Appendices

### 12.1 Test Statistics

| Category | Count |
|----------|-------|
| Unit tests | 200 |
| Integration tests | 116 |
| Example programs | 11 |
| **Total test assets** | **327** |

### 12.2 Test Commands Reference

```bash
make test-all          # Run all tests
make test-unit         # Unit tests only
make test-integration  # Integration tests only
make test-examples     # Run examples as tests
make test-coverage     # With coverage
make clean-coverage    # Remove coverage data
```

---

**Document Control**:
- Version: 1.1.0
- Last Updated: 2025-12-06
- Status: Draft (Platform Abstraction Refactoring)
- Copyright © 2025 Michael Gardner, A Bit of Help, Inc.
- License: BSD-3-Clause

**Change History**:
| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0.0 | 2025-12-02 | Michael Gardner | Initial release |
| 1.1.0 | 2025-12-06 | Michael Gardner | Added Section 7 Platform Adapter Testing for multi-platform test strategy |
