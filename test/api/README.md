# TZif API Tests

**Purpose**: Black-box validation of TZif's public API contracts (`TZif.API.*`)

## Overview

API tests validate that TZif's public facade works correctly with real I/O and actual timezone data. These tests:

- ✅ Test the **fully instantiated** public API (`TZif.API.Desktop`)
- ✅ Use **real filesystem I/O** (not mocks)
- ✅ Validate TZif's **public contracts**
- ✅ Belong in the **tzif repository** (not dependent projects)

## Test Scope

### What Tests Belong Here

- Public API operations: `Find_By_Id`, `Find_By_Region`, `Find_By_Pattern`, etc.
- Timezone lookup with real zone files
- Transition calculations for specific epochs
- Source discovery and validation
- Cache import/export functionality
- Error handling at the public API boundary

### What Tests Do NOT Belong Here

- Internal layer coordination (→ `test/integration/`)
- Domain logic in isolation (→ `test/unit/domain/`)
- Infrastructure adapters (→ `test/unit/infrastructure/`)
- Dependent project behavior (→ zoneinfo, etc.)

## Test Structure

Each test validates one public API operation:

```ada
pragma Ada_2022;
with Test_Framework;
with TZif.API.Desktop;  -- Fully instantiated public facade

procedure Test_Find_Zone is
   use Test_Framework;
   use TZif.API.Desktop;

   Result : Find_By_Id_Result_Type;
begin
   Put_Line ("Test: Find Zone - Public API");

   -- Test public API contract
   API.Find_By_Id (Make_Zone_Id ("America/New_York"), Result);

   Assert (Result.State = Ok_State, "Should find valid zone");
   Assert (Get_Zone_Id (Result.Value) = "America/New_York",
           "Zone ID matches");

   Report_Results (Test_Count, Pass_Count);
end Test_Find_Zone;
```

## Running Tests

```bash
# Run all API tests
make test-api

# Run individual test
./test/bin/api/test_find_zone
```

## Test Ownership Rationale

**Why API tests belong in tzif, not dependent projects:**

1. **Contract Ownership**: TZif owns and validates its own public API contracts
2. **Coverage Visibility**: TZif's API coverage shows up in tzif CI/CD metrics
3. **Faster Feedback**: Contributors see API breakage immediately
4. **Clear Responsibility**: Dependent projects test their own behavior, not TZif's

**What dependent projects should test:**

- How they **use** TZif (integration patterns)
- Their own domain logic
- Their own public API surface
- **Not** TZif's API contracts (trust TZif's own tests)

## Test Migration

API tests are being migrated from dependent projects (e.g., `zoneinfo/test/tzif_api_e2e/`) to this directory.

**Migration Status**: TBD

## Test Data

Tests use real IANA timezone data from:
- `/usr/share/zoneinfo` (macOS, Linux)
- System timezone database

Common test zones:
- `UTC` - No transitions, simple case
- `America/New_York` - US Eastern, DST transitions
- `Europe/London` - European timezone, DST
- `America/Los_Angeles` - US Pacific

## Coverage Goals

- All public API operations have at least one happy-path test
- Error cases tested at API boundary
- Real-world timezone scenarios validated
- Target: 100% public API operation coverage
