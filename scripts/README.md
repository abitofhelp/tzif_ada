# Project Scripts

**Development automation scripts for building, testing, and maintaining TZif**

**Version:** 1.0.0
**Date:** November 16, 2025
**SPDX-License-Identifier:** BSD-3-Clause
**License File:** See the LICENSE file in the project root.
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.
**Status:** Released

---

## Overview

This directory contains scripts that automate development tasks for the TZif library. Scripts are implemented in Python or shell for cross-platform compatibility and maintainability.

**Benefits**:
- **Better readability** - Easier to understand than complex Makefile rules
- **Cross-platform** - Works on macOS, Linux, and Windows
- **Error handling** - Proper exception handling and user feedback
- **Testability** - Scripts can be unit tested
- **Maintainability** - Easier to modify and extend

---

## Active Scripts

### Development Tools

#### `arch_guard.py`

**Purpose:** Validate hexagonal architecture layer dependencies

**Usage:**
```bash
# Via Makefile
make check-arch

# Direct execution
python3 scripts/arch_guard.py
```

**What it does:**
- Scans all Ada source files for `with` clauses
- Validates layer dependency rules (Domain → Application → Infrastructure)
- Enforces center-seeking dependencies
- Prevents transitive dependency violations

**Exit Codes:**
- `0` - All architecture rules satisfied
- `1` - One or more architecture violations found

**Features:**
- Validates 110+ Ada files
- Stand-alone library enforcement
- Library_Interface validation
- Bootstrap layer handling

**Related Documentation:** [Architecture Enforcement Guide](../docs/guides/architecture_enforcement.md)

---

#### `coverage.sh`

**Purpose:** Run test suite with code coverage analysis

**Usage:**
```bash
# Via Makefile
make test-coverage

# Direct execution
bash scripts/coverage.sh
```

**What it does:**
1. Cleans previous coverage artifacts
2. Rebuilds tests with coverage instrumentation
3. Runs complete test suite
4. Generates HTML coverage reports using gcovr
5. Opens coverage report in browser

**Output:**
- Terminal summary of coverage statistics
- HTML reports in `coverage/` directory

**Requirements:**
- gcovr (install via: `pip3 install gcovr`)
- GNAT toolchain with gcov support

---

#### `run_gnatcov.sh`

**Purpose:** Advanced coverage analysis using GNAT Coverage

**Usage:**
```bash
bash scripts/run_gnatcov.sh
```

**What it does:**
- Runs tests with GNAT Coverage instrumentation
- Generates detailed coverage reports
- Provides statement, decision, and condition coverage

**Use Case:** Advanced coverage analysis beyond basic gcov

---

#### `fix_separators.sh`

**Purpose:** Standardize comment separator lines in Ada source files

**Status:** Temporary - Will be replaced by Ada formatter application

**Usage:**
```bash
bash scripts/fix_separators.sh
```

**What it does:**
- Finds all `.ads` and `.adb` files in test directory
- Ensures equals separator lines are exactly 75 characters
- Standardizes comment separator formatting

**Note:** This script will be deleted once the Ada formatter application is complete.

---

### Release Management

#### `sync_versions.py`

**Purpose:** Synchronize version numbers across all alire.toml files

**Usage:**
```bash
# Via Makefile (future)
make sync-versions

# Direct execution
python3 scripts/sync_versions.py
python3 scripts/sync_versions.py --dry-run
python3 scripts/sync_versions.py --verbose
```

**What it does:**
1. Reads version from root `alire.toml` (single source of truth)
2. Updates version in all layer alire.toml files to match

**Options:**
- `--dry-run` - Show what would be changed without modifying files
- `--verbose` - Show detailed information about each file

**Use Case:** Ensures all crates have synchronized version numbers during releases

---

#### `validate_release.py`

**Purpose:** Comprehensive pre-release validation checks

**Usage:**
```bash
python3 scripts/validate_release.py
python3 scripts/validate_release.py --verbose
python3 scripts/validate_release.py --quick
```

**What it validates:**
1. **File Headers** - Verifies copyright and SPDX identifiers
2. **Markdown Status** - Checks status fields are "Released"
3. **Build Warnings** - Verifies zero compiler warnings
4. **Test Suite** - Runs all tests and verifies 100% pass rate
5. **TODOs/FIXMEs** - Searches for remaining TODO comments
6. **Diagrams** - Verifies all .puml files have .svg exports
7. **Guides** - Checks required documentation is present
8. **Temporary Files** - Finds files that should be cleaned

**Options:**
- `--verbose` - Show detailed check information
- `--quick` - Skip slow build and test execution

**Exit Codes:**
- `0` - All validations passed (ready for release)
- `1` - One or more validations failed
- `2` - Script error or missing dependencies

**Use Case:** Automated release quality validation before tagging version

---

### Shared Utilities

#### `common.py`

**Purpose:** Shared utilities and helper functions for Python scripts

**Features:**
- Terminal color output (ANSI codes)
- OS detection (macOS, Linux, Windows)
- Command existence checking
- Package manager detection
- Common print functions (success, error, warning, info)

**Usage:** Imported by other Python scripts
```python
from common import print_success, command_exists, is_macos
```

**Note:** Only kept if other scripts import it

---

## Script Classification

### Active Development Scripts
Scripts used during regular development:
- `arch_guard.py` - Architecture validation
- `coverage.sh` - Code coverage analysis
- `run_gnatcov.sh` - Advanced coverage
- `fix_separators.sh` - Code formatting (temporary)

### Release Tools
Scripts used for version releases:
- `sync_versions.py` - Version synchronization
- `validate_release.py` - Pre-release validation

### Utilities
Shared code for other scripts:
- `common.py` - Shared Python utilities

---

## Integration with Makefile

Scripts are invoked from Makefile targets:

```makefile
# Architecture validation
check-arch:
	@python3 scripts/arch_guard.py

# Code coverage
test-coverage:
	@bash scripts/coverage.sh

# Version synchronization (future)
sync-versions:
	@python3 scripts/sync_versions.py

# Release validation (future)
validate-release:
	@python3 scripts/validate_release.py
```

This keeps the Makefile clean and delegates complex logic to testable scripts.

---

## Development Guidelines

### Adding New Scripts

When adding automation scripts:

1. **Use Python 3 or Bash** - Maximize portability
2. **Import from common.py** - Reuse utilities (Python scripts)
3. **Add docstrings** - Document purpose and usage
4. **Handle errors gracefully** - Provide helpful error messages
5. **Make executable** - `chmod +x scripts/your_script.sh`
6. **Add shebang** - `#!/usr/bin/env python3` or `#!/bin/bash`
7. **Document here** - Update this README
8. **Add to Makefile** - Create convenient target if appropriate

### Script Structure (Python)

```python
#!/usr/bin/env python3
"""
Brief description of what this script does.

Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
SPDX-License-Identifier: BSD-3-Clause
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
from common import print_success, print_error

def main() -> int:
    """Main entry point."""
    try:
        # Do work here
        print_success("Task complete!")
        return 0
    except Exception as e:
        print_error(f"Task failed: {e}")
        return 1

if __name__ == '__main__':
    sys.exit(main())
```

### Script Structure (Bash)

```bash
#!/bin/bash
# Brief description of what this script does
#
# Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
# SPDX-License-Identifier: BSD-3-Clause

set -e  # Exit on error

# Do work here
echo "Task complete!"
```

---

## Dependencies

### Required
- **Python 3.7+** - For Python scripts
- **Bash** - For shell scripts
- **pathlib** - File operations (Python built-in)
- **subprocess** - Running commands (Python built-in)

### Optional
- **gcovr** - Required for coverage.sh HTML reports
- **gnatcov** - Required for run_gnatcov.sh

---

## Educational Value

These scripts demonstrate:

1. **Professional project organization** - Scripts separated from build logic
2. **Cross-platform development** - OS detection and adaptation
3. **Error handling** - Proper exception handling and user feedback
4. **Code reuse** - Shared utilities in `common.py`
5. **Documentation** - Clear docstrings and READMEs
6. **Python best practices** - Type hints, descriptive names, modular design

---

## See Also

- [Makefile](../Makefile) - Build automation
- [Architecture Enforcement Guide](../docs/guides/architecture_enforcement.md)
- [Release Checklist](../docs/guides/release_checklist.md)
- [Test Guide](../docs/formal/software_test_guide.md)

---

**Document Control**:
- Version: 1.0.0
- Last Updated: 2025-11-16
- Status: Released
- Copyright © 2025 Michael Gardner, A Bit of Help, Inc.
- License: BSD-3-Clause
