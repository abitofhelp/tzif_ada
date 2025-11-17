# Release Checklist for v1.0.0

This document provides a comprehensive checklist for preparing and releasing version 1.0.0 of the Hybrid Architecture Template.

## Release Workflow Process

This section documents **who does what and when** during the release process.

### Step 1: Run Release Preparation Script
**Who:** Developer  
**What:** Update versions, dates, copyright years  
```bash
Have git staged with commit msg AND COMMIT before the prep.  This gives us a return to point if something screws up running the prep script.
DO NOT ADD CONTENT TO THE CHANGELOG FOR THE INITIAL RELEASE UNTIL AFTER PREPARE IS RUN.
python3 scripts/release.py prepare X.X.X
```
**Verifies:**  
- All alire.toml files have matching versions
- Version package updated (shared/src/tzif-version.ads)
- Markdown headers have correct version and date
- Copyright year is current in all files

### Step 2: Claude Code Review
**Who:** Developer + Claude (ada agent)  
**What:** Initial code review in this session  
**Focus:**  
- Code quality issues
- Documentation completeness
- Build warnings
- Test failures

### Step 3: Release Validation Scan
**Who:** Developer or Claude runs automated validation  
**What:** Systematic validation of all checklist items  
**How:** Run the automated validation script:  
```bash
python3 scripts/validate_release.py [--verbose] [--quick]
```

**Options:**  
- `--verbose` - Show detailed output for each check
- `--quick` - Skip time-consuming checks (build, tests)

**The script verifies:**  
- All file headers correct (source, tests, .md)
- Zero build warnings and style warnings
- All tests pass
- TODOs/FIXMEs documented or resolved
- Diagrams and guides up-to-date
- Temporary files purged
- No uncaught exceptions

**Exit codes:**  
- 0 = All validations passed (ready for release)
- 1 = One or more validations failed (needs fixing)
- 2 = Script error

### Step 4: Fix Issues from Validation
**Who:** Developer + Claude  
**What:** Address all blocking issues found in Step 3  

### Step 5: GPT-5 Comprehensive Code Review
**Who:** Developer + GPT-5  
**What:** Architecture and design pattern review  
**Focus:**  
- Architecture adherence (hexagonal, ports & adapters)
- Design pattern implementation
- Generic usage correctness
- Layer boundary violations

### Step 6: Fix Issues from GPT-5 Review
**Who:** Developer + Claude  
**What:** Address architectural and design issues  

### Step 7: Final Checklist Validation
**Who:** Developer manually reviews this checklist  
**What:** Verify every checkbox item below is complete  

### Step 8: Execute Release
**Who:** Developer  
**What:** Create and publish the release  
```bash
# Ensure that you have updated the content of changelog.md 
# from git history.  The content is NOT automatically generated.
git commit -am "Prepare release X.X.X"
python3 scripts/release.py release X.X.X
```
**This creates:**  
- Git tag vX.X.X
- GitHub release
- Pushes to remote

---

## Pre-Release Validation

> **💡 TIP:** Most of these checks are automated by `validate_release.py`:
> ```bash
> python3 scripts/validate_release.py --verbose
> ```
> Run this first to catch issues quickly, then manually verify any items it doesn't cover.

### 1. File Headers and Copyright

- [ ] **Verify all source files (.ads, .adb) have proper headers**
  - Copyright notice with current year
  - SPDX-License-Identifier: BSD-3-Clause
  - Purpose/description section
  - Design notes where applicable

- [ ] **Verify all test files have proper headers**
  - Same requirements as source files
  - Test purpose clearly documented

- [ ] **Verify all markdown files (.md) have proper headers**
  - **Version:** 1.0.0-rc1 current release version
  - **Date:** November 16, 2025
  - **Status:** "Released" (not "Unreleased") for release builds
  - **Copyright:** © YYYY Name, Company
  - Include header on book cover page (docs/book/src/cover.md)
  ```bash
  # Verify version headers are correct
  grep -r "Version:" --include="*.md" . | grep -v ".git"
  grep -r "Status:" --include="*.md" . | grep -v ".git"
  # Should show "Released" not "Unreleased"
  ```

- [ ] **Verify release script updates all headers**
  - Check `scripts/release.sh` or equivalent
  - Ensure it updates copyright year in:
    - Source code (.ads, .adb)
    - Test files
    - Markdown files (.md)
    - Book cover page

### 2. Documentation Quality

#### Source Code Documentation

- [ ] **Review all .ads files for detailed documentation**
  - Package purpose and design rationale
  - Type definitions with usage notes
  - Subprogram specifications with pre/postconditions
  - Examples where helpful

- [ ] **Review all .adb files for implementation notes**
  - .adb files should have similar header to .ads but less detail
  - If only .adb exists (no .ads), it should have the detailed docs that normally go in .ads

- [ ] **Update agent rules if needed**
  - Verify coding standards document includes case where only .adb exists
  - Ensure documentation standards are clear

#### Diagrams and Visual Documentation

- [ ] **Update all PlantUML diagrams (.puml)**
  - Verify architecture diagrams match current implementation
  - Update layer dependency diagrams
  - Review sequence diagrams for use cases

- [ ] **Update all SVG exports**
  - Regenerate from .puml sources
  - Verify rendering quality

- [ ] **Update architecture guides**
  - Verify all architectural decisions are documented
  - Update ADRs (Architecture Decision Records) if they exist
  - Ensure consistency with actual code

#### User-Facing Documentation

- [ ] **Update README.md**
  - Add section about execution modes:
    - Synchronous mode (tzif-bootstrap-main.adb)
    - Asynchronous/Tasking mode (tzif-bootstrap-main_concurrent.adb - post-1.0)
  - Document which mode is default (sync)
  - Explain tasking requirements (Runtime package, pthread)
  - Update build instructions
  - Update usage examples
  - Verify all links work

- [ ] **Update CHANGELOG.md**
  - Add v1.0.0 section
  - Document all features
  - Document known limitations
  - Note concurrent version is post-1.0

- [ ] **Update mdBook documentation**
  - Verify all chapters are complete
  - Check for broken internal links
  - Verify code examples compile
  - Test `make book-build` succeeds without warnings

### 3. Build Validation

#### Test Suite

- [ ] **Build all tests without warnings**
  ```bash
  alr build --profiles=*:validation
  # Check for zero warnings
  # Check for zero style check failures
  ```

- [ ] **Run all unit tests**
  ```bash
  make test
  # Verify all tests pass
  # Check coverage if applicable
  ```

- [ ] **Run all integration tests**
  - Document which integration tests exist
  - Verify they all pass

#### Source Code Build

- [ ] **Build source without warnings (debug mode)**
  ```bash
  alr build --profiles=*:development
  # Zero compilation warnings
  # Zero style check warnings
  ```

- [ ] **Build source without warnings (release mode)**
  ```bash
  alr build --profiles=*:release
  # Zero compilation warnings
  ```

- [ ] **Build source without warnings (optimize mode)**
  ```bash
  alr build --profiles=*:optimize
  # Zero compilation warnings
  ```

### 4. Code Quality Review

#### TODOs and Stubs

- [ ] **Search for all TODO comments**
  ```bash
  grep -r "TODO" src/ bootstrap/ domain/ application/ infrastructure/ presentation/ shared/
  ```
  - Review each TODO
  - Either implement or document as post-1.0 work
  - Add issue tracker items for post-1.0 TODOs

- [ ] **Search for stub implementations**
  ```bash
  grep -r "raise Program_Error" src/ bootstrap/ domain/ application/ infrastructure/ presentation/ shared/
  ```
  - Review each stub
  - Implement or document why stub is acceptable
  - Add tests for newly implemented functions

- [ ] **Search for FIXME comments**
  ```bash
  grep -r "FIXME" src/ bootstrap/ domain/ application/ infrastructure/ presentation/ shared/
  ```
  - Address all FIXMEs or move to issue tracker

#### Known Issues Documentation

- [ ] **Document signal handler task termination issue**
  - Add to CHANGELOG.md under "Known Issues"
  - Add to TODO.md or issue tracker
  - Include reproduction steps
  - Note it's targeted for post-1.0

- [ ] **Document concurrent version refactoring needed**
  - Explain Option B refactoring requirements
  - Estimate effort
  - Target for v1.1 or v2.0

### 5. Cleanup

- [ ] **Remove temporary files**
  ```bash
  find . -name "*.tmp" -o -name "*.bak" -o -name "*~"
  ```

- [ ] **Remove temporary scripts**
  - Review scripts/ directory
  - Remove any test/debug scripts not needed for users
  - Keep only release scripts and documented utilities

- [ ] **Clean build artifacts**
  ```bash
  make clean
  alr clean
  ```

- [ ] **Verify .gitignore is complete**
  - Ensure all build artifacts are ignored
  - Ensure all temporary files are ignored
  - Test with fresh clone

### 6. Error Handling Validation

This is a critical section that requires dedicated testing time.

#### Synchronous Mode Error Handling

- [ ] **Test domain layer error handling**
  - Invalid person names
  - Boundary conditions
  - Verify errors propagate correctly through Result types

- [ ] **Test application layer error handling**
  - Use case failures
  - Port communication failures
  - Error mapping between layers

- [ ] **Test infrastructure layer error handling**
  - Console output failures
  - Logger failures
  - Adapter error propagation

- [ ] **Test presentation layer error handling**
  - CLI parsing errors
  - Help/version flags
  - Invalid arguments
  - Signal handling (SIGINT, SIGTERM)

- [ ] **Test bootstrap composition errors**
  - Generic instantiation edge cases
  - Initialization failures

#### Asynchronous/Tasking Mode Error Handling (Post-1.0)

- [ ] **NOTE: Deferred to post-1.0 due to Option B refactoring requirements**
  - Document this in CHANGELOG.md
  - Create tracking issue

### 7. End-to-End Testing

#### CLI Option Testing (Synchronous Mode)

- [ ] **Test help flags**
  - [ ] `-h` displays help and exits with 0
  - [ ] `--help` displays help and exits with 0
  - [ ] Help output contains all expected sections

- [ ] **Test version flags**
  - [ ] `-v` displays version and exits with 0
  - [ ] `--version` displays version and exits with 0
  - [ ] Version output contains "v1.0.0"

- [ ] **Test quiet flags**
  - [ ] `-q` suppresses greeting output
  - [ ] `--quiet` suppresses greeting output
  - [ ] Exit codes still correct in quiet mode

- [ ] **Test valid name arguments**
  - [ ] Simple names: "Alice", "Bob"
  - [ ] Names with spaces: "Alice Smith"
  - [ ] Names with hyphens: "Mary-Jane"
  - [ ] Names with apostrophes: "O'Brien"

- [ ] **Test invalid name arguments**
  - [ ] Empty name returns error code 1
  - [ ] Name too long (>80 chars) returns error code 1
  - [ ] Name with numbers returns error code 1
  - [ ] Name with special chars returns error code 1

- [ ] **Test error cases**
  - [ ] No arguments shows usage and returns error code 1
  - [ ] Unknown flag shows error and returns error code 1
  - [ ] Verify error messages are clear and helpful

- [ ] **Test flag combinations**
  - [ ] `--help --quiet Alice` - help takes precedence
  - [ ] `--version --quiet` - version takes precedence
  - [ ] Multiple names - first is used or error shown

#### CLI Option Testing (Asynchronous Mode)

- [ ] **NOTE: Deferred to post-1.0**
  - Same tests as sync mode
  - Plus concurrency-specific tests

### 8. Performance and Resource Testing

- [ ] **Check binary size**
  - Debug build size reasonable
  - Release build size acceptable
  - Optimize build produces smallest binary

- [ ] **Check memory usage**
  - Run with valgrind or similar (if available)
  - Check for memory leaks
  - Verify clean shutdown

- [ ] **Check startup/shutdown time**
  - Measure time for help flag
  - Measure time for version flag
  - Measure time for simple greeting
  - **NOTE: Currently hangs due to signal handler issue - track for post-1.0**

### 9. Release Artifacts

- [ ] **Generate release notes**
  - Summarize features
  - Highlight architecture decisions
  - Note educational value
  - List known issues
  - Provide upgrade guidance (if applicable)

- [ ] **Tag release in git**
  ```bash
  git tag -a v1.0.0 -m "Release v1.0.0: Hexagonal Architecture Template"
  git push origin v1.0.0
  ```

- [ ] **Create GitHub release**
  - Upload binary artifacts (if applicable)
  - Include release notes
  - Link to documentation

- [ ] **Update alire.toml version**
  - Change from "0.1.0-dev" to "1.0.0"
  - Update dependencies if needed
  - Test with `alr build`

- [ ] **Publish to Alire (if desired)**
  - Follow Alire submission guidelines
  - Test installation in fresh environment

### 10. Documentation Publishing

- [ ] **Build final mdBook**
  ```bash
  make book-clean book-build
  ```

- [ ] **Deploy mdBook to GitHub Pages (if desired)**
  - Configure GitHub Pages
  - Push to gh-pages branch
  - Verify live site

- [ ] **Update project links**
  - README.md links to published docs
  - alire.toml website field
  - GitHub repository description

## Post-Release

### Immediate Tasks

- [ ] **Announce release**
  - Project README
  - Relevant forums/communities
  - Social media (if applicable)

- [ ] **Monitor for issues**
  - Watch issue tracker
  - Respond to questions
  - Triage bug reports

### v1.0.0 Planning (Hotfixes)

- [ ] **Fix signal handler task termination issue**
  - Investigate root cause
  - Implement fix
  - Add regression tests
  - Release as v1.0.0

### v1.1 or v2.0 Planning (Features)

- [ ] **Refactor concurrent version for Option B**
  - Update tzif-bootstrap-main_concurrent.adb
  - Match wrapper pattern from sync version
  - Add concurrent-specific tests
  - Update documentation

- [ ] **Add more example use cases**
  - Farewell message (as documented in Chapter 13)
  - Data validation examples
  - File I/O examples

- [ ] **Enhance error handling**
  - More detailed error messages
  - Error recovery strategies
  - Logging improvements

## Notes

### Current Known Issues (v1.0.0)

1. **Signal handler task does not terminate properly**
   - Cause: Task cleanup in Uninstall function times out
   - Impact: Binary hangs after logging "Signal handlers uninstalled"
   - Workaround: Use timeout in scripts/tests
   - Fix: Targeted for v1.0.0

2. **Concurrent version out of sync**
   - Cause: Not updated for Option B refactoring
   - Impact: Does not compile
   - Fix: Targeted for v1.1 or v2.0

### Testing Environment

- **Platform**: macOS (Darwin 24.6.0), x86_64
- **Compiler**: GNAT (from Alire)
- **Ada Version**: Ada 2022
- **Build Tool**: Alire + GPRbuild

### Support

For questions or issues:
- GitHub Issues: https://github.com/abitofhelp/tzif/issues
- Email: mjgardner@abitofhelp.com

---

**Template Version**: 1.0.0
**Last Updated**: 2025-10-27
**Maintained By**: Michael Gardner, A Bit of Help, Inc.
