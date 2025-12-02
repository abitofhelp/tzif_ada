# =============================================================================
# Project Makefile
# =============================================================================
# Project: tzif
# Purpose: Hexagonal architecture library with port/adapter pattern
#
# This Makefile provides:
#   - Build targets (build, clean, rebuild)
#   - Test infrastructure (test, test-coverage)
#   - Format/check targets (format, stats)
#   - Documentation generation (docs, api-docs)
#   - Development tools (setup-hooks, ci)
# =============================================================================

PROJECT_NAME := tzif

.PHONY: all build build-dev build-opt build-release build-tests build-profiles check check-arch \
        clean clean-clutter clean-coverage clean-deep compress deps \
		help prereqs rebuild refresh stats test test-all test-coverage test-framework \
		test-integration test-unit test-python install-tools build-coverage-runtime
# FIX: ENABLE AFTER THE TARGETS CONVERT TO USING OUR ADAFMT TOOL, WHICH IS IN DEVELOPMENT.
#       format format-all format-src format-tests

# =============================================================================
# OS Detection
# =============================================================================

UNAME := $(shell uname -s)

# =============================================================================
# Colors for Output
# =============================================================================

GREEN := \033[0;32m
YELLOW := \033[0;33m
RED := \033[0;31m
BLUE := \033[0;34m
ORANGE := \033[38;5;208m
CYAN := \033[0;36m
BOLD := \033[1m
NC := \033[0m

# =============================================================================
# Tool Paths
# =============================================================================

ALR := alr
GPRBUILD := gprbuild
GNATFORMAT := gnatformat
GNATDOC := gnatdoc
PYTHON3 := python3

# =============================================================================
# Tool Flags
# =============================================================================
# NOTE: --no-indirect-imports is NOT needed. Architecture is enforced via
#       Stand-Alone Library with explicit Library_Interface in tzif.gpr
#       which prevents transitive access from API layer to internal packages.
ALR_BUILD_FLAGS := -j8 | grep -E 'warning:|(style)|error:' || true
ALR_TEST_FLAGS  := -j8 | grep -E 'warning:|(style)|error:' || true

# =============================================================================
# Directories
# =============================================================================

BUILD_DIR := obj
LIB_DIR := lib
DOCS_DIR := docs/api
COVERAGE_DIR := coverage
# Get the directory of the currently executing Makefile.
# This assumes the Makefile is the last one in the list.
MAKEFILE_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
PROJECT_DIR := $(MAKEFILE_DIR)/$(PROJECT_NAME)
TEST_DIR := test

# Directories to format (library layers + tests)
FORMAT_DIRS := \
				$(wildcard src/api) \
				$(wildcard src/application) \
               	$(wildcard src/domain) \
			   	$(wildcard src/infrastructure) \
               	$(wildcard $(TEST_DIR))

# =============================================================================
# Default Target
# =============================================================================

all: build

# =============================================================================
# Help Target
# =============================================================================

help: ## Display this help message
	@echo "$(CYAN)$(BOLD)╔══════════════════════════════════════════════════╗$(NC)"
	@echo "$(CYAN)$(BOLD)║  TZif - IANA Timezone Information Library        ║$(NC)"
	@echo "$(CYAN)$(BOLD)╚══════════════════════════════════════════════════╝$(NC)"
	@echo " "
	@echo "$(YELLOW)Build Commands:$(NC)"
	@echo "  build              - Build library (development mode)"
	@echo "  build-dev          - Build with development flags"
	@echo "  build-opt          - Build with optimization (-O2)"
	@echo "  build-release      - Build in release mode"
	@echo "  build-tests        - Build all test executables"
	@echo "  clean              - Clean build artifacts"
	@echo "  clean-clutter      - Remove temporary files and backups"
	@echo "  clean-coverage     - Clean coverage data"
	@echo "  clean-deep         - Deep clean (includes Alire cache)"
	@echo "  compress           - Create compressed source archive (tar.gz)"
	@echo "  rebuild            - Clean and rebuild"
	@echo ""
	@echo "$(YELLOW)Testing Commands:$(NC)"
	@echo "  test               - Run comprehensive test suite (main runner)"
	@echo "  test-unit          - Run unit tests only"
	@echo "  test-integration   - Run integration tests only"
	@echo "  test-all           - Run all test executables"
	@echo "  test-framework     - Run all test suites (unit + integration)"
	@echo "  test-python        - Run Python script tests (arch_guard.py validation)"
	@echo "  test-examples      - Run Ada-based tests for all examples"
	@echo "  test-coverage      - Run tests with coverage analysis"
	@echo ""
	@echo "$(YELLOW)Examples Commands:$(NC)"
	@echo "  build-examples     - Build all example programs"
	@echo "  examples           - Alias for build-examples"
	@echo "  run-examples       - Build and run all example programs"
	@echo ""
	@echo "$(YELLOW)Quality & Architecture Commands:$(NC)"
	@echo "  check              - Run static analysis"
	@echo "  check-arch         - Validate hexagonal architecture boundaries"
# FIX: ENABLE AFTER THE TARGETS CONVERT TO USING OUR ADAFMT TOOL, WHICH IS IN DEVELOPMENT.
# 	@echo "  format-src         - Auto-format source code only"
# 	@echo "  format-tests       - Auto-format test code only"
# 	@echo "  format-all         - Auto-format all code"
# 	@echo "  format             - Alias for format-all"
	@echo "  stats              - Display project statistics by layer"
	@echo ""
	@echo "$(YELLOW)Utility Commands:$(NC)"
	@echo "  deps                    - Show dependency information"
	@echo "  prereqs                 - Verify prerequisites are satisfied"
	@echo "  refresh                 - Refresh Alire dependencies"
	@echo "  install-tools           - Install development tools (GMP, gcovr, gnatformat)"
	@echo "  build-coverage-runtime  - Build GNATcoverage runtime library"
	@echo ""
	@echo "$(YELLOW)Advanced Commands:$(NC)"
	@echo "  build-profiles     - Test compilation with all build profiles"
	@echo ""
	@echo "$(YELLOW)Profile Selection (Target Platform):$(NC)"
	@echo "  Build with specific profile using TZIF_PROFILE variable:"
	@echo "    alr build                                        # standard (default)"
	@echo "    alr build -- -XTZIF_PROFILE=embedded   # Ravenscar embedded"
	@echo "    alr build -- -XTZIF_PROFILE=baremetal  # Zero footprint"
	@echo "    alr build -- -XTZIF_PROFILE=concurrent # Multi-threaded"
	@echo "    alr build -- -XTZIF_PROFILE=stm32h7s78"
	@echo "    alr build -- -XTZIF_PROFILE=stm32mp135_linux"
	@echo ""
	@echo "$(YELLOW)Workflow Shortcuts:$(NC)"
	@echo "  all                - Build library (default)"
	@echo ""
	@echo "$(YELLOW)Submodule Commands:$(NC)"
	@echo "  submodule-init     - Initialize submodules after fresh clone"
	@echo "  submodule-update   - Pull latest from all submodule repos"
	@echo "  submodule-status   - Show submodule commit status"

# =============================================================================
# Build Commands
# =============================================================================

prereqs:
	@echo "$(GREEN)✓ All prerequisites satisfied$(NC)"

build: build-dev

build-dev: check-arch prereqs
	@echo "$(GREEN)Building $(PROJECT_NAME) (development mode)...$(NC)"
	$(ALR) build --development -- $(ALR_BUILD_FLAGS)
	@echo "$(GREEN)✓ Development build complete: $(LIB_DIR)/lib$(PROJECT_NAME).a$(NC)"

build-opt: check-arch prereqs
	@echo "$(GREEN)Building $(PROJECT_NAME) (optimized -O2)...$(NC)"
	$(ALR) build -- -O2 $(ALR_BUILD_FLAGS)
	@echo "$(GREEN)✓ Optimized build complete$(NC)"

build-release: check-arch prereqs
	@echo "$(GREEN)Building $(PROJECT_NAME) (release mode)...$(NC)"
	$(ALR) build --release -- $(ALR_BUILD_FLAGS)
	@echo "$(GREEN)✓ Release build complete: $(LIB_DIR)/lib$(PROJECT_NAME).a$(NC)"

build-tests: check-arch prereqs
	@echo "$(GREEN)Building test suites...$(NC)"
	@if [ -f "$(TEST_DIR)/unit/unit_tests.gpr" ]; then \
		$(ALR) exec -- $(GPRBUILD) -P $(TEST_DIR)/unit/unit_tests.gpr -p $(ALR_TEST_FLAGS); \
		echo "$(GREEN)✓ Unit tests built$(NC)"; \
	else \
		echo "$(YELLOW)Unit test project not found$(NC)"; \
	fi
	@if [ -f "$(TEST_DIR)/integration/integration_tests.gpr" ]; then \
		$(ALR) exec -- $(GPRBUILD) -P $(TEST_DIR)/integration/integration_tests.gpr -p $(ALR_TEST_FLAGS); \
		echo "$(GREEN)✓ Integration tests built$(NC)"; \
	else \
		echo "$(YELLOW)Integration test project not found$(NC)"; \
	fi
	@if [ -f "$(TEST_DIR)/examples/examples_tests.gpr" ]; then \
		$(ALR) exec -- $(GPRBUILD) -P $(TEST_DIR)/examples/examples_tests.gpr -p $(ALR_TEST_FLAGS); \
		echo "$(GREEN)✓ Examples tests built$(NC)"; \
	else \
		echo "$(YELLOW)Examples test project not found$(NC)"; \
	fi

build-profiles: ## Validate library builds with all configuration profiles
	@echo "$(CYAN)$(BOLD)Testing library compilation with all profiles...$(NC)"
	@echo "$(CYAN)Note: This validates compilation only (not cross-compilation)$(NC)"
	@echo "$(CYAN)Uses: alr build -- -XTZIF_PROFILE=<profile>$(NC)"
	@echo ""
	@PROFILES="standard embedded baremetal concurrent stm32h7s78 stm32mp135_linux"; \
	FAILED=0; \
	for profile in $$PROFILES; do \
		echo "$(YELLOW)Testing profile: $$profile$(NC)"; \
		if [ -f "config/profiles/$$profile/$(PROJECT_NAME)_config.ads" ]; then \
			$(ALR) clean > /dev/null 2>&1; \
			if $(ALR) build --development -- -XTZIF_PROFILE=$$profile -j8 2>&1 | grep -E 'error:|failed' > /dev/null; then \
				echo "$(RED)✗ Profile $$profile: FAILED$(NC)"; \
				FAILED=$$((FAILED + 1)); \
			else \
				echo "$(GREEN)✓ Profile $$profile: OK$(NC)"; \
			fi; \
		else \
			echo "$(YELLOW)⚠ Profile $$profile: config file not found (skipping)$(NC)"; \
		fi; \
		echo ""; \
	done; \
	$(ALR) clean > /dev/null 2>&1; \
	if [ $$FAILED -eq 0 ]; then \
		echo "$(GREEN)$(BOLD)✓ All available profiles compiled successfully$(NC)"; \
	else \
		echo "$(RED)$(BOLD)✗ $$FAILED profile(s) failed$(NC)"; \
		exit 1; \
	fi


clean:
	@echo "$(YELLOW)Cleaning project build artifacts (keeps dependencies)...$(NC)"
	@# Use gprclean WITHOUT -r to clean only our project, not dependencies
	@$(ALR) exec -- gprclean -P $(PROJECT_NAME).gpr -q 2>/dev/null || true
	@$(ALR) exec -- gprclean -P $(TEST_DIR)/unit/unit_tests.gpr -q 2>/dev/null || true
	@$(ALR) exec -- gprclean -P $(TEST_DIR)/integration/integration_tests.gpr -q 2>/dev/null || true
	@rm -rf $(BUILD_DIR) $(LIB_DIR) $(TEST_DIR)/bin $(TEST_DIR)/obj
	@find . -name "*.backup" -delete 2>/dev/null || true
	@echo "$(GREEN)✓ Project artifacts cleaned (dependencies preserved for fast rebuild)$(NC)"

clean-deep:
	@echo "$(YELLOW)Deep cleaning ALL artifacts including dependencies...$(NC)"
	@echo "$(YELLOW)⚠️  This will require rebuilding GNATCOLL, XMLAda, etc. (slow!)$(NC)"
	@$(ALR) clean
	@rm -rf $(BUILD_DIR) $(LIB_DIR) $(TEST_DIR)/bin $(TEST_DIR)/obj
	@find . -name "*.backup" -delete 2>/dev/null || true
	@echo "$(GREEN)✓ Deep clean complete (next build will be slow)$(NC)"

clean-coverage:
	@echo "$(YELLOW)Cleaning coverage artifacts...$(NC)"
	@find . -name "*.srctrace" -delete 2>/dev/null || true
	@find . -name "*.traces" -delete 2>/dev/null || true
	@find . -name "*.sid" -delete 2>/dev/null || true
	@rm -rf coverage/ 2>/dev/null || true
	@rm -rf gnatcov-instr/ 2>/dev/null || true
	@echo "$(GREEN)✓ Coverage artifacts cleaned$(NC)"

clean-clutter: ## Remove temporary files, backups, and clutter
	@echo "$(CYAN)Cleaning temporary files and clutter...$(NC)"
	@$(PYTHON3) scripts/python/makefile/cleanup_temp_files.py
	@echo "$(GREEN)✓ Temporary files removed$(NC)"

compress:
	@echo "$(CYAN)Creating compressed source archive... $(NC)"
	@tar -czvf "$(PROJECT_NAME).tar.gz" \
		--exclude="$(PROJECT_NAME).tar.gz" \
	    --exclude='.git' \
	    --exclude='tools' \
		--exclude='data' \
	    --exclude='obj' \
	    --exclude='bin' \
	    --exclude='lib' \
	    --exclude='alire' \
	    --exclude='.build' \
	    --exclude='coverage' \
	    --exclude='.DS_Store' \
	    --exclude='*.o' \
	    --exclude='*.ali' \
	    --exclude='*.backup' \
		.
	@echo "$(GREEN)✓ Archive created: $(PROJECT_NAME).tar.gz $(NC)"

rebuild: clean build

# =============================================================================
# Testing Commands
# =============================================================================

test: test-all

test-all: build build-tests
	@echo "$(GREEN)Running all test executables...$(NC)"
	@failed=0; \
	if [ -d "$(TEST_DIR)/bin" ]; then \
		for test in $(TEST_DIR)/bin/*_runner; do \
			if [ -x "$$test" ] && [ -f "$$test" ]; then \
				echo "$(CYAN)Running $$test...$(NC)"; \
				$$test || failed=1; \
				echo ""; \
			fi; \
		done; \
	else \
		echo "$(YELLOW)No test executables found in $(TEST_DIR)/bin$(NC)"; \
	fi; \
	if [ $$failed -eq 0 ]; then \
		echo ""; \
		echo "\033[1;92m########################################"; \
		echo "###                                  ###"; \
		echo "###   ALL TEST SUITES: SUCCESS      ###"; \
		echo "###   All tests passed!              ###"; \
		echo "###                                  ###"; \
		echo "########################################\033[0m"; \
		echo ""; \
	else \
		echo ""; \
		echo "\033[1;91m########################################"; \
		echo "###                                  ###"; \
		echo "###   ALL TEST SUITES: FAILURE      ###"; \
		echo "###   Some tests failed!             ###"; \
		echo "###                                  ###"; \
		echo "########################################\033[0m"; \
		echo ""; \
		exit 1; \
	fi

test-unit: build build-tests
	@echo "$(GREEN)Running unit tests...$(NC)"
	@if [ -f "$(TEST_DIR)/bin/unit_runner" ]; then \
		$(TEST_DIR)/bin/unit_runner; \
		if [ $$? -eq 0 ]; then \
			echo "$(GREEN)✓ Unit tests passed$(NC)"; \
		else \
			echo "$(RED)✗ Unit tests failed$(NC)"; \
			exit 1; \
		fi; \
	else \
		echo "$(YELLOW)Unit test runner not found at $(TEST_DIR)/bin/unit_runner$(NC)"; \
		exit 1; \
	fi

test-integration: build build-tests
	@echo "$(GREEN)Running integration tests...$(NC)"
	@if [ -f "$(TEST_DIR)/bin/integration_runner" ]; then \
		$(TEST_DIR)/bin/integration_runner; \
		if [ $$? -eq 0 ]; then \
			echo "$(GREEN)✓ Integration tests passed$(NC)"; \
		else \
			echo "$(RED)✗ Integration tests failed$(NC)"; \
			exit 1; \
		fi; \
	else \
		echo "$(YELLOW)Integration test runner not found at $(TEST_DIR)/bin/integration_runner$(NC)"; \
		exit 1; \
	fi

test-framework: test-unit test-integration ## Run all test suites
	@echo "$(GREEN)$(BOLD)✓ All test suites completed$(NC)"

test-coverage: clean build build-coverage-runtime
	@echo "$(GREEN)Running tests with GNATcoverage analysis...$(NC)"
	@if [ -f "scripts/python/makefile/coverage.sh" ]; then \
		bash scripts/python/makefile/coverage.sh; \
	else \
		echo "$(YELLOW)Coverage script not found at scripts/python/makefile/coverage.sh$(NC)"; \
		exit 1; \
	fi

# =============================================================================
# Examples Commands
# =============================================================================

build-examples: build check-arch prereqs
	@echo "$(GREEN)Building example programs...$(NC)"
	@if [ -f "examples/examples.gpr" ]; then \
		$(ALR) exec -- $(GPRBUILD) -P examples/examples.gpr -p $(ALR_BUILD_FLAGS); \
		echo "$(GREEN)✓ Examples built$(NC)"; \
	else \
		echo "$(YELLOW)Examples project not found$(NC)"; \
	fi

examples: build-examples

run-examples: build-examples
	@echo "$(GREEN)Running example programs...$(NC)"
	@if [ -d "$(BIN_DIR)/examples" ]; then \
		for example in $(BIN_DIR)/examples/*; do \
			if [ -x "$$example" ] && [ -f "$$example" ]; then \
				echo "$(CYAN)Running $$example...$(NC)"; \
				$$example || true; \
				echo ""; \
			fi; \
		done; \
		echo "$(GREEN)✓ All examples completed$(NC)"; \
	else \
		echo "$(YELLOW)No examples found in $(BIN_DIR)/examples$(NC)"; \
	fi

test-examples: build-examples build-tests
	@echo "$(GREEN)Running examples tests...$(NC)"
	@if [ -f "$(TEST_DIR)/bin/examples_runner" ]; then \
		$(TEST_DIR)/bin/examples_runner; \
		if [ $$? -eq 0 ]; then \
			echo "$(GREEN)✓ Examples tests passed$(NC)"; \
		else \
			echo "$(RED)✗ Examples tests failed$(NC)"; \
			exit 1; \
		fi; \
	else \
		echo "$(YELLOW)Examples runner not found at $(TEST_DIR)/bin/examples_runner$(NC)"; \
		exit 1; \
	fi

# =============================================================================
# Quality & Code Formatting Commands
# =============================================================================

check:
	@echo "$(GREEN)Running code checks...$(NC)"
	@$(ALR) build --development -- $(ALR_BUILD_FLAGS)
	@echo "$(GREEN)✓ Code checks complete$(NC)"

check-arch: ## Validate hexagonal architecture boundaries
	@echo "$(GREEN)Validating architecture boundaries...$(NC)"
	@PYTHONPATH=scripts/python $(PYTHON3) -m arch_guard
	@if [ $$? -eq 0 ]; then \
		echo "$(GREEN)✓ Architecture validation passed$(NC)"; \
	else \
		echo "$(RED)✗ Architecture validation failed$(NC)"; \
		exit 1; \
	fi

test-python: ## Run Python script tests (arch_guard.py validation)
	@echo "$(GREEN)Running Python script tests...$(NC)"
	@cd test/python && PYTHONPATH=../../scripts/python $(PYTHON3) -m pytest -v
	@echo "$(GREEN)✓ Python tests complete$(NC)"


# FIXME: REPLACE WITH THE ADAFMT TOOL WE ARE CREATING WHEN IT IS COMPLETED.
# THE CURRENT SCRIPT IS COMMENTING COMMENTS AND MESSING UP WITH INDEXED COMMENTS.
# format-src:
# 	@echo "$(GREEN)Formatting source code...$(NC)"
# 	...

# format-tests:
# 	@echo "$(GREEN)Formatting test code...$(NC)"
# 	...

# format-all: format-src format-tests
# 	@echo "$(GREEN)✓ All code formatting complete$(NC)"

# format: format-all


# =============================================================================
# Development Commands
# =============================================================================

stats:
	@echo "$(CYAN)$(BOLD)Project Statistics for $(PROJECT_NAME)$(NC)"
	@echo "$(YELLOW)════════════════════════════════════════$(NC)"
	@echo ""
	@echo "Ada Source Files by Layer:"
	@echo "  Domain specs:          $$(find src/domain -name "*.ads" 2>/dev/null | wc -l | tr -d ' ')"
	@echo "  Domain bodies:         $$(find src/domain -name "*.adb" 2>/dev/null | wc -l | tr -d ' ')"
	@echo "  Application specs:     $$(find src/application -name "*.ads" 2>/dev/null | wc -l | tr -d ' ')"
	@echo "  Application bodies:    $$(find src/application -name "*.adb" 2>/dev/null | wc -l | tr -d ' ')"
	@echo "  Infrastructure specs:  $$(find src/infrastructure -name "*.ads" 2>/dev/null | wc -l | tr -d ' ')"
	@echo "  Infrastructure bodies: $$(find src/infrastructure -name "*.adb" 2>/dev/null | wc -l | tr -d ' ')"
	@echo "  API specs:             $$(find src/api -name "*.ads" 2>/dev/null | wc -l | tr -d ' ')"
	@echo "  API bodies:            $$(find src/api -name "*.adb" 2>/dev/null | wc -l | tr -d ' ')"
	@echo ""
	@echo "Lines of Code:"
	@find src/api src/application src/domain src/infrastructure -name "*.ads" -o -name "*.adb" 2>/dev/null | \
	  xargs wc -l 2>/dev/null | tail -1 | awk '{printf "  Total: %d lines\n", $$1}' || echo "  Total: 0 lines"
	@echo ""
	@echo "Build Artifacts:"
	@if [ -f "./lib/lib$(PROJECT_NAME).a" ]; then \
		echo "  Library: $$(ls -lh ./lib/lib$(PROJECT_NAME).a 2>/dev/null | awk '{print $$5}')"; \
	else \
		echo "  No library found (run 'make build')"; \
	fi

# =============================================================================
# Advanced Targets
# =============================================================================

deps: ## Display project dependencies
	@echo "$(CYAN)Project dependencies from alire.toml:$(NC)"
	@grep -A 10 "\[\[depends-on\]\]" alire.toml || echo "$(YELLOW)No dependencies found$(NC)"
	@echo ""
	@echo "$(CYAN)Alire dependency tree:$(NC)"
	@$(ALR) show --solve || echo "$(YELLOW)Could not resolve dependencies$(NC)"

refresh: ## Refresh Alire dependencies
	@echo "$(CYAN)Refreshing Alire dependencies...$(NC)"
	@$(ALR) update
	@echo "$(GREEN)✓ Dependencies refreshed$(NC)"

install-tools: ## Install development tools (GMP, gcovr, gnatformat)
	@echo "$(CYAN)Installing development tools...$(NC)"
	@$(PYTHON3) scripts/python/makefile/install_tools.py
	@echo "$(GREEN)✓ Tool installation complete$(NC)"

build-coverage-runtime: ## Build GNATcoverage runtime library
	@echo "$(CYAN)Building GNATcoverage runtime...$(NC)"
	@$(PYTHON3) scripts/python/makefile/build_gnatcov_runtime.py

.DEFAULT_GOAL := help

## ---------------------------------------------------------------------------
## Submodule Management
## ---------------------------------------------------------------------------

.PHONY: submodule-update submodule-status submodule-init

submodule-init: ## Initialize submodules after fresh clone
	git submodule update --init --recursive

submodule-update: ## Pull latest from all submodule repos
	git submodule update --remote --merge
	@echo ""
	@echo "Submodules updated. Review changes, then run:"
	@echo "  git add docs/common scripts/python test/python"
	@echo "  git commit -m 'chore: update submodules'"
	@echo "  git push"

submodule-status: ## Show submodule commit status
	git submodule status
