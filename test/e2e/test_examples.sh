#!/usr/bin/env bash
#  E2E tests for example programs
set -uo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
EXAMPLES_DIR="$PROJECT_ROOT/bin/examples"

PASSED=0
FAILED=0

echo "======================================================================="
echo "  TZif Examples E2E Tests"
echo "======================================================================="
echo ""

if [ ! -d "$EXAMPLES_DIR" ]; then
    echo -e "${RED}[FAIL]${NC} Examples directory not found"
    exit 1
fi

test_example() {
    local example_name=$1
    local example_path="$EXAMPLES_DIR/$example_name"
    
    if [ ! -f "$example_path" ]; then
        echo -e "${YELLOW}[SKIP]${NC} $example_name"
        return
    fi
    
    echo -n "Testing $example_name ... "
    
    if output=$("$example_path" 2>&1); then
        if echo "$output" | grep -q "Done\\."; then
            echo -e "${GREEN}[PASS]${NC}"
            PASSED=$((PASSED + 1))
        else
            echo -e "${RED}[FAIL]${NC}"
            FAILED=$((FAILED + 1))
        fi
    else
        echo -e "${RED}[FAIL]${NC} (exit: $?)"
        FAILED=$((FAILED + 1))
    fi
}

test_example "find_by_id"
test_example "find_my_id"
test_example "get_transition_at_epoch"
test_example "discover_sources"
test_example "get_version"
test_example "validate_source"
test_example "load_source"
test_example "find_by_pattern"
test_example "find_by_region"
test_example "find_by_regex"
test_example "list_all_zones"
test_example "import_cache"
test_example "cache_export_import"

echo ""
echo "======================================================================="
echo "  Results: $PASSED passed, $FAILED failed"
echo "======================================================================="

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed!${NC}"
    exit 1
fi
