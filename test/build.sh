#!/bin/bash
# ===========================================================================
# Build script for TZif test suite
# ===========================================================================
# Uses 'alr exec' to ensure proper dependency resolution via Alire

set -e

cd "$(dirname "$0")/.."

echo "Building TZif test suite..."
alr exec -- gprbuild -p -P test/test.gpr -j0

echo ""
echo "Test executable: test/bin/aunit_test_runner"
echo "Run tests with: cd test && ./bin/aunit_test_runner"
