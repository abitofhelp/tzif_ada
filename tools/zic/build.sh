#!/bin/bash
# ===========================================================================
# build.sh - Build script for zic (Zone Information Compiler)
# ===========================================================================
#
# Usage:
#   ./build.sh           - Build zic compiler
#   ./build.sh zones     - Compile all timezone binaries (timestamped)
#   ./build.sh clean     - Clean build artifacts
#   ./build.sh test      - Build and test zic
#   ./build.sh install   - Build and copy to bin/tools/
# ===========================================================================

set -e  # Exit on error

case "${1:-build}" in
  build)
    echo "Building zic (Zone Information Compiler)..."
    make
    echo ""
    echo "✓ zic built successfully"
    ./zic --version
    ;;

  zones)
    echo "Compiling all timezone binaries (UTC timestamp)..."
    make zones
    ;;

  clean)
    echo "Cleaning zic build artifacts..."
    make clean
    echo "✓ Clean complete"
    ;;
    
  test)
    echo "Building and testing zic..."
    make
    echo ""
    echo "Compiling test zones..."
    mkdir -p test_output
    ./zic -d test_output ../../data/tzdb-2025b/etcetera
    echo ""
    echo "Testing with TZif parser..."
    cd ../..
    ./bin/examples/timezone_lookup_example tools/zic/test_output/Etc/UTC UTC
    cd tools/zic
    rm -rf test_output
    echo ""
    echo "✓ All tests passed"
    ;;
    
  install)
    echo "Building zic..."
    make
    echo ""
    echo "Installing zic to bin/tools/..."
    mkdir -p ../../bin/tools
    cp zic ../../bin/tools/
    echo "✓ Installed: bin/tools/zic"
    ../../bin/tools/zic --version
    ;;
    
  *)
    echo "Usage: $0 {build|zones|clean|test|install}"
    echo ""
    echo "Commands:"
    echo "  build    - Build zic compiler"
    echo "  zones    - Compile all timezone binaries to data/tzif/TIMESTAMP/"
    echo "  clean    - Remove build artifacts"
    echo "  test     - Build and test zic"
    echo "  install  - Install zic to bin/tools/"
    exit 1
    ;;
esac
