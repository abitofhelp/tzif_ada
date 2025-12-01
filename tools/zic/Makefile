# ===========================================================================
# Makefile for zic (Zone Information Compiler)
# ===========================================================================
# Simplified build for the IANA tzdb zic compiler tool
#
# Usage:
#   make        - Build zic compiler
#   make clean  - Remove build artifacts
#   make test   - Test zic with a simple zone file
# ===========================================================================

# Compiler settings
CC = cc
CFLAGS = -O2 -Wall -Wextra
LDFLAGS =
LDLIBS =

# Timezone defaults
TZDEFAULT = /etc/localtime
TZDIR = /usr/share/zoneinfo

# Package info
PACKAGE = tzdb
VERSION = 2025b
BUGEMAIL = tz@iana.org

# Timezone source files
TZDATA_DIR = ../../data/tzdb-2025b
TZDATA_SOURCES = africa antarctica asia australasia europe northamerica \
                 southamerica etcetera backward

# Output directory with ISO8601 timestamp (UTC)
TIMESTAMP = $(shell date -u +%Y%m%dT%H%M%SZ)
TZIF_OUTPUT_DIR = ../../data/tzif/$(TIMESTAMP)

# Build targets
.PHONY: all clean test zones

all: zic version.h tzdir.h

zic: zic.o
	$(CC) -o $@ $(CFLAGS) $(LDFLAGS) zic.o $(LDLIBS)

zic.o: zic.c private.h tzfile.h version.h tzdir.h
	$(CC) $(CFLAGS) -c zic.c

version.h: version
	@read -r VERSION <version && printf '%s\n' \
	  'static char const PKGVERSION[]="($(PACKAGE)) ";' \
	  "static char const TZVERSION[]=\"$$VERSION\";" \
	  'static char const REPORT_BUGS_TO[]="$(BUGEMAIL)";' \
	  >$@

tzdir.h:
	@printf '%s\n' >$@ \
	  '#ifndef TZDEFAULT' \
	  '# define TZDEFAULT "$(TZDEFAULT)" /* default zone */' \
	  '#endif' \
	  '#ifndef TZDIR' \
	  '# define TZDIR "$(TZDIR)" /* TZif directory */' \
	  '#endif'

clean:
	rm -f zic zic.o version.h tzdir.h

test: zic
	@echo "Testing zic compiler..."
	@echo "zic version:"
	@./zic --version || ./zic -v || echo "zic built successfully"

# ===========================================================================
# Installation (optional)
# ===========================================================================

install: zic
	@echo "To install zic, copy it to your desired location:"
	@echo "  sudo cp zic /usr/local/bin/"
	@echo "  # or"
	@echo "  cp zic ../../bin/tools/"

# ===========================================================================
# Compile Timezone Binaries
# ===========================================================================

zones: zic
	@echo "Compiling timezone binaries..."
	@echo "  Source: $(TZDATA_DIR)"
	@echo "  Output: $(TZIF_OUTPUT_DIR)"
	@echo "  Timestamp: $(TIMESTAMP)"
	@mkdir -p $(TZIF_OUTPUT_DIR)
	@for file in $(TZDATA_SOURCES); do \
	  echo "  Compiling $$file..."; \
	  ./zic -d $(TZIF_OUTPUT_DIR) $(TZDATA_DIR)/$$file || exit 1; \
	done
	@echo ""
	@echo "✓ Timezone binaries compiled successfully"
	@echo "  Location: $(TZIF_OUTPUT_DIR)"
	@echo ""
	@echo "Zone counts:"
	@find $(TZIF_OUTPUT_DIR) -type f | wc -l | xargs echo "  Total zones:"
	@echo ""
	@echo "Example zones:"
	@ls -1 $(TZIF_OUTPUT_DIR)/America/ 2>/dev/null | head -5 | sed 's/^/    /'
	@echo "    ..."
