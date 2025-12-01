# zic - Zone Information Compiler

This directory contains the IANA `zic` (Zone Information Compiler) tool for compiling timezone source files into TZif binary format.

## Overview

The `zic` compiler reads timezone description files (like `northamerica`, `europe`, etc.) and compiles them into binary TZif files that can be parsed by the TZif library.

## Building

```bash
# Build zic compiler
make

# Compile all IANA timezone binaries (creates timestamped directory)
make zones

# Clean build artifacts
make clean

# Test zic
make test
```

### Compiling Timezone Binaries

The `make zones` target compiles all IANA timezone source files into TZif binary format:

```bash
make zones
```

This creates a timestamped directory in `../../data/tzif/` with ISO8601 format (UTC):
- Example: `data/tzif/20251105T093308Z/` (Z suffix indicates UTC)
- Contains ~600 compiled timezone files
- Organized by region (America/, Europe/, Asia/, etc.)

Each compilation run creates a new timestamped directory, allowing you to maintain multiple versions. The UTC timestamp ensures consistency regardless of local timezone.

## Usage

### Compile a single timezone

```bash
# Compile Los Angeles timezone
./zic -d output_dir /path/to/tzdata/northamerica

# This creates: output_dir/America/Los_Angeles
```

### Compile all IANA timezones

```bash
# Create output directory
mkdir -p zoneinfo

# Compile all zones from tzdata
cd ../../data/tzdb-2025b
../../tools/zic/zic -d ../../tools/zic/zoneinfo africa antarctica asia australasia \
                     europe northamerica southamerica backward

# Compiled zones are now in tools/zic/zoneinfo/
```

### Common Options

```bash
# Show version
./zic --version

# Verbose output
./zic -v -d output_dir source_file

# Specify output directory
./zic -d /path/to/output source_file
```

## Files

- `zic.c` - Zone compiler source code
- `private.h` - Internal header
- `tzfile.h` - TZif format definitions
- `version` - Version file (2025b)
- `Makefile` - Build configuration

## Source

Source files are from IANA tzdb distribution: https://www.iana.org/time-zones

## Use Cases

### For Development

Compile a subset of timezones for testing:

```bash
./zic -d test_zones ../../data/tzdb-2025b/northamerica
# Creates test_zones/America/Los_Angeles, America/New_York, etc.
```

### For Embedded Systems

Compile only the zones you need to minimize binary size:

```bash
# Compile only 3 zones for embedded device
echo "Zone UTC 0 - UTC" > minimal.zi
echo "Zone America/Los_Angeles -8:00 US P%sT" >> minimal.zi
echo "Zone America/New_York -5:00 US E%sT" >> minimal.zi

./zic -d embedded_zones minimal.zi
```

### For Production

Compile the full IANA database:

```bash
cd ../../data/tzdb-2025b
make TZDIR=../../tools/zic/zoneinfo install
```

## Integration with TZif Library

Once you've compiled timezone files with `zic`, you can use them with the TZif parser:

```ada
with Infrastructure.Query_Timezone_Info;

Result := Query_Timezone_Info
  ("tools/zic/zoneinfo/America/Los_Angeles", 1733011200);
```

## Notes

- The compiled TZif files are in binary format per RFC 9636
- zic automatically generates TZif version 2/3/4 files
- The output is portable across different architectures
- Compiled files can be distributed with your application

## See Also

- RFC 9636: TZif Binary Format
- IANA Time Zone Database: https://www.iana.org/time-zones
- tzdb-2025b source: `../../data/tzdb-2025b/`
