#!/usr/bin/env python3
"""
Generate additional invalid TZif test files to improve parser coverage.
Focuses on edge cases not covered by the initial 16 test files.
"""

import struct
import os

# Change to the script's directory
os.chdir(os.path.dirname(os.path.abspath(__file__)))

print("Generating additional TZif parser test files...")
print("=" * 60)

# File 17: Partial read during header count fields (truncate during UTC/local count)
# This should trigger the Read_Int32 error path for partial reads
with open("partial_header_utc_local.tzif", "wb") as f:
    f.write(b"TZif2")      # Magic + version
    f.write(b"\x00" * 15)  # Reserved
    # Write only 2 bytes of the UTC/local count (should be 4)
    f.write(b"\x00\x01")   # Partial Int32
print("✓ partial_header_utc_local.tzif - Truncated during UTC/local count read")

# File 18: Partial read during standard/wall count
with open("partial_header_std_wall.tzif", "wb") as f:
    f.write(b"TZif2")
    f.write(b"\x00" * 15)
    f.write(struct.pack(">I", 0))  # isutcnt - complete
    f.write(b"\x00\x01")   # Partial isstdcnt
print("✓ partial_header_std_wall.tzif - Truncated during std/wall count read")

# File 19: Partial read during leap count
with open("partial_header_leap.tzif", "wb") as f:
    f.write(b"TZif2")
    f.write(b"\x00" * 15)
    f.write(struct.pack(">I", 0))  # isutcnt
    f.write(struct.pack(">I", 0))  # isstdcnt
    f.write(b"\x00\x01")   # Partial leapcnt
print("✓ partial_header_leap.tzif - Truncated during leap count read")

# File 20: Partial read during transition count
with open("partial_header_transition.tzif", "wb") as f:
    f.write(b"TZif2")
    f.write(b"\x00" * 15)
    f.write(struct.pack(">I", 0))  # isutcnt
    f.write(struct.pack(">I", 0))  # isstdcnt
    f.write(struct.pack(">I", 0))  # leapcnt
    f.write(b"\x00\x01")   # Partial timecnt
print("✓ partial_header_transition.tzif - Truncated during transition count read")

# File 21: Partial read during type count
with open("partial_header_type.tzif", "wb") as f:
    f.write(b"TZif2")
    f.write(b"\x00" * 15)
    f.write(struct.pack(">I", 0))  # isutcnt
    f.write(struct.pack(">I", 0))  # isstdcnt
    f.write(struct.pack(">I", 0))  # leapcnt
    f.write(struct.pack(">I", 0))  # timecnt
    f.write(b"\x00\x01")   # Partial typecnt
print("✓ partial_header_type.tzif - Truncated during type count read")

# File 22: Partial read during abbrev chars
with open("partial_header_abbrev.tzif", "wb") as f:
    f.write(b"TZif2")
    f.write(b"\x00" * 15)
    f.write(struct.pack(">I", 0))  # isutcnt
    f.write(struct.pack(">I", 0))  # isstdcnt
    f.write(struct.pack(">I", 0))  # leapcnt
    f.write(struct.pack(">I", 0))  # timecnt
    f.write(struct.pack(">I", 0))  # typecnt
    f.write(b"\x00\x01")   # Partial charcnt
print("✓ partial_header_abbrev.tzif - Truncated during abbrev chars read")

# File 23: Valid TZif Version 1 file (most tests use V2+)
# This should trigger the V1-only parsing path (line 584)
with open("valid_v1.tzif", "wb") as f:
    # Version 1 header
    f.write(b"TZif\x00")   # Magic + Version 1 (null byte)
    f.write(b"\x00" * 15)  # Reserved

    # Header counts
    f.write(struct.pack(">I", 0))  # isutcnt
    f.write(struct.pack(">I", 0))  # isstdcnt
    f.write(struct.pack(">I", 0))  # leapcnt
    f.write(struct.pack(">I", 1))  # timecnt - 1 transition
    f.write(struct.pack(">I", 1))  # typecnt - 1 type
    f.write(struct.pack(">I", 4))  # charcnt - 4 chars for "UTC\0"

    # Version 1 data (32-bit)
    f.write(struct.pack(">i", 0))           # Transition time (epoch 0)
    f.write(struct.pack(">B", 0))           # Type index
    f.write(struct.pack(">i", 0))           # UTC offset
    f.write(struct.pack(">B", 0))           # DST flag
    f.write(struct.pack(">B", 0))           # Abbrev index
    f.write(b"UTC\x00")                      # Abbreviation
print("✓ valid_v1.tzif - Valid Version 1 file (tests V1-only code path)")

# File 24: TZif V2 with POSIX TZ string
# Most current tests don't include POSIX TZ strings
with open("v2_with_posix_tz.tzif", "wb") as f:
    # V1 header (required for V2+ files)
    f.write(b"TZif\x00")
    f.write(b"\x00" * 15)
    f.write(struct.pack(">I", 0))  # isutcnt
    f.write(struct.pack(">I", 0))  # isstdcnt
    f.write(struct.pack(">I", 0))  # leapcnt
    f.write(struct.pack(">I", 0))  # timecnt
    f.write(struct.pack(">I", 0))  # typecnt
    f.write(struct.pack(">I", 0))  # charcnt

    # V2 header
    f.write(b"TZif2")
    f.write(b"\x00" * 15)
    f.write(struct.pack(">I", 0))  # isutcnt
    f.write(struct.pack(">I", 0))  # isstdcnt
    f.write(struct.pack(">I", 0))  # leapcnt
    f.write(struct.pack(">I", 1))  # timecnt
    f.write(struct.pack(">I", 1))  # typecnt
    f.write(struct.pack(">I", 4))  # charcnt

    # V2 data (64-bit)
    f.write(struct.pack(">q", 0))           # Transition time (64-bit)
    f.write(struct.pack(">B", 0))           # Type index
    f.write(struct.pack(">i", 0))           # UTC offset
    f.write(struct.pack(">B", 0))           # DST flag
    f.write(struct.pack(">B", 0))           # Abbrev index
    f.write(b"UTC\x00")                      # Abbreviation

    # POSIX TZ string (new!)
    f.write(b"\nUTC0\n")                    # POSIX TZ string with newline delimiters
print("✓ v2_with_posix_tz.tzif - V2 file with POSIX TZ string")

# File 25: Partial read during 64-bit transition time (V2 data section)
with open("partial_v2_transition.tzif", "wb") as f:
    # V1 header (minimal)
    f.write(b"TZif\x00")
    f.write(b"\x00" * 15)
    f.write(struct.pack(">I", 0) * 6)  # All counts = 0

    # V2 header
    f.write(b"TZif2")
    f.write(b"\x00" * 15)
    f.write(struct.pack(">I", 0))  # isutcnt
    f.write(struct.pack(">I", 0))  # isstdcnt
    f.write(struct.pack(">I", 0))  # leapcnt
    f.write(struct.pack(">I", 1))  # timecnt - says there's 1 transition
    f.write(struct.pack(">I", 1))  # typecnt
    f.write(struct.pack(">I", 4))  # charcnt

    # V2 data - only write 4 bytes of the 64-bit transition time
    f.write(b"\x00\x00\x00\x01")   # Partial 64-bit int (should be 8 bytes)
print("✓ partial_v2_transition.tzif - Truncated during 64-bit transition read")

# File 26: Partial read during single byte (type index)
with open("partial_type_index.tzif", "wb") as f:
    f.write(b"TZif2")
    f.write(b"\x00" * 15)
    f.write(struct.pack(">I", 0))  # isutcnt
    f.write(struct.pack(">I", 0))  # isstdcnt
    f.write(struct.pack(">I", 0))  # leapcnt
    f.write(struct.pack(">I", 1))  # timecnt
    f.write(struct.pack(">I", 1))  # typecnt
    f.write(struct.pack(">I", 4))  # charcnt

    # Write complete transition time but truncate before type index
    # Note: For Version 2 in header-only, we're still in V1 section
    f.write(struct.pack(">i", 0))  # 32-bit transition (V1 section)
    # Missing: type index byte - file ends here
print("✓ partial_type_index.tzif - Truncated before type index byte")

print("=" * 60)
print(f"Generated 10 additional test files (17-26)")
print(f"Total test files: 26 (16 original + 10 new)")
print("\nNew coverage targets:")
print("  - Partial reads during header parsing (6 files)")
print("  - Version 1 file parsing (1 file)")
print("  - POSIX TZ string parsing (1 file)")
print("  - Partial 64-bit integer reads (1 file)")
print("  - Partial byte reads (1 file)")
