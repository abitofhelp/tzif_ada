#!/usr/bin/env python3
"""Generate invalid TZif test files for parser error testing"""

import struct
import os

# Create output directory if needed
os.makedirs(".", exist_ok=True)

# 1. Invalid version byte (0xFF instead of '2', '3', or '4')
with open("invalid_version.tzif", "wb") as f:
    f.write(b"TZif\xFF")  # Magic + invalid version
    f.write(b"\x00" * 15)  # Reserved bytes
    f.write(b"\x00" * 24)  # Rest of header (zeros)

# 2. Valid header with count mismatch
# Header says 5 transitions, but we only provide 2
with open("count_mismatch.tzif", "wb") as f:
    # Version 1 header
    f.write(b"TZif2")      # Magic + version
    f.write(b"\x00" * 15)  # Reserved
    f.write(struct.pack(">I", 0))  # isutcnt
    f.write(struct.pack(">I", 0))  # isstdcnt
    f.write(struct.pack(">I", 0))  # leapcnt
    f.write(struct.pack(">I", 5))  # timecnt (says 5!)
    f.write(struct.pack(">I", 1))  # typecnt
    f.write(struct.pack(">I", 4))  # charcnt
    # Version 1 data (32-bit) - only provide 2 transitions instead of 5
    f.write(struct.pack(">i", 0))  # transition 1
    f.write(struct.pack(">i", 1000))  # transition 2
    # Missing 3 transitions!

# 3. Truncated in middle of transition data
with open("truncated_transition.tzif", "wb") as f:
    f.write(b"TZif2")
    f.write(b"\x00" * 15)
    f.write(struct.pack(">I", 0))  # isutcnt
    f.write(struct.pack(">I", 0))  # isstdcnt
    f.write(struct.pack(">I", 0))  # leapcnt
    f.write(struct.pack(">I", 3))  # timecnt = 3
    f.write(struct.pack(">I", 1))  # typecnt
    f.write(struct.pack(">I", 4))  # charcnt
    # Provide only 1.5 transitions (truncate mid-integer)
    f.write(struct.pack(">i", 0))
    f.write(b"\x00\x01")  # Only 2 bytes of 4-byte integer

# 4. Header-only file (no data section)
with open("header_only.tzif", "wb") as f:
    f.write(b"TZif2")
    f.write(b"\x00" * 15)
    f.write(struct.pack(">I", 0))  # isutcnt
    f.write(struct.pack(">I", 0))  # isstdcnt
    f.write(struct.pack(">I", 0))  # leapcnt
    f.write(struct.pack(">I", 2))  # timecnt = 2
    f.write(struct.pack(">I", 1))  # typecnt = 1
    f.write(struct.pack(">I", 4))  # charcnt = 4
    # NO DATA - file ends after header

# 5. Corrupted integers (non-big-endian or garbage)
with open("malformed_integers.tzif", "wb") as f:
    f.write(b"TZif2")
    f.write(b"\x00" * 15)
    # Write garbage for count fields
    f.write(b"\xFF\xFF\xFF\xFF")  # isutcnt = -1 (invalid)
    f.write(b"\x80\x00\x00\x00")  # isstdcnt = very large
    f.write(b"\x00\x00\x00\x00")  # leapcnt
    f.write(b"\x00\x00\x00\x01")  # timecnt
    f.write(b"\x00\x00\x00\x01")  # typecnt
    f.write(b"\x00\x00\x00\x04")  # charcnt

# 6. Zero counts but with data (logical inconsistency)
with open("zero_counts_with_data.tzif", "wb") as f:
    f.write(b"TZif2")
    f.write(b"\x00" * 15)
    f.write(struct.pack(">I", 0))  # all counts = 0
    f.write(struct.pack(">I", 0))
    f.write(struct.pack(">I", 0))
    f.write(struct.pack(">I", 0))  # timecnt = 0 (no transitions)
    f.write(struct.pack(">I", 0))  # typecnt = 0 (no types!)
    f.write(struct.pack(">I", 0))  # charcnt = 0
    # But provide data anyway (should be ignored or error)
    f.write(b"GARBAGE DATA HERE" * 10)

# 7. Version 2 marker but version 1 data size
with open("version_mismatch.tzif", "wb") as f:
    # Version 1 header with minimal data
    f.write(b"TZif\x00")  # Version 0/1
    f.write(b"\x00" * 15)
    f.write(struct.pack(">I", 0) * 3)
    f.write(struct.pack(">I", 1))  # 1 transition
    f.write(struct.pack(">I", 1))  # 1 type
    f.write(struct.pack(">I", 4))  # 4 chars
    f.write(struct.pack(">i", 0))  # transition time
    f.write(b"\x00")  # transition type index
    f.write(struct.pack(">i", 0))  # UTC offset
    f.write(b"\x00")  # is DST
    f.write(b"\x00")  # abbreviation index
    f.write(b"UTC\x00")  # abbreviation
    # Now Version 2 header but wrong!
    f.write(b"TZif3")  # Version 3 (but provide version 1-style data)
    # ... truncated, inconsistent

# 8. Excessive counts (potential DoS)
with open("excessive_counts.tzif", "wb") as f:
    f.write(b"TZif2")
    f.write(b"\x00" * 15)
    f.write(struct.pack(">I", 0))
    f.write(struct.pack(">I", 0))
    f.write(struct.pack(">I", 0))
    f.write(struct.pack(">I", 0x7FFFFFFF))  # timecnt = 2 billion!
    f.write(struct.pack(">I", 0x7FFFFFFF))  # typecnt = 2 billion!
    f.write(struct.pack(">I", 0x7FFFFFFF))  # charcnt = 2 billion!
    # No actual data

# 9. Negative counts (impossible but test integer handling)
with open("negative_counts.tzif", "wb") as f:
    f.write(b"TZif2")
    f.write(b"\x00" * 15)
    f.write(struct.pack(">i", -1))  # signed -1 (0xFFFFFFFF unsigned)
    f.write(struct.pack(">i", -5))
    f.write(struct.pack(">i", -10))
    f.write(struct.pack(">i", 1))
    f.write(struct.pack(">i", 1))
    f.write(struct.pack(">i", 4))

# 10. Future version number
with open("future_version.tzif", "wb") as f:
    f.write(b"TZif9")  # Version 9 (doesn't exist yet)
    f.write(b"\x00" * 15)
    f.write(b"\x00" * 24)  # Rest of header

print("Generated 10 invalid TZif test files:")
print("  1. invalid_version.tzif - Invalid version byte (0xFF)")
print("  2. count_mismatch.tzif - Header says 5 transitions, provides 2")
print("  3. truncated_transition.tzif - File ends mid-integer")
print("  4. header_only.tzif - Valid header but no data")
print("  5. malformed_integers.tzif - Corrupted count fields")
print("  6. zero_counts_with_data.tzif - Claims no data but has data")
print("  7. version_mismatch.tzif - Version 1/3 inconsistency")
print("  8. excessive_counts.tzif - Counts = 2 billion (DoS test)")
print("  9. negative_counts.tzif - Negative count values")
print(" 10. future_version.tzif - Version 9 (unsupported)")
