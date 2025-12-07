#!/usr/bin/env python3
"""Generate one more test file - transition type index out of bounds"""

import struct

# 16. Transition type index out of bounds
with open("type_index_out_of_bounds.tzif", "wb") as f:
    f.write(b"TZif2")
    f.write(b"\x00" * 15)
    f.write(struct.pack(">I", 0) * 3)
    f.write(struct.pack(">I", 2))  # 2 transitions
    f.write(struct.pack(">I", 1))  # 1 type (only index 0 valid)
    f.write(struct.pack(">I", 4))  # 4 chars
    # Version 1 data
    f.write(struct.pack(">i", 0))    # transition 1 time
    f.write(struct.pack(">i", 1000)) # transition 2 time
    f.write(b"\x00")  # transition 1 -> type index 0 (OK)
    f.write(b"\x05")  # transition 2 -> type index 5 (OUT OF BOUNDS!)
    # Only 1 type info
    f.write(struct.pack(">i", 0))  # UTC offset
    f.write(b"\x00")  # isDST
    f.write(b"\x00")  # abbrev index
    f.write(b"UTC\x00")

print("Generated: type_index_out_of_bounds.tzif - Transition points to non-existent type")
