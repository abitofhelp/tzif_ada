#!/usr/bin/env python3
"""Generate additional invalid TZif test files"""

import struct

# 11. File with only magic (no version or anything else)
with open("only_magic.tzif", "wb") as f:
    f.write(b"TZif")  # Just magic, nothing else

# 12. Reserved bytes not zero (should be ignored but let's test)
with open("nonzero_reserved.tzif", "wb") as f:
    f.write(b"TZif2")
    f.write(b"\xFF" * 15)  # Reserved should be zero
    f.write(b"\x00" * 24)

# 13. Single byte file
with open("single_byte.tzif", "wb") as f:
    f.write(b"T")

# 14. Correct magic but version 1 with insufficient data
with open("v1_no_data.tzif", "wb") as f:
    f.write(b"TZif\x00")  # Version 1
    f.write(b"\x00" * 15)  # Reserved
    f.write(struct.pack(">I", 0))  # isutcnt
    f.write(struct.pack(">I", 0))  # isstdcnt  
    f.write(struct.pack(">I", 0))  # leapcnt
    f.write(struct.pack(">I", 1))  # timecnt = 1
    f.write(struct.pack(">I", 0))  # typecnt = 0 (INVALID: need at least 1)
    f.write(struct.pack(">I", 0))  # charcnt

# 15. Abbreviation index out of bounds
with open("abbr_out_of_bounds.tzif", "wb") as f:
    f.write(b"TZif2")
    f.write(b"\x00" * 15)
    f.write(struct.pack(">I", 0) * 3)
    f.write(struct.pack(">I", 1))  # 1 transition
    f.write(struct.pack(">I", 1))  # 1 type
    f.write(struct.pack(">I", 3))  # 3 chars ("UTC")
    # Version 1 data
    f.write(struct.pack(">i", 0))  # transition time
    f.write(b"\x00")  # type index
    f.write(struct.pack(">i", 0))  # UTC offset
    f.write(b"\x00")  # isDST
    f.write(b"\x99")  # abbrev index = 153 (way out of bounds!)
    f.write(b"UTC")  # abbreviations

print("Generated 5 more invalid test files:")
print(" 11. only_magic.tzif - Just 'TZif' with nothing else")
print(" 12. nonzero_reserved.tzif - Reserved bytes are 0xFF")
print(" 13. single_byte.tzif - Just 'T'")
print(" 14. v1_no_data.tzif - Claims types but typecnt=0")
print(" 15. abbr_out_of_bounds.tzif - Abbreviation index invalid")
