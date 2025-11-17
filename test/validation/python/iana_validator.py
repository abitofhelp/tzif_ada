#!/usr/bin/env python3
"""
IANA Timezone Validation Test Harness - Python Reference Implementation

This script provides a reference implementation using Python's zoneinfo module
for validating ZoneInfo (Ada) against the authoritative IANA timezone database.

Requirements:
    - Python 3.9+ (for zoneinfo module)
    - tzdata 2025b (system or package)

Usage:
    python3 iana_validator.py get_offset America/New_York 2024-03-10T02:30:00
    python3 iana_validator.py list_zones
    python3 iana_validator.py get_version
"""

import sys
import json
from datetime import datetime, timezone
from zoneinfo import ZoneInfo, available_timezones
from typing import Dict, Any, Optional
import os


def get_tzdata_version() -> str:
    """Get the tzdata version being used by Python."""
    # Check for +VERSION file in system zoneinfo
    version_paths = [
        '/usr/share/zoneinfo/+VERSION',
        '/var/db/timezone/zoneinfo/+VERSION'
    ]

    for path in version_paths:
        if os.path.exists(path):
            with open(path, 'r') as f:
                return f.read().strip()

    # Try tzdata package if installed
    try:
        import tzdata
        return tzdata.__version__
    except ImportError:
        pass

    return "unknown"


def get_utc_offset(zone_name: str, dt_iso: str) -> Dict[str, Any]:
    """
    Get UTC offset for a timezone at a specific time.

    Args:
        zone_name: IANA timezone identifier (e.g., "America/New_York")
        dt_iso: ISO 8601 datetime string (e.g., "2024-03-10T02:30:00")

    Returns:
        Dictionary with:
            - success: bool
            - offset_seconds: int (if success)
            - offset_str: str (if success, e.g., "-05:00")
            - error: str (if not success)
    """
    try:
        # Parse datetime as naive (local to the timezone)
        dt = datetime.fromisoformat(dt_iso)

        # Load timezone
        tz = ZoneInfo(zone_name)

        # Localize the datetime
        dt_local = dt.replace(tzinfo=tz)

        # Get UTC offset
        offset = dt_local.utcoffset()
        if offset is None:
            return {
                "success": False,
                "error": "Unable to determine UTC offset"
            }

        offset_seconds = int(offset.total_seconds())

        # Format offset as +/-HH:MM
        hours, remainder = divmod(abs(offset_seconds), 3600)
        minutes = remainder // 60
        sign = '+' if offset_seconds >= 0 else '-'
        offset_str = f"{sign}{hours:02d}:{minutes:02d}"

        return {
            "success": True,
            "offset_seconds": offset_seconds,
            "offset_str": offset_str,
            "tzname": dt_local.tzname()
        }

    except Exception as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": type(e).__name__
        }


def is_ambiguous_time(zone_name: str, dt_iso: str) -> Dict[str, Any]:
    """
    Check if a local time is ambiguous (occurs twice due to DST fall back).

    Returns:
        Dictionary with:
            - success: bool
            - is_ambiguous: bool (if success)
            - error: str (if not success)
    """
    try:
        dt = datetime.fromisoformat(dt_iso)
        tz = ZoneInfo(zone_name)

        # Try to create with fold=0 and fold=1
        dt_fold0 = dt.replace(tzinfo=tz, fold=0)
        dt_fold1 = dt.replace(tzinfo=tz, fold=1)

        # If offsets differ, time is ambiguous
        offset0 = dt_fold0.utcoffset()
        offset1 = dt_fold1.utcoffset()

        is_ambiguous = (offset0 != offset1)

        return {
            "success": True,
            "is_ambiguous": is_ambiguous,
            "offset_fold0": int(offset0.total_seconds()) if offset0 else None,
            "offset_fold1": int(offset1.total_seconds()) if offset1 else None
        }

    except Exception as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": type(e).__name__
        }


def is_gap_time(zone_name: str, dt_iso: str) -> Dict[str, Any]:
    """
    Check if a local time is in a gap (never exists due to DST spring forward).

    Python's zoneinfo doesn't directly expose gap detection, so we check if
    localizing the time results in a different local time.

    Returns:
        Dictionary with:
            - success: bool
            - is_gap: bool (if success)
            - error: str (if not success)
    """
    try:
        dt = datetime.fromisoformat(dt_iso)
        tz = ZoneInfo(zone_name)

        # Localize the datetime
        dt_local = dt.replace(tzinfo=tz)

        # Convert to UTC and back
        dt_utc = dt_local.astimezone(timezone.utc)
        dt_back = dt_utc.astimezone(tz)

        # If the local time changed, it was in a gap
        is_gap = (dt.replace(tzinfo=None) != dt_back.replace(tzinfo=None))

        return {
            "success": True,
            "is_gap": is_gap,
            "original_time": dt_iso,
            "normalized_time": dt_back.replace(tzinfo=None).isoformat()
        }

    except Exception as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": type(e).__name__
        }


def list_all_timezones() -> Dict[str, Any]:
    """
    List all available timezone identifiers.

    Returns:
        Dictionary with:
            - success: bool
            - count: int
            - zones: list of str (sorted)
    """
    try:
        zones = sorted(list(available_timezones()))
        return {
            "success": True,
            "count": len(zones),
            "zones": zones
        }
    except Exception as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": type(e).__name__
        }


def find_zones_by_pattern(pattern: str) -> Dict[str, Any]:
    """
    Find timezone identifiers matching a pattern (substring search).

    Returns:
        Dictionary with:
            - success: bool
            - count: int
            - zones: list of str
    """
    try:
        all_zones = available_timezones()
        matching = sorted([z for z in all_zones if pattern in z])
        return {
            "success": True,
            "count": len(matching),
            "zones": matching
        }
    except Exception as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": type(e).__name__
        }


def find_zones_by_region(region: str) -> Dict[str, Any]:
    """
    Find timezone identifiers in a specific region.

    Returns:
        Dictionary with:
            - success: bool
            - count: int
            - zones: list of str
    """
    try:
        all_zones = available_timezones()
        matching = sorted([z for z in all_zones if z.startswith(region + "/")])
        return {
            "success": True,
            "count": len(matching),
            "zones": matching
        }
    except Exception as e:
        return {
            "success": False,
            "error": str(e),
            "error_type": type(e).__name__
        }


def main():
    """Main entry point for command-line usage."""
    if len(sys.argv) < 2:
        print(json.dumps({
            "success": False,
            "error": "Usage: iana_validator.py <command> [args...]",
            "commands": [
                "get_version",
                "get_offset <zone> <datetime>",
                "is_ambiguous <zone> <datetime>",
                "is_gap <zone> <datetime>",
                "list_zones",
                "find_pattern <pattern>",
                "find_region <region>"
            ]
        }, indent=2))
        sys.exit(1)

    command = sys.argv[1]
    result = {"command": command}

    if command == "get_version":
        result.update({
            "success": True,
            "tzdata_version": get_tzdata_version(),
            "python_version": sys.version
        })

    elif command == "get_offset":
        if len(sys.argv) != 4:
            result.update({
                "success": False,
                "error": "Usage: get_offset <zone> <datetime>"
            })
        else:
            zone = sys.argv[2]
            dt = sys.argv[3]
            result.update(get_utc_offset(zone, dt))

    elif command == "is_ambiguous":
        if len(sys.argv) != 4:
            result.update({
                "success": False,
                "error": "Usage: is_ambiguous <zone> <datetime>"
            })
        else:
            zone = sys.argv[2]
            dt = sys.argv[3]
            result.update(is_ambiguous_time(zone, dt))

    elif command == "is_gap":
        if len(sys.argv) != 4:
            result.update({
                "success": False,
                "error": "Usage: is_gap <zone> <datetime>"
            })
        else:
            zone = sys.argv[2]
            dt = sys.argv[3]
            result.update(is_gap_time(zone, dt))

    elif command == "list_zones":
        result.update(list_all_timezones())

    elif command == "find_pattern":
        if len(sys.argv) != 3:
            result.update({
                "success": False,
                "error": "Usage: find_pattern <pattern>"
            })
        else:
            pattern = sys.argv[2]
            result.update(find_zones_by_pattern(pattern))

    elif command == "find_region":
        if len(sys.argv) != 3:
            result.update({
                "success": False,
                "error": "Usage: find_region <region>"
            })
        else:
            region = sys.argv[2]
            result.update(find_zones_by_region(region))

    else:
        result.update({
            "success": False,
            "error": f"Unknown command: {command}"
        })

    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()
