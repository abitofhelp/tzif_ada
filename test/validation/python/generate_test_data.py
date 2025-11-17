#!/usr/bin/env python3
"""
Generate Comprehensive Test Data for IANA Validation

Creates extensive test data covering:
- All 600+ timezones
- Multiple test dates (historical, current, future)
- DST transitions for each zone
- Edge cases and boundary conditions

Output: JSON file with all test cases
"""

import json
import sys
from datetime import datetime, timedelta, timezone
from zoneinfo import ZoneInfo, available_timezones
from typing import List, Dict, Any


def generate_test_dates() -> List[str]:
    """Generate a comprehensive set of test dates."""
    dates = []

    # Historical dates
    dates.append("1970-01-01T00:00:00")  # Unix epoch
    dates.append("1980-06-15T12:00:00")  # Pre-2000
    dates.append("1999-12-31T23:59:59")  # Y2K boundary

    # 2000s
    dates.append("2000-01-01T00:00:00")  # Y2K
    dates.append("2010-07-04T12:00:00")  # Mid-2010s

    # Recent years
    dates.append("2020-01-15T12:00:00")  # Winter
    dates.append("2020-07-15T12:00:00")  # Summer
    dates.append("2023-03-15T12:00:00")  # Spring
    dates.append("2023-09-15T12:00:00")  # Fall

    # Current year
    dates.append("2024-01-15T12:00:00")  # Winter
    dates.append("2024-04-15T12:00:00")  # Spring
    dates.append("2024-07-15T12:00:00")  # Summer
    dates.append("2024-10-15T12:00:00")  # Fall

    # DST transition dates for US (2024)
    dates.append("2024-03-10T01:30:00")  # Before spring forward
    dates.append("2024-03-10T02:30:00")  # During spring forward (gap)
    dates.append("2024-03-10T03:30:00")  # After spring forward
    dates.append("2024-11-03T00:30:00")  # Before fall back
    dates.append("2024-11-03T01:30:00")  # During fall back (ambiguous)
    dates.append("2024-11-03T02:30:00")  # After fall back

    # Future dates
    dates.append("2025-06-15T12:00:00")
    dates.append("2030-01-01T00:00:00")
    dates.append("2050-12-31T23:59:59")

    return dates


def get_zone_info(zone_name: str, test_dates: List[str]) -> Dict[str, Any]:
    """Get comprehensive info for a timezone."""
    try:
        tz = ZoneInfo(zone_name)

        results = {
            "zone": zone_name,
            "test_dates": []
        }

        for date_str in test_dates:
            try:
                dt = datetime.fromisoformat(date_str)
                dt_local = dt.replace(tzinfo=tz)

                offset = dt_local.utcoffset()
                offset_seconds = int(offset.total_seconds()) if offset else None

                # Check if ambiguous
                dt_fold0 = dt.replace(tzinfo=tz, fold=0)
                dt_fold1 = dt.replace(tzinfo=tz, fold=1)
                is_ambiguous = (dt_fold0.utcoffset() != dt_fold1.utcoffset())

                # Check if gap
                dt_utc = dt_local.astimezone(timezone.utc)
                dt_back = dt_utc.astimezone(tz)
                is_gap = (dt.replace(tzinfo=None) != dt_back.replace(tzinfo=None))

                results["test_dates"].append({
                    "datetime": date_str,
                    "offset_seconds": offset_seconds,
                    "tzname": dt_local.tzname(),
                    "is_ambiguous": is_ambiguous,
                    "is_gap": is_gap
                })
            except Exception as e:
                results["test_dates"].append({
                    "datetime": date_str,
                    "error": str(e)
                })

        return results

    except Exception as e:
        return {
            "zone": zone_name,
            "error": str(e)
        }


def main():
    print("Generating comprehensive test data...", file=sys.stderr)

    # Get all timezones
    all_zones = sorted(list(available_timezones()))
    print(f"Total timezones: {len(all_zones)}", file=sys.stderr)

    # Generate test dates
    test_dates = generate_test_dates()
    print(f"Test dates per zone: {len(test_dates)}", file=sys.stderr)

    total_tests = len(all_zones) * len(test_dates)
    print(f"Total test cases: {total_tests}", file=sys.stderr)

    # Generate test data for all zones
    test_data = {
        "generated": datetime.now().isoformat(),
        "tzdata_version": "2025b",
        "total_zones": len(all_zones),
        "test_dates_per_zone": len(test_dates),
        "total_test_cases": total_tests,
        "zones": []
    }

    for i, zone_name in enumerate(all_zones, 1):
        if i % 50 == 0:
            print(f"Processing zone {i}/{len(all_zones)}...", file=sys.stderr)

        zone_info = get_zone_info(zone_name, test_dates)
        test_data["zones"].append(zone_info)

    # Output JSON
    print(json.dumps(test_data, indent=2))

    print(f"\nGeneration complete!", file=sys.stderr)
    print(f"Total zones: {len(test_data['zones'])}", file=sys.stderr)
    print(f"Total test cases: {total_tests}", file=sys.stderr)


if __name__ == "__main__":
    main()
