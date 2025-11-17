# TZif Development Roadmap

**Version:** 1.0.0-rc1  
**Date:** November 16, 2025  
**SPDX-License-Identifier:** BSD-3-Clause  
**License File:** See the LICENSE file in the project root.  
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.  
**Status:** Unreleased  

---

## Overview

This roadmap outlines planned enhancements and features for future TZif library releases. Items are prioritized based on user impact, architectural considerations, and platform support goals.

---

## v1.1.0 - Platform Expansion & Performance (Q1 2026)

**Focus**: Complete cross-platform support and add performance monitoring

### High Priority

#### 1. Fix GitHub Actions CI Workflow
**Status**: Known Issues
**Effort**: Medium
**Impact**: High

- Review and fix known issues in `.github/workflows/ci.yml`
- Update CI workflow to match current build process
- Verify all test suites run correctly in CI
- Add coverage reporting to CI pipeline
- Test across multiple platforms (Linux, macOS, Windows)
- Update CI documentation

**Rationale**: Continuous integration is critical for maintaining code quality and catching regressions. Current CI has known issues that prevent it from validating pull requests and releases properly.

#### 2. Complete Windows Platform Support
**Status**: Engineered but not enabled
**Effort**: Medium
**Impact**: High

- Complete testing of `infrastructure-platform-windows.ads/adb`
- Verify TZif file path resolution on Windows
- Test timezone source discovery on Windows
- Add Windows-specific CI testing
- Update documentation to reflect full Windows support

**Rationale**: Windows platform code exists but needs thorough testing and validation before release. Critical for broad platform adoption.

#### 3. Add Cache Hit/Miss Statistics
**Status**: Proposed
**Effort**: Low
**Impact**: Medium

- Add statistics tracking to `Infrastructure.Cache.Zone_Cache`
- Implement cache hit/miss counters
- Add cache efficiency metrics (hit rate, eviction count)
- Expose statistics via query interface
- Document performance monitoring best practices

**Rationale**: Production deployments need visibility into cache performance for tuning and monitoring. Helps users optimize cache size and usage patterns.

### Medium Priority

#### 4. Path Traversal Protection Audit
**Status**: Review Required
**Effort**: Low
**Impact**: High (Security)

- Audit `Infrastructure.Paths.Canonical` implementation
- Verify protection against ".." path traversal attacks
- Add security-focused tests for path canonicalization
- Document security guarantees in API
- Consider adding path validation preconditions

**Rationale**: Security hardening for production use. Path canonicalization must be proven secure against directory traversal attacks.

#### 5. TZif File Size Limits (DOS Protection)
**Status**: Proposed
**Effort**: Low
**Impact**: Medium (Security)

- Add configurable maximum file size for TZif parsing
- Implement early rejection of oversized files
- Add configuration via `TZif_Config` package
- Document DOS protection mechanisms
- Add tests with large/malicious TZif files

**Rationale**: Prevents denial-of-service attacks via extremely large TZif files that could consume excessive memory during parsing.

---

## v1.2.0 - Testing & Quality Enhancements (Q2 2026)

**Focus**: Enhanced test coverage and performance validation

### Medium Priority

#### 6. Property-Based Testing for Parser
**Status**: Proposed
**Effort**: Medium
**Impact**: Medium

- Integrate property-based testing framework (consider adapting QuickCheck concepts)
- Generate randomized TZif files for parser testing
- Test parser invariants (round-trip, error boundaries)
- Catch edge cases not covered by example-based tests
- Document property testing approach

**Rationale**: Property-based testing excels at finding edge cases in parsers. Would significantly increase confidence in TZif parser correctness across all possible inputs.

#### 7. Performance Benchmark Suite
**Status**: Proposed
**Effort**: Medium
**Impact**: Medium

- Create benchmark suite for performance claims
- Verify O(1) lookup performance characteristics
- Measure cache hit/miss performance impact
- Benchmark parse times for different TZif versions
- Add regression testing for performance
- Document expected performance characteristics

**Rationale**: Current documentation claims O(1) performance. Benchmarks would validate these claims and prevent performance regressions in future releases.

---

## v2.0.0 - API Evolution & Advanced Features (Q3 2026)

**Focus**: Backward-compatible API enhancements and advanced timezone features

### Low Priority

#### 8. Port Versioning Strategy
**Status**: Planning
**Effort**: Medium
**Impact**: Low (Future-proofing)

- Document strategy for evolving port interfaces
- Consider versioned port packages (`Application.Port.V2.Inbound.Find_By_Id`)
- Plan deprecation policy for old ports
- Ensure backward compatibility path
- Create migration guide template

**Rationale**: Establishes clear path for API evolution without breaking existing users. Important for long-term library stability.

#### 9. Error Context Enrichment
**Status**: Proposed
**Effort**: Medium
**Impact**: Low

- Add operation context to error types
- Consider adding stack trace information
- Implement error chaining (cause tracking)
- Add source location information (file, line)
- Document debugging best practices

**Rationale**: Enhanced error diagnostics would aid production debugging. Useful for troubleshooting complex error scenarios.

#### 10. Advanced Timezone Queries
**Status**: Proposed
**Effort**: High
**Impact**: Low

- Add timezone offset queries (find zones with specific UTC offset)
- Implement DST transition queries (find next DST change)
- Add historical timezone data queries
- Support fuzzy matching for timezone names
- Consider adding timezone alias resolution

**Rationale**: Advanced queries would support more sophisticated timezone applications. Nice-to-have features for specialized use cases.

#### 11. Controlled Types for Resource Management
**Status**: Consideration
**Effort**: Low
**Impact**: Low

- Evaluate need for `Ada.Finalization.Controlled` types
- Consider for file handle management
- Document current resource management approach
- Plan for future cleanup actions if needed

**Rationale**: Current design doesn't need cleanup actions, but future enhancements might benefit from controlled types.

---

## Future Considerations (Beyond v2.0.0)

### Research & Exploration

- **IANA Data Updates**: Automatic timezone database updates
- **Network Protocol Support**: Fetch timezone data from remote sources
- **Binary Cache Format**: Faster cache loading than JSON
- **Compressed TZif Support**: Handle compressed timezone files
- **Leap Second Prediction**: Future leap second estimation
- **Parallel Zone Loading**: Concurrent TZif parsing for startup

---

## Contributing to the Roadmap

Community feedback shapes TZif's development. If you have suggestions for the roadmap:

1. **File an Issue**: Propose new features via GitHub Issues
2. **Join Discussions**: Participate in roadmap planning discussions
3. **Submit PRs**: Contribute implementations for roadmap items
4. **Share Use Cases**: Help us understand your timezone needs

**Contact**: support@abitofhelp.com

---

## Roadmap Status Legend

- **Proposed**: Idea stage, not yet committed
- **Planning**: Design and specification phase
- **In Progress**: Active development
- **Review Required**: Needs technical assessment
- **Completed**: Shipped in release

---

## Version History

| Version | Release Date | Focus Area |
|---------|-------------|------------|
| v1.0.0  | Nov 2025    | Initial release (Linux, macOS, BSD) |
| v1.1.0  | Q1 2026     | Windows support + performance monitoring |
| v1.2.0  | Q2 2026     | Enhanced testing and benchmarks |
| v2.0.0  | Q3 2026     | API evolution and advanced features |

---

**Last Updated**: November 16, 2025
**Next Review**: January 2026
