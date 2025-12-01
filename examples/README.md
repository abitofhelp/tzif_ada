# TZif Examples

This directory contains 11 working examples demonstrating the TZif library's capabilities.

## Building Examples

```bash
# Build all examples
alr build

# Or use gprbuild directly
alr exec -- gprbuild -P examples/examples.gpr
```

Executables will be placed in `../bin/examples/`.

## Running Examples

### Quick Start Examples

These examples provide working demonstrations with actual output:

1. **find_by_id** - Find and parse timezone by exact ID match
   ```bash
   ./bin/examples/find_by_id
   ```
   Demonstrates looking up "America/Phoenix" and displaying timezone metadata.

2. **find_my_id** - Detect and display local system timezone
   ```bash
   ./bin/examples/find_my_id
   ```
   Shows how to detect the system's configured timezone.

3. **get_transition_at_epoch** - Query timezone offset at specific time
   ```bash
   ./bin/examples/get_transition_at_epoch
   ```
   Demonstrates querying UTC offset for a given epoch (e.g., 2024-07-01).

### Additional Examples

The following examples are executable but refer to the test suite for detailed usage patterns:

- **discover_sources** - Scan filesystem for timezone source files
- **find_by_pattern** - Find timezones matching a glob pattern
- **find_by_region** - Find all timezones in a geographic region
- **find_by_regex** - Find timezones matching a regular expression
- **get_version** - Get TZif library version information
- **list_all_zones** - List all available timezone IDs
- **load_source** - Load and parse a TZif source file
- **validate_source** - Validate TZif source file format

For detailed usage examples of these features, see the test suite:
- Integration tests: `../test/integration/`
- Unit tests: `../test/unit/`

## Key Features Demonstrated

- **Result Monad Pattern**: All examples use `Result[T, Error]` for error handling
- **NO EXCEPTIONS**: Functional error handling with explicit error paths
- **TZif.API Facade**: Public API for common operations
- **Railway-Oriented Programming**: Chained Result operations

## Example Structure

Each working example follows this pattern:

```ada
with TZif.API;
with TZif.Domain.Value_Object.Zone_Id.Result;

procedure Example is
   use TZif.API;

   -- Validate Zone_Id using Result monad
   Zone_Id_Result : constant Zone_Id_Result.Result :=
      Validate_Zone_Id("America/New_York");
begin
   if Is_Ok(Zone_Id_Result) then
      -- Use the validated Zone_Id
      Zone_Id : constant Zone_Id_Type := Value(Zone_Id_Result);
      -- ... perform operations ...
   else
      -- Handle error explicitly
      Error : constant Error_Type := Error_Info(Zone_Id_Result);
      Put_Line("Error: " & To_String(Error.Message));
   end if;
end Example;
```

## See Also

- [Main README](../README.md) - Library overview and installation
- [Software Requirements Specification](../docs/formal/software_requirements_specification.md)
- [Software Design Specification](../docs/formal/software_design_specification.md)
- [Software Test Guide](../docs/formal/software_test_guide.md)
- [Test Suite](../test/) - Comprehensive usage examples

## License

Copyright © 2025 Michael Gardner, A Bit of Help, Inc.

Licensed under the BSD-3-Clause License. See [../LICENSE](../LICENSE) for details.
