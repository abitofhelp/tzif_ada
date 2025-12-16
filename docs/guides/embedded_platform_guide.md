# Embedded Platform Guide

**Version:** 3.0.2<br>
**Date:** 2025-12-16<br>
**SPDX-License-Identifier:** BSD-3-Clause<br>
**License File:** See the LICENSE file in the project root<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.<br>
**Status:** Released

---

## Overview

This guide explains how to use TZif on embedded platforms such as STM32, ARM Cortex-M, and other resource-constrained systems.

---

## Architecture for Embedded

TZif's hexagonal architecture enables embedded deployment:

```
+-------------------+
|   Your Application |
+-------------------+
         |
+-------------------+
|    TZif.API       |  -- Use TZif.API.Embedded composition root
+-------------------+
         |
+-------------------+
|   Your I/O Adapter |  -- Implement platform-specific I/O
+-------------------+
         |
+-------------------+
|  Flash/RAM Storage |  -- Store TZif files
+-------------------+
```

---

## Creating a Custom I/O Adapter

### Step 1: Define Your I/O Package

```ada
package My_Embedded_IO is
   type Byte_Array is array (Positive range <>) of Interfaces.Unsigned_8;
   subtype TZif_Buffer is Byte_Array (1 .. 65_536);

   package Read_File_Result is new TZif.Domain.Error.Result.Generic_Result
     (T => Read_Info);

   procedure Read_File
     (Id     :     Zone_Id_Input_Type;
      Bytes  : out Byte_Array;
      Length : out Natural;
      Result : out Read_File_Result.Result);

   --  Implement other required I/O procedures...
end My_Embedded_IO;
```

### Step 2: Implement File Reading

```ada
procedure Read_File
  (Id     :     Zone_Id_Input_Type;
   Bytes  : out Byte_Array;
   Length : out Natural;
   Result : out Read_File_Result.Result)
is
   --  Map Zone_Id to flash address
   Flash_Addr : constant System.Address := Get_Zone_Address (To_String (Id));
begin
   if Flash_Addr = System.Null_Address then
      Result := Read_File_Result.Error (Not_Found_Error, "Zone not found in flash");
      Length := 0;
      return;
   end if;

   --  Copy from flash to buffer
   Copy_From_Flash (Flash_Addr, Bytes, Length);
   Result := Read_File_Result.Ok ((Bytes_Read => Length));
end Read_File;
```

### Step 3: Create Composition Root

```ada
package My_Embedded_Ops is new TZif.Application.Operations.All_Operations
  (Byte_Array => My_Embedded_IO.Byte_Array,
   Read_File_Result => My_Embedded_IO.Read_File_Result,
   Read_File => My_Embedded_IO.Read_File,
   --  ... other parameters
  );

package My_API is new TZif.API.Operations.Facade (Ops => My_Embedded_Ops);
```

---

## Build Profiles

TZif supports embedded-specific build profiles:

### Embedded (Ravenscar)

```bash
alr build -- -XTZIF_PROFILE=embedded
```

Characteristics:
- Ravenscar profile (restricted Ada)
- 512KB+ RAM
- Limited zones/transitions
- No tasking beyond Ravenscar

### Bare Metal

```bash
alr build -- -XTZIF_PROFILE=baremetal
```

Characteristics:
- Zero Footprint Profile
- 128KB+ RAM
- Minimal zone support
- No dynamic allocation

---

## Memory Considerations

### Static Allocation

Domain types use bounded containers:
- `Bounded_Vector` for transitions
- `Bounded_String` for identifiers
- Fixed-size buffers for I/O

### Typical Memory Usage

| Component | Size |
|-----------|------|
| Zone data buffer | 64 KB max |
| Parser state | ~4 KB |
| Result values | ~1 KB |

---

## Storing Timezone Data

### Option 1: Flash Storage

Store TZif binary files in flash:
```
flash/
  +-- America_New_York.tzif
  +-- America_Los_Angeles.tzif
  +-- UTC.tzif
```

### Option 2: Compiled Resources

Compile TZif data as Ada constants:
```ada
package Zone_Data is
   UTC_Data : constant Byte_Array := (16#54#, 16#5A#, 16#69#, 16#66#, ...);
end Zone_Data;
```

---

## Platform-Specific Notes

### STM32

- Use STM32 HAL for flash access
- Map zones to flash sectors
- Consider QSPI for larger databases

### ARM Cortex-M

- Use MPU for flash protection
- Align zone data to word boundaries
- Consider DMA for large reads

---

## Limitations

1. **No Dynamic Source Discovery** - Must know zone locations at compile time
2. **Limited Zone Set** - Include only needed zones
3. **No Regex Support** - Use Find_By_Id or precomputed lists
4. **Single Source** - Typically one flash region

---

## Example: Minimal Embedded Usage

```ada
with TZif.API;
with My_Embedded_IO;

procedure Get_Local_Offset is
   use TZif.API;

   Zone_Id : constant Zone_Id_Type := Make_Zone_Id ("America/Denver");
   Epoch   : constant Epoch_Seconds_Type := Get_Current_Epoch;
   Result  : constant Transition_Result :=
     Get_Transition_At_Epoch (Zone_Id, Epoch);
begin
   if Is_Ok (Result) then
      --  Use offset for time conversion
      Apply_Offset (Value (Result).Offset);
   end if;
end Get_Local_Offset;
```

---

**License:** BSD-3-Clause<br>
**Copyright:** © 2025 Michael Gardner, A Bit of Help, Inc.
