# Test Spies for Callback-Based Use Cases

This folder contains library-level spies for the three callback-style inbound ports:

- `Test_Spies.Find_By_Pattern_Spy`  (`with Application.Port.Inbound.Find_By_Pattern;`)
- `Test_Spies.Find_By_Regex_Spy`    (`with Application.Port.Inbound.Find_By_Regex;`)
- `Test_Spies.Find_By_Region_Spy`   (`with Application.Port.Inbound.Find_By_Region;`)

## Why

Ada's accessibility rules forbid passing a nested procedure to a library-level `access procedure` type.
These spies provide **library-level** callback procedures (`Collect`) plus protected state (`Reset`, `Count`, `Names`)
so tests can observe what was yielded—*with no production changes*.

## How to use in a test

```ada
with Test_Spies.Find_By_Pattern_Spy;

procedure Test_Find_By_Pattern is
   use Application.Port.Inbound.Find_By_Pattern;
   package UC is new Application.Usecase.Find_By_Pattern.Use_Case
     (Repository_Find_By_Pattern =>
        Infrastructure.Adapter.File_System.Repository.Find_By_Pattern);

   Pattern : constant Pattern_String := Pattern_Strings.To_Bounded_String ("America");
   Result  : Find_By_Pattern_Result;
begin
   Test_Spies.Find_By_Pattern_Spy.Reset;

   Result := UC.Execute (Pattern, Test_Spies.Find_By_Pattern_Spy.Collect'Access);

   -- Assertions:
   --  Assert (Is_Ok (Result));
   --  Assert (Test_Spies.Find_By_Pattern_Spy.Count > 0);
end Test_Find_By_Pattern;
```

## Build integration

Add `test_support/*.adb` and `.ads` to your test project file (`tests.gpr`) or test library GPR.
No changes to production GPR files are required.
