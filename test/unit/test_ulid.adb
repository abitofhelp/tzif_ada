pragma Ada_2022;
--  ======================================================================
--  Test_ULID
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Purpose:
--    Comprehensive unit tests for ULID functionality including:
--    - Validation (Is_Valid_ULID_String)
--    - Parsing (Parse_ULID with Result monad)
--    - Null ULID handling
--    - Generation (uniqueness, ordering)
--    - Monotonic increment (same-millisecond handling)
--    - Thread safety (concurrent generation)
--    - Seed-based deterministic generation
--
--  ======================================================================

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Containers.Ordered_Sets;
with Test_Framework;
with TZif.Domain.Value_Object.Source_Info;
with TZif.Domain.Error;
with TZif.Infrastructure.ULID;

procedure Test_ULID is

   use TZif.Domain.Value_Object.Source_Info;
   use TZif.Infrastructure.ULID;

   --  Type alias for cleaner code
   package ULID_Result renames
     TZif.Domain.Value_Object.Source_Info.ULID_Result;

   Test_Count : Natural := 0;
   Pass_Count : Natural := 0;

   procedure Assert (Condition : Boolean; Test_Name : String) is
   begin
      Test_Count := Test_Count + 1;
      if Condition then
         Pass_Count := Pass_Count + 1;
         Put_Line ("  [PASS] " & Test_Name);
      else
         Put_Line ("  [FAIL] " & Test_Name);
      end if;
   end Assert;

   --  =====================================================================
   --  Test 1: ULID Validation (Is_Valid_ULID_String)
   --  =====================================================================

   procedure Test_Validation is
   begin
      Put_Line ("Test: ULID Validation");

      --  Valid ULIDs (all Crockford Base32)
      Assert
        (Is_Valid_ULID_String ("01ARZ3NDEKTSV4RRFFQ69G5FAV"),
         "Valid ULID: all uppercase Base32");

      Assert
        (Is_Valid_ULID_String ("00000000000000000000000000"),
         "Valid ULID: all zeros (Null ULID)");

      Assert
        (Is_Valid_ULID_String ("7ZZZZZZZZZZZZZZZZZZZZZZZZZ"),
         "Valid ULID: all max Base32 chars (Z)");

      --  Invalid length
      Assert
        (not Is_Valid_ULID_String (""),
         "Invalid ULID: empty string");

      Assert
        (not Is_Valid_ULID_String ("01ARZ3NDEKTSV4RRFFQ69G5FA"),
         "Invalid ULID: too short (25 chars)");

      Assert
        (not Is_Valid_ULID_String ("01ARZ3NDEKTSV4RRFFQ69G5FAVX"),
         "Invalid ULID: too long (27 chars)");

      --  Invalid characters (excluded from Crockford Base32)
      Assert
        (not Is_Valid_ULID_String ("01ARZ3NDEKTSV4RRFFQ69G5FIL"),
         "Invalid ULID: contains 'I' (excluded)");

      Assert
        (not Is_Valid_ULID_String ("01ARZ3NDEKTSV4RRFFQ69G5FLO"),
         "Invalid ULID: contains 'L' and 'O' (excluded)");

      Assert
        (not Is_Valid_ULID_String ("01arz3ndektsv4rrffq69g5fav"),
         "Invalid ULID: lowercase not allowed");

      Assert
        (not Is_Valid_ULID_String ("01ARZ3NDEKTSV4RRFFQ69G5F@V"),
         "Invalid ULID: contains special char '@'");

      Assert
        (not Is_Valid_ULID_String ("01ARZ3NDEKTSV4RRFFQ69G5F V"),
         "Invalid ULID: contains space");

   end Test_Validation;

   --  =====================================================================
   --  Test 2: ULID Parsing (Parse_ULID with Result monad)
   --  =====================================================================

   procedure Test_Parsing is
   begin
      Put_Line ("Test: ULID Parsing");

      --  Valid parse returns Ok
      declare
         Result : constant ULID_Result.Result :=
           Parse_ULID ("01ARZ3NDEKTSV4RRFFQ69G5FAV");
      begin
         Assert (ULID_Result.Is_Ok (Result), "Valid ULID parses to Ok result");

         if ULID_Result.Is_Ok (Result) then
            declare
               ULID : constant ULID_Type := ULID_Result.Value (Result);
            begin
               Assert
                 (To_String (ULID) = "01ARZ3NDEKTSV4RRFFQ69G5FAV",
                  "Parsed ULID has correct value");
               Assert (not Is_Null (ULID), "Parsed ULID is not null");
            end;
         end if;
      end;

      --  Invalid parse returns Error
      declare
         Result : constant ULID_Result.Result := Parse_ULID ("INVALID");
      begin
         Assert
           (ULID_Result.Is_Error (Result),
            "Invalid ULID parses to Error result");

         if ULID_Result.Is_Error (Result) then
            declare
               use TZif.Domain.Error;
               Err : constant Error_Type :=
                 ULID_Result.Error_Info (Result);
               Err_Msg : constant String :=
                 Error_Strings.To_String (Err.Message);
            begin
               Assert
                 (Err_Msg'Length > 0,
                  "Error result contains descriptive message");
            end;
         end if;
      end;

      --  Empty string returns Error
      declare
         Result : constant ULID_Result.Result := Parse_ULID ("");
      begin
         Assert
           (ULID_Result.Is_Error (Result),
            "Empty string parses to Error");
      end;

      --  Roundtrip: Parse(To_String(ULID)) succeeds
      declare
         Original : constant ULID_Type := Generate;
         Str      : constant String    := To_String (Original);
         Result   : constant ULID_Result.Result := Parse_ULID (Str);
      begin
         Assert (ULID_Result.Is_Ok (Result), "Roundtrip parse succeeds");

         if ULID_Result.Is_Ok (Result) then
            declare
               Parsed : constant ULID_Type := ULID_Result.Value (Result);
            begin
               Assert (Original = Parsed, "Roundtrip ULID equals original");
            end;
         end if;
      end;

   end Test_Parsing;

   --  =====================================================================
   --  Test 3: Null ULID
   --  =====================================================================

   procedure Test_Null_ULID is
   begin
      Put_Line ("Test: Null ULID");

      --  Null ULID is all zeros
      declare
         Null_ID : constant ULID_Type := Null_ULID;
      begin
         Assert
           (To_String (Null_ID) = "00000000000000000000000000",
            "Null ULID is all zeros");
         Assert (Is_Null (Null_ID), "Is_Null(Null_ULID) returns True");
      end;

      --  Generated ULID is not null
      declare
         Generated : constant ULID_Type := Generate;
      begin
         Assert (not Is_Null (Generated), "Generated ULID is not null");
         Assert (Generated /= Null_ULID, "Generated ULID != Null_ULID");
      end;

      --  Null ULID compares less than any generated ULID (lexicographic)
      declare
         Null_ID   : constant ULID_Type := Null_ULID;
         Generated : constant ULID_Type := Generate;
      begin
         Assert
           (Null_ID < Generated,
            "Null ULID < generated ULID (lexicographic ordering)");
      end;

   end Test_Null_ULID;

   --  =====================================================================
   --  Test 4: Basic ULID Generation
   --  =====================================================================

   procedure Test_Generation is
      --  Set for uniqueness checking
      package ULID_Sets is new Ada.Containers.Ordered_Sets
        (Element_Type => ULID_Type);
      use ULID_Sets;

      Batch_Size : constant := 1_000;
   begin
      Put_Line ("Test: Basic ULID Generation");

      --  Single generation
      declare
         ULID : constant ULID_Type := Generate;
      begin
         Assert (not Is_Null (ULID), "Generated ULID is not null");
         Assert
           (To_String (ULID)'Length = 26, "Generated ULID has correct length");
         Assert
           (Is_Valid_ULID_String (To_String (ULID)),
            "Generated ULID passes validation");
      end;

      --  Batch uniqueness test (1000 ULIDs)
      declare
         ULID_Set : Set;
      begin
         for I in 1 .. Batch_Size loop
            declare
               ULID : constant ULID_Type := Generate;
            begin
               Insert (ULID_Set, ULID);
            end;
         end loop;

         Assert
           (Natural (Length (ULID_Set)) = Batch_Size,
            "All" & Batch_Size'Image & " generated ULIDs are unique");
      end;

   end Test_Generation;

   --  =====================================================================
   --  Test 5: Monotonic Increment (Same Millisecond)
   --  =====================================================================

   procedure Test_Monotonic_Increment is
      Rapid_Batch : constant := 100;
      ULIDs       : array (1 .. Rapid_Batch) of ULID_Type;
   begin
      Put_Line ("Test: Monotonic Increment");

      --  Generate ULIDs as rapidly as possible
      --  (likely within same millisecond)
      for I in ULIDs'Range loop
         ULIDs (I) := Generate;
      end loop;

      --  Verify lexicographic ordering (monotonic property)
      declare
         All_Ordered : Boolean := True;
      begin
         for I in ULIDs'First .. ULIDs'Last - 1 loop
            if not (ULIDs (I) < ULIDs (I + 1)) then
               All_Ordered := False;
               exit;
            end if;
         end loop;

         Assert
           (All_Ordered,
            "Rapidly generated ULIDs maintain lexicographic ordering " &
            "(monotonic)");
      end;

      --  Verify all unique
      declare
         All_Unique : Boolean := True;
      begin
         for I in ULIDs'First .. ULIDs'Last - 1 loop
            for J in I + 1 .. ULIDs'Last loop
               if ULIDs (I) = ULIDs (J) then
                  All_Unique := False;
                  exit;
               end if;
            end loop;
            exit when not All_Unique;
         end loop;

         Assert
           (All_Unique,
            "Rapidly generated ULIDs are all unique (no collisions)");
      end;

   end Test_Monotonic_Increment;

   --  =====================================================================
   --  Test 6: Thread Safety (Concurrent Generation)
   --  =====================================================================

   procedure Test_Thread_Safety is
      Task_Count  : constant := 4;
      Per_Task    : constant := 50;
      Total_Count : constant := Task_Count * Per_Task;

      --  Storage for collected ULIDs
      type ULID_Array is array (1 .. Total_Count) of ULID_Type;

      --  Protected collector to safely aggregate from multiple tasks
      protected Collector is
         procedure Add (U : ULID_Type);
         procedure Task_Finished;
         entry Wait_For_All;
         function Get_Count return Natural;
         function Get_ULIDs return ULID_Array;
      private
         ULIDs      : ULID_Array := [others => Null_ULID];
         Count      : Natural := 0;
         Tasks_Done : Natural := 0;
      end Collector;

      protected body Collector is
         procedure Add (U : ULID_Type) is
         begin
            if Count < Total_Count then
               Count := Count + 1;
               ULIDs (Count) := U;
            end if;
         end Add;

         procedure Task_Finished is
         begin
            Tasks_Done := Tasks_Done + 1;
         end Task_Finished;

         entry Wait_For_All when Tasks_Done = Task_Count is
         begin
            null;  --  Just unblock when all tasks have signaled
         end Wait_For_All;

         function Get_Count return Natural is
         begin
            return Count;
         end Get_Count;

         function Get_ULIDs return ULID_Array is
         begin
            return ULIDs;
         end Get_ULIDs;
      end Collector;

      --  Worker task type
      task type Worker_Task;

      task body Worker_Task is
      begin
         for I in 1 .. Per_Task loop
            Collector.Add (Generate);
         end loop;
         Collector.Task_Finished;
      end Worker_Task;

      --  Check uniqueness
      function Count_Unique return Natural is
         Collected : constant ULID_Array := Collector.Get_ULIDs;
         N         : constant Natural := Total_Count;
         Unique    : Natural := 0;
         Is_Dup    : Boolean;
      begin
         for I in 1 .. N loop
            Is_Dup := False;
            for J in 1 .. I - 1 loop
               if Collected (I) = Collected (J) then
                  Is_Dup := True;
                  exit;
               end if;
            end loop;
            if not Is_Dup then
               Unique := Unique + 1;
            end if;
         end loop;
         return Unique;
      end Count_Unique;

      --  Declare workers
      Workers : array (1 .. Task_Count) of Worker_Task;
      pragma Unreferenced (Workers);

   begin
      Put_Line ("Test: Thread Safety (Concurrent Generation)");

      --  Wait for all workers to complete
      Collector.Wait_For_All;

      Assert
        (Collector.Get_Count = Total_Count,
         "Concurrent tasks collected" & Total_Count'Image &
         " ULIDs (got" & Collector.Get_Count'Image & ")");

      Assert
        (Count_Unique = Total_Count,
         "All" & Total_Count'Image & " ULIDs are unique - no race conditions" &
         " (got" & Count_Unique'Image & " unique)");

   end Test_Thread_Safety;

   --  =====================================================================
   --  Test 7: Seed-Based Deterministic Generation
   --  =====================================================================

   procedure Test_Seed_Based_Generation is
   begin
      Put_Line ("Test: Seed-Based Deterministic Generation");

      --  Same seed produces same ULID
      declare
         ULID1 : constant ULID_Type := Generate_From_Seed ("test-seed-123");
         ULID2 : constant ULID_Type := Generate_From_Seed ("test-seed-123");
      begin
         Assert
           (ULID1 = ULID2,
            "Same seed produces identical ULID (deterministic)");
      end;

      --  Different seeds produce different ULIDs
      declare
         ULID_A : constant ULID_Type := Generate_From_Seed ("seed-A");
         ULID_B : constant ULID_Type := Generate_From_Seed ("seed-B");
      begin
         Assert
           (ULID_A /= ULID_B,
            "Different seeds produce different ULIDs");
      end;

      --  Seed-based ULID is valid
      declare
         ULID : constant ULID_Type :=
           Generate_From_Seed ("production-fixture");
      begin
         Assert (not Is_Null (ULID), "Seed-based ULID is not null");
         Assert
           (Is_Valid_ULID_String (To_String (ULID)),
            "Seed-based ULID passes validation");
      end;

   end Test_Seed_Based_Generation;

   --  =====================================================================
   --  Test 8: Comparison and Ordering
   --  =====================================================================

   procedure Test_Comparison is
   begin
      Put_Line ("Test: Comparison and Ordering");

      --  Equality
      declare
         ULID1 : constant ULID_Type := Generate;
         ULID2 : constant ULID_Type := ULID1;
      begin
         Assert (ULID1 = ULID2, "ULID equals itself");
      end;

      --  Inequality
      declare
         ULID_X : constant ULID_Type := Generate;
         ULID_Y : constant ULID_Type := Generate;
      begin
         Assert (ULID_X /= ULID_Y, "Different ULIDs are not equal");
      end;

      --  Less-than ordering (temporal)
      declare
         Earlier : constant ULID_Type := Generate;
         Later   : constant ULID_Type := Generate;
      begin
         Assert
           (Earlier < Later,
            "Earlier ULID < later ULID (lexicographic = temporal)");
         Assert (not (Later < Earlier), "Later ULID not < earlier ULID");
      end;

   end Test_Comparison;

   --  =====================================================================
   --  Main Test Execution
   --  =====================================================================

begin
   Put_Line ("====================================================");
   Put_Line ("  ULID Comprehensive Test Suite");
   Put_Line ("====================================================");

   Test_Validation;
   Test_Parsing;
   Test_Null_ULID;
   Test_Generation;
   Test_Monotonic_Increment;
   Test_Thread_Safety;
   Test_Seed_Based_Generation;
   Test_Comparison;

   --  Summary
   Put_Line ("====================================================");
   Put_Line
     ("  Results:" & Pass_Count'Image & " /" & Test_Count'Image & " passed");
   if Pass_Count = Test_Count then
      Put_Line ("  Status: ALL TESTS PASSED");
   else
      Put_Line ("  Status: FAILURES DETECTED");
   end if;
   Put_Line ("====================================================");

   Test_Framework.Register_Results (Test_Count, Pass_Count);

   if Pass_Count /= Test_Count then
      Ada.Command_Line.Set_Exit_Status (1);
   end if;

end Test_ULID;
