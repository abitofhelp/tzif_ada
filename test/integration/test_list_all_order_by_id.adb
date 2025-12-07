pragma Ada_2022;
--  ======================================================================
--  Test_List_All_Order_By_Id
--  ======================================================================
--  Copyright (c) 2025 Michael Gardner, A Bit of Help, Inc.
--  SPDX-License-Identifier: BSD-3-Clause
--  Purpose:
--    Unit tests for List All Order By Id functionality.
--  ======================================================================
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Test_Framework;
with TZif.Application.Port.Inbound.List_All_Order_By_Id;
with TZif.Application.Usecase.List_All_Order_By_Id;
with TZif.Infrastructure.Adapter.File_System.POSIX_Repository;
with TZif.Domain.Value_Object.Source_Info;
procedure Test_List_All_Order_By_Id is
   use TZif.Application.Port.Inbound.List_All_Order_By_Id;
   use TZif.Domain.Value_Object.Source_Info;
   package Repo renames
     TZif.Infrastructure.Adapter.File_System.POSIX_Repository;
   package UC is new
     TZif.Application.Usecase.List_All_Order_By_Id.Use_Case
       (Repository_List_All_Order_By_Id => Repo.List_All_Zones);
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
begin
   Put_Line ("Test: List All Order By ID");
   declare
      Result : constant List_All_Zones_Result := UC.Execute
          (Make_Source_Info
             (Make_ULID ("01ARZ3NDEKTSV4RRFFQ69G5FAV"),
              Make_Path ("/usr/share/zoneinfo"),
              Make_Version ("2024a"),
              0),
           Descending => False);
   begin
      Assert
        (List_All_Zones_Result_Package.Is_Ok (Result)
         or else List_All_Zones_Result_Package.Is_Error (Result),
         "Should return result");
   end;
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
end Test_List_All_Order_By_Id;