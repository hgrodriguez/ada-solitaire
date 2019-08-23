--  with AUnit.Assertions;
with AUnit.Test_Caller;

package body Tableau.Tests4_To_String is

   procedure To_String_Empty_Tableau (T : in out Test) is
      pragma Unreferenced (T);
   begin
      null;
   end To_String_Empty_Tableau;

   --------------------------------------------------------------------
   --  the test suit construction
   package Caller is new AUnit.Test_Caller
     (Tableau.Tests4_To_String.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
      N   : constant String := "Tableau.Tests4_To_String.";
   begin
      --  One_Liners
      Ret.Add_Test (Caller.
                      Create (N & "To_String_Empty_Tableau",
                        To_String_Empty_Tableau'Access));

      return Ret;
   end Suite;

end Tableau.Tests4_To_String;
