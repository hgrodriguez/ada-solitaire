with AUnit.Assertions;
with AUnit.Test_Caller;

package body Tableau.Test is

   procedure Construct (T : in out Test) is
      pragma Unreferenced (T);
      Tab : Tableau.Tableau_Type := Tableau.Construct;
      pragma Warnings (Off, Tab);
   begin
      null;
   end Construct;

   procedure Construct_Check_Size (T : in out Test) is
      pragma Unreferenced (T);
      Tab : constant Tableau.Tableau_Type := Tableau.Construct;
   begin
      AUnit.Assertions.Assert (Tab.Size = 0,
                               "t.Size=" & Tab.Size'Image &
                                 " /= 0");
   end Construct_Check_Size;

   --------------------------------------------------------------------
   --  the test suit construction
   package Caller is new AUnit.Test_Caller (Tableau.Test.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
      N   : constant String := "Tableau.Test.";
   begin
      --  ctor tests
      Ret.Add_Test (Caller.
                     Create (N & "Construct",
                       Construct'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Construct_Check_Size",
                        Construct_Check_Size'Access));
      return Ret;
   end Suite;

end Tableau.Test;
