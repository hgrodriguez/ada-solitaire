with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Tableau.Test is
   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Construct (T : in out Test);
   procedure Construct_Check_Size (T : in out Test);

   procedure Add_1_Card_Check_Size (T : in out Test);
   procedure Add_3_Cards_Check_Size (T : in out Test);
   procedure Add_7_Cards_Check_Size (T : in out Test);

   procedure Add_1_Card_Check_Stack (T : in out Test);
   procedure Add_3_Cards_Check_Stack (T : in out Test);
   procedure Add_7_Cards_Check_Stack (T : in out Test);

end Tableau.Test;
