with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Pile_Of_Cards.Test is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;
private
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Construct (T : in out Test);
   procedure Put_One_Card (T : in out Test);

end Pile_Of_Cards.Test;
