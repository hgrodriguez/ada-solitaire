with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Deck.Test is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Ctor (T : in out Test);



end Deck.Test;
