with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Deck.Test is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;
private
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Is_Red_Diamond (T : in out Test);
   procedure Test_Is_Red_Heart (T : in out Test);
   procedure Test_Is_Black_Clubs (T : in out Test);
   procedure Test_Is_Black_Spade (T : in out Test);

end Deck.Test;
