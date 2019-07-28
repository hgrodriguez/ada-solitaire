with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Foundation.Test is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Construct (T : in out Test);
   procedure Construct_Check_Size (T : in out Test);

   procedure Check_Accepted_Empty_Foundation (T : in out Test);
   procedure Accepts_Suit_Diamond_All_Cards (T : in out Test);
   procedure Accepts_Suit_Club_All_Cards (T : in out Test);
   procedure Accepts_Suit_Heart_All_Cards (T : in out Test);
   procedure Accepts_Suit_Spade_All_Cards (T : in out Test);

end Foundation.Test;
