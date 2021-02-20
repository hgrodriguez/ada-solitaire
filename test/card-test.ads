with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Card.Test is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Ctor_Rank (T : in out Test);
   procedure Ctor_Top_Rank (T : in out Test);
   procedure Ctor_Suit (T : in out Test);

   procedure Suit_Is_Red_Diamond (T : in out Test);
   procedure Suit_Is_Red_Heart (T : in out Test);

   procedure Suit_Is_Black_Club (T : in out Test);
   procedure Suit_Is_Black_Spade (T : in out Test);

   procedure Image_1 (T : in out Test);

   procedure Short_Image (T : in out Test);

end Card.Test;
