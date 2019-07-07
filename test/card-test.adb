with Card;
with Deck; use Deck;

with AUnit.Assertions;
with AUnit.Test_Caller;
with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package body Card.Test is
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Ctor_Rank(T : in out Test) is
      pragma Unreferenced (T);
      rank : constant Deck.Rank_Type := Deck.Ace;
      suit : constant Deck.Suit_Type := Deck.Diamond;
      c    : Card.Card_Type;
      
   begin
      c := Card.Ctor (rank, suit);
      AUNit.Assertions.Assert (c.Get_Rank = rank,
                               "Rank should be: " & rank'Image &
                               ", but is:" & c.Get_Rank'Image);
   end Test_Ctor_Rank;

   procedure Test_Ctor_Suit (T : in out Test) is
      pragma Unreferenced (T);
      rank : constant Deck.Rank_Type := Deck.Ace;
      suit : constant Deck.Suit_Type := Deck.Diamond;
      c    : Card.Card_Type;
      
   begin
      c := Card.Ctor (rank, suit);
      AUNit.Assertions.Assert (c.Get_Suit = suit,
                               "Suit should be: " & suit'Image &
                               ", but is:" & c.Get_Suit'Image);
   end Test_Ctor_Suit;

   package Caller is new AUnit.Test_Caller (Card.Test.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
             := new AUnit.Test_Suites.Test_Suite;
   begin
      Ret.Add_Test(Caller.Create("Card.Test_Ctor_Rank",
                                 Test_Ctor_Rank'Access));
      Ret.Add_Test(Caller.Create("Card.Test_Ctor_Suit",
                                 Test_Ctor_Suit'Access));
      return Ret;
   end Suite;

end Card.Test;
