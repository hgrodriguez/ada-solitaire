with Pile_Of_Cards;
with Card;
with Deck;

with AUnit.Assertions;
with AUnit.Test_Caller;
with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package body Pile_Of_Cards.Test is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   --------------------------------------------------------------------
   -- all test procedures
   
   -- test constructing a pile
   procedure Construct(T : in out Test) is
      pragma Unreferenced (T);
      pile : Pile_Of_Cards.Pile_Type;
      
   begin
      pile := Pile_Of_Cards.Construct;
      AUNit.Assertions.Assert (pile.Is_Empty, "should be empty!");
      AUNit.Assertions.Assert (pile.Size = 0,
                               "size should=0, " &
                                 "but is=" & pile.Size'Image);
   end Construct;
  
   -- FIFO: putting elements into the pile
   procedure Put_One_Card(T : in out Test) is
      pragma Unreferenced (T);
      c    : Card.Card_Type := Card.Construct (Rank => Deck.Ace,
                                               Suit => Deck.Diamond);
      pile : Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;
      
   begin
      pile.Put (c);
      AUNit.Assertions.Assert (not pile.Is_Empty, "should not be empty!");
      AUNit.Assertions.Assert (pile.Size = 1,
                               "size should=1, " &
                                 "but is=" & pile.Size'Image);
   end Put_One_Card;
   
   
   
   --------------------------------------------------------------------
   -- the test suit construction
   package Caller is new AUnit.Test_Caller (Pile_Of_Cards.Test.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
             := new AUnit.Test_Suites.Test_Suite;
   begin
      -- ctor tests
      Ret.Add_Test (Caller.
                      Create("Pile_Of_Cards.Construct",
                        Construct'Access));

      -- FIFO tests
      Ret.Add_Test (Caller.
                      Create("Pile_Of_Cards.Put_One_Card",
                        Put_One_Card'Access));
      return Ret;
   end Suite;

end Pile_Of_Cards.Test;
