with Card;
with Deck; use Deck;

with AUnit.Assertions;
with AUnit.Test_Caller;
with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package body Card.Test is
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   --------------------------------------------------------------------
   -- all test procedures
   
   -- test constructing a card
   procedure Ctor_Rank(T : in out Test) is
      pragma Unreferenced (T);
      rank : constant Deck.Rank_Type := Deck.Ace;
      suit : constant Deck.Suit_Type := Deck.Diamond;
      c    : Card.Card_Type;
      
   begin
      c := Card.Construct (rank, suit);
      AUNit.Assertions.Assert (c.Get_Rank = rank,
                               "Rank should be: " & rank'Image &
                               ", but is:" & c.Get_Rank'Image);
   end Ctor_Rank;
   
   procedure Ctor_Suit (T : in out Test) is
      pragma Unreferenced (T);
      rank : constant Deck.Rank_Type := Deck.Ace;
      suit : constant Deck.Suit_Type := Deck.Diamond;
      c    : Card.Card_Type;
      
   begin
      c := Card.Construct (rank, suit);
      AUNit.Assertions.Assert (c.Get_Suit = suit,
                               "Suit should be: " & suit'Image &
                               ", but is:" & c.Get_Suit'Image);
   end Ctor_Suit;


   --------------------------------------------------------------------
   -- test comparisons of two cards
   --------------------------------------------------------------------

   --------------------------------------------------------------------
   -- rank
   procedure Rank_Is_Equal_To_Using_Identical_Rank (T : in out Test) is
      pragma Unreferenced (T);
      rank : constant Deck.Rank_Type := Deck.Ace;
      suit : constant Deck.Suit_Type := Deck.Diamond;
      c1   : Card.Card_Type := Card.Construct (rank, suit);
      c2   : Card.Card_Type := Card.Construct (rank, suit);
      
   begin
      AUNit.Assertions.Assert (c1.Rank_Is_Equal_To (c2),
                                 "c1.Rank:" & c1.Get_Rank'Image &
                                 " should be equal to " &
                                 "c2.Rank:" & c2.Get_Rank'Image);
   end Rank_Is_Equal_To_Using_Identical_Rank;

   procedure Rank_Is_Equal_To_Using_Equal_Rank (T : in out Test) is
      pragma Unreferenced (T);
      rank1 : constant Deck.Rank_Type := Deck.Ace;
      rank2 : constant Deck.Rank_Type := Deck.Ace;
      suit  : constant Deck.Suit_Type := Deck.Diamond;
      c1    : Card.Card_Type := Card.Construct (rank1, suit);
      c2    : Card.Card_Type := Card.Construct (rank2, suit);
      
   begin
      AUNit.Assertions.Assert (c1.Rank_Is_Equal_To (c2),
                                 "c1.Rank:" & c1.Get_Rank'Image &
                                 " should be equal to " &
                                 "c2.Rank:" & c2.Get_Rank'Image);
   end Rank_Is_Equal_To_Using_Equal_Rank;

   procedure Rank_Is_Higher_Than_True (T : in out Test) is
      pragma Unreferenced (T);
      rank_low  : constant Deck.Rank_Type := Deck.Ace;
      rank_high : constant Deck.Rank_Type := Deck.King;
      suit      : constant Deck.Suit_Type := Deck.Diamond;
      c1        : Card.Card_Type := Card.Construct (rank_high, suit);
      c2        : Card.Card_Type := Card.Construct (rank_low, suit);
      
   begin
      AUNit.Assertions.Assert (c1.Rank_Is_Higher_Than (c2),
                                 "c1.Rank:" & c1.Get_Rank'Image &
                                 " should be higher than " &
                                 "c2.Rank:" & c2.Get_Rank'Image);
   end Rank_Is_Higher_Than_True;

   procedure Rank_Is_Higher_Than_False (T : in out Test) is
      pragma Unreferenced (T);
      rank_low  : constant Deck.Rank_Type := Deck.Ace;
      rank_high : constant Deck.Rank_Type := Deck.King;
      suit      : constant Deck.Suit_Type := Deck.Diamond;
      c1        : Card.Card_Type := Card.Construct (rank_low, suit);
      c2        : Card.Card_Type := Card.Construct (rank_high, suit);
      
   begin
      AUNit.Assertions.Assert (not c1.Rank_Is_Higher_Than (c2),
                                 "c1.Rank:" & c1.Get_Rank'Image &
                                 " should be higher than " &
                                 "c2.Rank:" & c2.Get_Rank'Image);
   end Rank_Is_Higher_Than_False;

   procedure Rank_Is_Lower_Than_True (T : in out Test) is
      pragma Unreferenced (T);
      rank_low  : constant Deck.Rank_Type := Deck.Ace;
      rank_high : constant Deck.Rank_Type := Deck.King;
      suit      : constant Deck.Suit_Type := Deck.Diamond;
      c1        : Card.Card_Type := Card.Construct (rank_low, suit);
      c2        : Card.Card_Type := Card.Construct (rank_high, suit);
      
   begin
      AUNit.Assertions.Assert (c1.Rank_Is_Lower_Than (c2),
                                 "c1.Rank:" & c1.Get_Rank'Image &
                                 " should be lower than " &
                                 "c2.Rank:" & c2.Get_Rank'Image);
   end Rank_Is_Lower_Than_True;

   procedure Rank_Is_Lower_Than_False (T : in out Test) is
      pragma Unreferenced (T);
      rank_low  : constant Deck.Rank_Type := Deck.Ace;
      rank_high : constant Deck.Rank_Type := Deck.King;
      suit      : constant Deck.Suit_Type := Deck.Diamond;
      c1        : Card.Card_Type := Card.Construct (rank_high, suit);
      c2        : Card.Card_Type := Card.Construct (rank_low, suit);
      
   begin
      AUNit.Assertions.Assert (not c1.Rank_Is_Lower_Than (c2),
                                 "c1.Rank:" & c1.Get_Rank'Image &
                                 " should be higher than " &
                                 "c2.Rank:" & c2.Get_Rank'Image);
   end Rank_Is_Lower_Than_False;
   
   
   
   
   
   
   
   --------------------------------------------------------------------
   -- suit
   procedure Suit_Is_Equal_To_Using_Identical_Suit (T : in out Test) is
      pragma Unreferenced (T);
      rank : constant Deck.Rank_Type := Deck.Ace;
      suit : constant Deck.Suit_Type := Deck.Diamond;
      c1   : Card.Card_Type := Card.Construct (rank, suit);
      c2   : Card.Card_Type := Card.Construct (rank, suit);
      
   begin
      AUNit.Assertions.Assert (c1.Suit_Is_Equal_To (c2),
                                 "c1.Suit:" & c1.Get_Suit'Image &
                                 " should be equal to " &
                                 "c2.Suit:" & c2.Get_Suit'Image);
   end Suit_Is_Equal_To_Using_Identical_Suit;

   procedure Suit_Is_Equal_To_Using_Equal_Suit (T : in out Test) is
      pragma Unreferenced (T);
      rank  : constant Deck.Rank_Type := Deck.Ace;
      suit1 : constant Deck.Suit_Type := Deck.Diamond;
      suit2 : constant Deck.Suit_Type := Deck.Diamond;
      c1    : Card.Card_Type := Card.Construct (rank, suit1);
      c2    : Card.Card_Type := Card.Construct (rank, suit2);
      
   begin
      AUNit.Assertions.Assert (c1.Suit_Is_Equal_To (c2),
                                 "c1.Suit:" & c1.Get_Suit'Image &
                                 " should be equal to " &
                                 "c2.Suit:" & c2.Get_Suit'Image);
   end Suit_Is_Equal_To_Using_Equal_Suit;
   
   procedure Suit_Is_Red_Diamond (T : in out Test) is
      pragma Unreferenced (T);
      rank : constant Deck.Rank_Type := Deck.Ace;
      suit : constant Deck.Suit_Type := Deck.Diamond;
      c    : Card.Card_Type := Card.Construct (rank, suit);
      
   begin
      AUNit.Assertions.Assert (c.Suit_Is_Red,
                                 "c.Suit:" & c.Get_Suit'Image &
                                 " should be red");
   end Suit_Is_Red_Diamond;
   
   procedure Suit_Is_Red_Heart (T : in out Test) is
      pragma Unreferenced (T);
      rank : constant Deck.Rank_Type := Deck.Ace;
      suit : constant Deck.Suit_Type := Deck.Heart;
      c    : Card.Card_Type := Card.Construct (rank, suit);
      
   begin
      AUNit.Assertions.Assert (c.Suit_Is_Red,
                                 "c.Suit:" & c.Get_Suit'Image &
                                 " should be red");
   end Suit_Is_Red_Heart;
   
   
   
   
   
   
   --------------------------------------------------------------------
   -- the test suit construction
   package Caller is new AUnit.Test_Caller (Card.Test.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
             := new AUnit.Test_Suites.Test_Suite;
   begin
      -- ctor tests
      Ret.Add_Test (Caller.
                      Create("Card.Ctor_Rank",
                        Ctor_Rank'Access));
      Ret.Add_Test (Caller.
                      Create("Card.Ctor_Suit",
                        Ctor_Suit'Access));
      
      -- rank tests
      Ret.Add_Test (Caller.
                      Create("Card.Rank_Is_Equal_To_Using_Identical_Rank",
                        Rank_Is_Equal_To_Using_Identical_Rank'Access));
      Ret.Add_Test (Caller.
                      Create("Card.Rank_Is_Equal_To_Using_Equal_Rank",
                        Rank_Is_Equal_To_Using_Equal_Rank'Access));
      Ret.Add_Test (Caller.
                      Create("Card.Rank_Is_Higher_Than_True",
                        Rank_Is_Higher_Than_True'Access));
      Ret.Add_Test (Caller.
                      Create("Card.Rank_Is_Higher_Than_False",
                        Rank_Is_Higher_Than_False'Access));

      Ret.Add_Test (Caller.
                      Create("Card.Rank_Is_Lower_Than_True",
                        Rank_Is_Lower_Than_True'Access));
      Ret.Add_Test (Caller.
                      Create("Card.Rank_Is_Lower_Than_False",
                        Rank_Is_Lower_Than_False'Access));
      
      -- suit tests
      Ret.Add_Test (Caller.
                      Create("Card.Suit_Is_Equal_To_Using_Identical_Suit",
                        Suit_Is_Equal_To_Using_Identical_Suit'Access));
      Ret.Add_Test (Caller.
                      Create("Card.Suit_Is_Equal_To_Using_Equal_Suit",
                        Suit_Is_Equal_To_Using_Equal_Suit'Access));
      Ret.Add_Test (Caller.
                      Create("Card.Suit_Is_Red_Diamond",
                        Suit_Is_Red_Diamond'Access));
      Ret.Add_Test (Caller.
                      Create("Card.Suit_Is_Red_Heart",
                        Suit_Is_Red_Heart'Access));
      
      return Ret;
   end Suite;

end Card.Test;
