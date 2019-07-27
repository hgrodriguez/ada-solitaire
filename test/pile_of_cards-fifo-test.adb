with Ada.Exceptions;
with Pile_Of_Cards.FIFO;
with Card;
with Deck;

with AUnit.Assertions;
with AUnit.Test_Caller;

package body Pile_Of_Cards.FIFO.Test is

   --  test constructing a pile
   procedure Construct (T : in out Test) is
      pragma Unreferenced (T);
      pile : Pile_Of_Cards.FIFO.Pile_Type_FIFO;

   begin
      pile := Pile_Of_Cards.FIFO.Construct;
      AUnit.Assertions.Assert (pile.Is_Empty, "should be empty!");
      AUnit.Assertions.Assert (pile.Size = 0,
                               "size should=0, " &
                                 "but is=" & pile.Size'Image);
   end Construct;

   procedure Put_One_Card (T : in out Test) is
      pragma Unreferenced (T);
      c    : constant Card.Card_Type := Card.Construct (Rank => Deck.Ace,
                                                        Suit => Deck.Diamond);
      pile : Pile_Of_Cards.FIFO.Pile_Type_FIFO := Pile_Of_Cards.FIFO.Construct;

   begin
      pile.Put (c);
      AUnit.Assertions.Assert (not pile.Is_Empty, "should not be empty!");
      AUnit.Assertions.Assert (pile.Size = 1,
                               "size should=1, " &
                                 "but is=" & pile.Size'Image);
   end Put_One_Card;

   procedure Put_Two_Cards (T : in out Test) is
      pragma Unreferenced (T);
      c1   : constant Card.Card_Type := Card.Construct (Rank => Deck.Ace,
                                                        Suit => Deck.Diamond);
      c2   : constant Card.Card_Type := Card.Construct (Rank => Deck.Two,
                                                        Suit => Deck.Diamond);
      pile : Pile_Of_Cards.FIFO.Pile_Type_FIFO := Pile_Of_Cards.FIFO.Construct;

   begin
      pile.Put (c1);
      pile.Put (c2);
      AUnit.Assertions.Assert (not pile.Is_Empty, "should not be empty!");
      AUnit.Assertions.Assert (pile.Size = 2,
                               "size should=2, " &
                                 "but is=" & pile.Size'Image);
   end Put_Two_Cards;

   procedure Get_One_Card (T : in out Test) is
      pragma Unreferenced (T);
      c    : constant Card.Card_Type := Card.Construct (Rank => Deck.Ace,
                                                        Suit => Deck.Diamond);
      pile : Pile_Of_Cards.FIFO.Pile_Type_FIFO := Pile_Of_Cards.FIFO.Construct;
      r    : Card.Card_Type;

   begin
      pile.Put (c);
      r := pile.Get;
      AUnit.Assertions.Assert (pile.Is_Empty, "should be empty!");
      AUnit.Assertions.Assert (pile.Size = 0,
                               "size should=0, " &
                                 "but is=" & pile.Size'Image);
      AUnit.Assertions.Assert (r.Is_Equal_To (c),
                               "r:" & r.Image &
                                 " /= c:" & c.Image);
   end Get_One_Card;

   procedure Get_One_Card_Put_Two (T : in out Test) is
      pragma Unreferenced (T);
      pile : Pile_Of_Cards.FIFO.Pile_Type_FIFO := Pile_Of_Cards.FIFO.Construct;
      c1   : constant Card.Card_Type := Card.Construct (Rank => Deck.Ace,
                                                        Suit => Deck.Diamond);
      c2   : constant Card.Card_Type := Card.Construct (Rank => Deck.Two,
                                                        Suit => Deck.Diamond);
      r    : Card.Card_Type;
   begin
      pile.Put (c1);
      pile.Put (c2);
      r := pile.Get;
      AUnit.Assertions.Assert (pile.Size = 1,
                               "size should=1, " &
                                 "but is=" & pile.Size'Image);
      AUnit.Assertions.Assert (r.Is_Equal_To (c1),
                               "r:" & r.Image &
                                 " /= c:" & c1.Image);
   end Get_One_Card_Put_Two;

   procedure Get_Two_Cards_Put_Two (T : in out Test) is
      pragma Unreferenced (T);
      pile : Pile_Of_Cards.FIFO.Pile_Type_FIFO := Pile_Of_Cards.FIFO.Construct;
      c1   : constant Card.Card_Type := Card.Construct (Rank => Deck.Ace,
                                                        Suit => Deck.Diamond);
      c2   : constant Card.Card_Type := Card.Construct (Rank => Deck.Two,
                                                        Suit => Deck.Diamond);
      r1   : Card.Card_Type;
      r2   : Card.Card_Type;
   begin
      pile.Put (c1);
      pile.Put (c2);
      r1 := pile.Get;
      r2 := pile.Get;
      AUnit.Assertions.Assert (pile.Size = 0,
                               "size should=1, " &
                                 "but is=" & pile.Size'Image);
      AUnit.Assertions.Assert (r1.Is_Equal_To (c1),
                               "r1:" & r1.Image &
                                 " /= c1:" & c1.Image);
      AUnit.Assertions.Assert (r2.Is_Equal_To (c2),
                               "r2:" & r2.Image &
                                 " /= c2:" & c2.Image);
   end Get_Two_Cards_Put_Two;

   procedure Get_One_Card_No_Put_Exception;
   procedure Get_One_Card_No_Put_Exception is
      pile : Pile_Of_Cards.FIFO.Pile_Type_FIFO := Pile_Of_Cards.FIFO.Construct;
      r    : Card.Card_Type;
      pragma Warnings (Off, r);
   begin
      r := pile.Get;
   exception
      when Pile_Empty_Exception => raise;
      when Exc : others =>
         AUnit.Assertions.Assert (False,
                                  "Get_One_Card_No_Put_Exception: " &
                                    "wrong exception raised:" &
                                 Ada.Exceptions.Exception_Name (Exc));
   end Get_One_Card_No_Put_Exception;

   procedure Get_One_Card_No_Put (T : in out Test) is
      pragma Unreferenced (T);

   begin
      AUnit.Assertions.Assert_Exception (Get_One_Card_No_Put_Exception'Access,
                                         "Get_One_Card_No_Put: " &
                                           "no exception raised");
   end Get_One_Card_No_Put;

   --------------------------------------------------------------------
   --  the test suit construction
   package Caller is new AUnit.Test_Caller (Pile_Of_Cards.FIFO.Test.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      --  ctor tests
      Ret.Add_Test (Caller.
                      Create ("Pile_Of_Cards.Construct",
                        Construct'Access));

      --  FIFO tests
      Ret.Add_Test (Caller.
                      Create ("Pile_Of_Cards.Put_One_Card",
                        Put_One_Card'Access));
      Ret.Add_Test (Caller.
                      Create ("Pile_Of_Cards.Put_Two_Cards",
                        Put_Two_Cards'Access));
      Ret.Add_Test (Caller.
                      Create ("Pile_Of_Cards.Get_One_Card",
                        Get_One_Card'Access));
      Ret.Add_Test (Caller.
                      Create ("Pile_Of_Cards.Get_One_Card_Put_Two",
                        Get_One_Card_Put_Two'Access));
      Ret.Add_Test (Caller.
                      Create ("Pile_Of_Cards.Get_Two_Cards_Put_Two",
                        Get_Two_Cards_Put_Two'Access));
      Ret.Add_Test (Caller.
                      Create ("Pile_Of_Cards.Get_One_Card_No_Put",
                        Get_One_Card_No_Put'Access));
      return Ret;
   end Suite;

end Pile_Of_Cards.FIFO.Test;
