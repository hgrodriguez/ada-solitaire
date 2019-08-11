with Ada.Containers; use Ada.Containers;
with Ada.Exceptions;

with Card;
with Deck;
with Pile_Of_Cards.LIFO;

with AUnit.Assertions;
with AUnit.Test_Caller;

package body Pile_Of_Cards.LIFO.Test is

   --------------------------------------------------------------------
   --  test constructing a pile
   procedure Construct (T : in out Test) is
      pragma Unreferenced (T);
      pile : Pile_Of_Cards.LIFO.Pile_Type_LIFO;

   begin
      pile := Pile_Of_Cards.LIFO.Construct;
      AUnit.Assertions.Assert (pile.Is_Empty, "should be empty!");
      AUnit.Assertions.Assert (pile.Size = 0,
                               "size should=0, " &
                                 "but is=" & pile.Size'Image);
   end Construct;

   --------------------------------------------------------------------
   --  Push operations
   procedure Push_One_Card (T : in out Test) is
      pragma Unreferenced (T);
      c    : constant Card.Card_Type := Card.Construct (Rank => Deck.Ace,
                                                        Suit => Deck.Diamond);
      pile : Pile_Of_Cards.LIFO.Pile_Type_LIFO := Pile_Of_Cards.LIFO.Construct;

   begin
      pile.Push (c);
      AUnit.Assertions.Assert (not pile.Is_Empty, "should not be empty!");
      AUnit.Assertions.Assert (pile.Size = 1,
                               "size should=1, " &
                                 "but is=" & pile.Size'Image);
   end Push_One_Card;

   procedure Push_Two_Cards (T : in out Test) is
      pragma Unreferenced (T);
      c1   : constant Card.Card_Type := Card.Construct (Rank => Deck.Ace,
                                                        Suit => Deck.Diamond);
      c2   : constant Card.Card_Type := Card.Construct (Rank => Deck.Two,
                                                        Suit => Deck.Diamond);
      pile : Pile_Of_Cards.LIFO.Pile_Type_LIFO := Pile_Of_Cards.LIFO.Construct;

   begin
      pile.Push (c1);
      pile.Push (c2);
      AUnit.Assertions.Assert (not pile.Is_Empty, "should not be empty!");
      AUnit.Assertions.Assert (pile.Size = 2,
                               "size should=2, " &
                                 "but is=" & pile.Size'Image);
   end Push_Two_Cards;

   --------------------------------------------------------------------
   --  Pop operations
   procedure Pop_One_Pushed_One (T : in out Test) is
      pragma Unreferenced (T);
      c    : constant Card.Card_Type := Card.Construct (Rank => Deck.Ace,
                                                        Suit => Deck.Diamond);
      pile : Pile_Of_Cards.LIFO.Pile_Type_LIFO := Pile_Of_Cards.LIFO.Construct;
      r    : Card.Card_Type;
   begin
      pile.Push (c);
      r := pile.Pop;
      AUnit.Assertions.Assert (r.Is_Equal_To (c), "pushed: " & c.Image &
                                 " /= r:" & r.Image);
   end Pop_One_Pushed_One;

   procedure Pop_One_Pushed_Two (T : in out Test) is
      pragma Unreferenced (T);
      c1   : constant Card.Card_Type := Card.Construct (Rank => Deck.Ace,
                                                        Suit => Deck.Diamond);
      c2   : constant Card.Card_Type := Card.Construct (Rank => Deck.Two,
                                                        Suit => Deck.Diamond);
      pile : Pile_Of_Cards.LIFO.Pile_Type_LIFO := Pile_Of_Cards.LIFO.Construct;
      r    : Card.Card_Type;
   begin
      pile.Push (c1);
      pile.Push (c2);
      r := pile.Pop;
      AUnit.Assertions.Assert (r.Is_Equal_To (c2), "pushed: " & c2.Image &
                                 " /= r:" & r.Image);
   end Pop_One_Pushed_Two;

   procedure Pop_Two_Pushed_Two (T : in out Test) is
      pragma Unreferenced (T);
      c1   : constant Card.Card_Type := Card.Construct (Rank => Deck.Ace,
                                                        Suit => Deck.Diamond);
      c2   : constant Card.Card_Type := Card.Construct (Rank => Deck.Two,
                                                        Suit => Deck.Diamond);
      pile : Pile_Of_Cards.LIFO.Pile_Type_LIFO := Pile_Of_Cards.LIFO.Construct;
      r1   : Card.Card_Type;
      r2   : Card.Card_Type;
   begin
      pile.Push (c1);
      pile.Push (c2);
      r2 := pile.Pop;
      r1 := pile.Pop;
      AUnit.Assertions.Assert (r1.Is_Equal_To (c1), "pushed: " & c1.Image &
                                 " /= r1:" & r1.Image);
      AUnit.Assertions.Assert (r2.Is_Equal_To (c2), "pushed: " & c2.Image &
                                 " /= r2:" & r2.Image);
   end Pop_Two_Pushed_Two;

   procedure Pop_One_Pushed_None_Exception;
   procedure Pop_One_Pushed_None_Exception is
      pile : Pile_Of_Cards.LIFO.Pile_Type_LIFO := Pile_Of_Cards.LIFO.Construct;
      r    : Card.Card_Type;
      pragma Warnings (Off, r);
   begin
      r := pile.Pop;
   exception
      when Pile_Empty_Exception => raise;
      when Exc : others =>
         AUnit.Assertions.Assert (False,
                                  "Pop_One_Pushed_None_Exception: " &
                                    "wrong exception raised:" &
                                    Ada.Exceptions.Exception_Name (Exc));
   end Pop_One_Pushed_None_Exception;

   procedure Pop_One_Pushed_None (T : in out Test) is
      pragma Unreferenced (T);

   begin
      AUnit.Assertions.Assert_Exception (Pop_One_Pushed_None_Exception'Access,
                                         "Pop_One_Pushed_None: " &
                                           "no exception raised");
   end Pop_One_Pushed_None;

   procedure Pop_Two_Pushed_One_Exception;
   procedure Pop_Two_Pushed_One_Exception is
      pile : Pile_Of_Cards.LIFO.Pile_Type_LIFO := Pile_Of_Cards.LIFO.Construct;
      r    : Card.Card_Type;
      pragma Warnings (Off, r);
   begin
      r := pile.Pop;
      r := pile.Pop;
   exception
      when Pile_Empty_Exception => raise;
      when Exc : others =>
         AUnit.Assertions.Assert (False,
                                  "Pop_Two_Pushed_One_Exception: " &
                                    "wrong exception raised:" &
                                    Ada.Exceptions.Exception_Name (Exc));
   end Pop_Two_Pushed_One_Exception;

   procedure Pop_Two_Pushed_One (T : in out Test) is
      pragma Unreferenced (T);
      c    : constant Card.Card_Type := Card.Construct (Rank => Deck.Ace,
                                                        Suit => Deck.Diamond);
      pile : Pile_Of_Cards.LIFO.Pile_Type_LIFO := Pile_Of_Cards.LIFO.Construct;
   begin
      pile.Push (c);
      AUnit.Assertions.Assert_Exception (Pop_Two_Pushed_One_Exception'Access,
                                         "Pop_Two_Pushed_One: " &
                                           "no exception raised");
   end Pop_Two_Pushed_One;

   --------------------------------------------------------------------
   --  Peek operations
   procedure Peek_No_Cards_Exception;
   procedure Peek_No_Cards_Exception is
      pile : constant Pile_Of_Cards.LIFO.Pile_Type_LIFO
        := Pile_Of_Cards.LIFO.Construct;
      r    : Card.Card_Type;
      pragma Warnings (Off, r);
   begin
      r := pile.Peek;
   exception
      when Pile_Empty_Exception => raise;
      when Exc : others =>
         AUnit.Assertions.Assert (False,
                                  "Peek_No_Cards_Exception: " &
                                    "wrong exception raised:" &
                                    Ada.Exceptions.Exception_Name (Exc));
   end Peek_No_Cards_Exception;

   procedure Peek_No_Cards (T : in out Test) is
      pragma Unreferenced (T);
   begin
      AUnit.Assertions.Assert_Exception (Peek_No_Cards_Exception'Access,
                                         "Peek_No_Cards: " &
                                           "no exception raised");
   end Peek_No_Cards;

   procedure Peek_One_Card (T : in out Test) is
      pragma Unreferenced (T);
      pile : Pile_Of_Cards.LIFO.Pile_Type_LIFO := Pile_Of_Cards.LIFO.Construct;
      c    : constant Card.Card_Type := Card.Construct (Rank => Deck.Ace,
                                                        Suit => Deck.Diamond);
      r    : Card.Card_Type;
   begin
      pile.Push (c);
      r := pile.Peek;
      AUnit.Assertions.Assert (r.Is_Equal_To (c),
                               "r:" & r.Image &
                                 " /= c:" & c.Image);
   end Peek_One_Card;

   procedure Peek_Two_Cards (T : in out Test) is
      pragma Unreferenced (T);
      pile : Pile_Of_Cards.LIFO.Pile_Type_LIFO := Pile_Of_Cards.LIFO.Construct;
      c1   : constant Card.Card_Type := Card.Construct (Rank => Deck.Ace,
                                                        Suit => Deck.Diamond);
      c2   : constant Card.Card_Type := Card.Construct (Rank => Deck.Two,
                                                        Suit => Deck.Diamond);
      r    : Card.Card_Type;
   begin
      pile.Push (c1);
      pile.Push (c2);
      r := pile.Peek;
      AUnit.Assertions.Assert (r.Is_Equal_To (c2),
                               "r:" & r.Image &
                                 " /= c2:" & c2.Image);
   end Peek_Two_Cards;

   --------------------------------------------------------------------
   --  Has operations

   procedure Has_Not_No_Cards (T : in out Test) is
      pragma Unreferenced (T);
      pile  : constant Pile_Of_Cards.LIFO.Pile_Type_LIFO
        := Pile_Of_Cards.LIFO.Construct;
      c_has : constant Card.Card_Type := Card.Construct (Rank => Deck.King,
                                                         Suit => Deck.Diamond);
   begin
      AUnit.Assertions.Assert (not pile.Has (c_has), "Has=True");
   end Has_Not_No_Cards;

   procedure Has_Not_1_Card (T : in out Test) is
      pragma Unreferenced (T);
      pile  : Pile_Of_Cards.LIFO.Pile_Type_LIFO
        := Pile_Of_Cards.LIFO.Construct;
      c1    : constant Card.Card_Type := Card.Construct (Rank => Deck.Ace,
                                                         Suit => Deck.Diamond);
      c_has : constant Card.Card_Type := Card.Construct (Rank => Deck.King,
                                                         Suit => Deck.Diamond);
   begin
      pile.Push (c1);
      AUnit.Assertions.Assert (not pile.Has (c_has), "Has=True");
   end Has_Not_1_Card;

   procedure Has_Not_2_Cards (T : in out Test) is
      pragma Unreferenced (T);
      pile  : Pile_Of_Cards.LIFO.Pile_Type_LIFO
        := Pile_Of_Cards.LIFO.Construct;
      c1    : constant Card.Card_Type := Card.Construct (Rank => Deck.Ace,
                                                         Suit => Deck.Diamond);
      c2    : constant Card.Card_Type := Card.Construct (Rank => Deck.Two,
                                                         Suit => Deck.Diamond);
      c_has : constant Card.Card_Type := Card.Construct (Rank => Deck.King,
                                                         Suit => Deck.Diamond);
   begin
      pile.Push (c1);
      pile.Push (c2);
      AUnit.Assertions.Assert (not pile.Has (c_has), "Has=True");
   end Has_Not_2_Cards;

   procedure Has_1_Card (T : in out Test) is
      pragma Unreferenced (T);
      pile  : Pile_Of_Cards.LIFO.Pile_Type_LIFO
        := Pile_Of_Cards.LIFO.Construct;
      c1    : constant Card.Card_Type := Card.Construct (Rank => Deck.Ace,
                                                         Suit => Deck.Diamond);
      c_has : constant Card.Card_Type := c1;
   begin
      pile.Push (c1);
      AUnit.Assertions.Assert (pile.Has (c_has), "Has=False");
   end Has_1_Card;

   procedure Has_2_Cards_1st_Push (T : in out Test) is
      pragma Unreferenced (T);
      pile  : Pile_Of_Cards.LIFO.Pile_Type_LIFO
        := Pile_Of_Cards.LIFO.Construct;
      c1    : constant Card.Card_Type := Card.Construct (Rank => Deck.Ace,
                                                         Suit => Deck.Diamond);
      c2    : constant Card.Card_Type := Card.Construct (Rank => Deck.Two,
                                                         Suit => Deck.Diamond);
      c_has : constant Card.Card_Type := c1;
   begin
      pile.Push (c1);
      pile.Push (c2);
      AUnit.Assertions.Assert (pile.Has (c_has), "Has=False");
   end Has_2_Cards_1st_Push;

   procedure Has_2_Cards_2nd_Push (T : in out Test) is
      pragma Unreferenced (T);
      pile  : Pile_Of_Cards.LIFO.Pile_Type_LIFO
        := Pile_Of_Cards.LIFO.Construct;
      c1    : constant Card.Card_Type := Card.Construct (Rank => Deck.Ace,
                                                         Suit => Deck.Diamond);
      c2    : constant Card.Card_Type := Card.Construct (Rank => Deck.Two,
                                                         Suit => Deck.Diamond);
      c_has : constant Card.Card_Type := c2;
   begin
      pile.Push (c1);
      pile.Push (c2);
      AUnit.Assertions.Assert (pile.Has (c_has), "Has=False");
   end Has_2_Cards_2nd_Push;

   --------------------------------------------------------------------
   --  the test suit construction
   package Caller is new AUnit.Test_Caller (Pile_Of_Cards.LIFO.Test.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
      N   : constant String := "Pile_Of_Cards.LIFO.";
   begin
      --  ctor tests
      Ret.Add_Test (Caller.
                      Create (N & "Construct",
                        Construct'Access));

      --  Push/Pop
      Ret.Add_Test (Caller.
                      Create (N & "Push_One_Card",
                        Push_One_Card'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Push_Two_Cards",
                        Push_Two_Cards'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Pop_One_Pushed_One",
                        Pop_One_Pushed_One'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Pop_One_Pushed_Two",
                        Pop_One_Pushed_Two'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Pop_Two_Pushed_Two",
                        Pop_Two_Pushed_Two'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Pop_One_Pushed_None",
                        Pop_One_Pushed_None'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Pop_Two_Pushed_One",
                        Pop_Two_Pushed_One'Access));

      Ret.Add_Test (Caller.
                      Create (N & "Peek_No_Cards",
                        Peek_No_Cards'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Peek_One_Card",
                        Peek_One_Card'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Peek_Two_Cards",
                        Peek_Two_Cards'Access));

      Ret.Add_Test (Caller.
                      Create (N & "Has_Not_No_Cards",
                        Has_Not_No_Cards'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Has_Not_1_Card",
                        Has_Not_1_Card'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Has_Not_2_Cards",
                        Has_Not_2_Cards'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Has_1_Card",
                        Has_1_Card'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Has_2_Cards_1st_Push",
                        Has_2_Cards_1st_Push'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Has_2_Cards_2nd_Push",
                        Has_2_Cards_2nd_Push'Access));

      return Ret;
   end Suite;

end Pile_Of_Cards.LIFO.Test;
