with Ada.Exceptions;

with Definitions;
with Card;
with Cards;
with Pile_Of_Cards.FIFO;

with AUnit.Assertions;
with AUnit.Test_Caller;

package body Pile_Of_Cards.FIFO.Test is

   --------------------------------------------------------------------
   --  Constructs
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

   --------------------------------------------------------------------
   --  Puts
   procedure Put_One_Card (T : in out Test) is
      pragma Unreferenced (T);
      c    : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
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
      c1   : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
      c2   : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Two,
                           Suit => Definitions.Diamond);
      pile : Pile_Of_Cards.FIFO.Pile_Type_FIFO := Pile_Of_Cards.FIFO.Construct;

   begin
      pile.Put (c1);
      pile.Put (c2);
      AUnit.Assertions.Assert (not pile.Is_Empty, "should not be empty!");
      AUnit.Assertions.Assert (pile.Size = 2,
                               "size should=2, " &
                                 "but is=" & pile.Size'Image);
   end Put_Two_Cards;

   --------------------------------------------------------------------
   --  Gets
   procedure Get_One_Card (T : in out Test) is
      pragma Unreferenced (T);
      c    : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
      pile : Pile_Of_Cards.FIFO.Pile_Type_FIFO := Pile_Of_Cards.FIFO.Construct;
      r    : Card.Card_Type;

   begin
      pile.Put (c);
      r := pile.Get;
      AUnit.Assertions.Assert (pile.Is_Empty, "should be empty!");
      AUnit.Assertions.Assert (pile.Size = 0,
                               "size should=0, " &
                                 "but is=" & pile.Size'Image);
      AUnit.Assertions.Assert (Cards.Is_Equal_To (r, c),
                               "r:" & r.Image &
                                 " /= c:" & c.Image);
   end Get_One_Card;

   procedure Get_One_Card_Put_Two (T : in out Test) is
      pragma Unreferenced (T);
      pile : Pile_Of_Cards.FIFO.Pile_Type_FIFO := Pile_Of_Cards.FIFO.Construct;
      c1   : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
      c2   : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Two,
                           Suit => Definitions.Diamond);
      r    : Card.Card_Type;
   begin
      pile.Put (c1);
      pile.Put (c2);
      r := pile.Get;
      AUnit.Assertions.Assert (pile.Size = 1,
                               "size should=1, " &
                                 "but is=" & pile.Size'Image);
      AUnit.Assertions.Assert (Cards.Is_Equal_To (r, c1),
                               "r:" & r.Image &
                                 " /= c:" & c1.Image);
   end Get_One_Card_Put_Two;

   procedure Get_Two_Cards_Put_Two (T : in out Test) is
      pragma Unreferenced (T);
      pile : Pile_Of_Cards.FIFO.Pile_Type_FIFO := Pile_Of_Cards.FIFO.Construct;
      c1   : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
      c2   : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Two,
                           Suit => Definitions.Diamond);
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
      AUnit.Assertions.Assert (Cards.Is_Equal_To (r1, c1),
                               "r1:" & r1.Image &
                                 " /= c1:" & c1.Image);
      AUnit.Assertions.Assert (Cards.Is_Equal_To (r2, c2),
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
   --  Peeks
   procedure Peek_No_Cards_Exception;
   procedure Peek_No_Cards_Exception is
      pile : constant Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Pile_Of_Cards.FIFO.Construct;
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
      pile : Pile_Of_Cards.FIFO.Pile_Type_FIFO := Pile_Of_Cards.FIFO.Construct;
      c    : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
      r    : Card.Card_Type;
   begin
      pile.Put (c);
      r := pile.Peek;
      AUnit.Assertions.Assert (Cards.Is_Equal_To (r, c),
                               "r:" & r.Image &
                                 " /= c:" & c.Image);
   end Peek_One_Card;

   procedure Peek_Two_Cards (T : in out Test) is
      pragma Unreferenced (T);
      pile : Pile_Of_Cards.FIFO.Pile_Type_FIFO := Pile_Of_Cards.FIFO.Construct;
      c1   : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
      c2   : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Two,
                           Suit => Definitions.Diamond);
      r    : Card.Card_Type;
   begin
      pile.Put (c1);
      pile.Put (c2);
      r := pile.Peek;
      AUnit.Assertions.Assert (Cards.Is_Equal_To (r, c1),
                               "r:" & r.Image &
                                 " /= c1:" & c1.Image);
   end Peek_Two_Cards;

   --------------------------------------------------------------------
   --  Has's
   procedure Has_Not_No_Cards (T : in out Test) is
      pragma Unreferenced (T);
      pile : constant Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Pile_Of_Cards.FIFO.Construct;
      c    : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
   begin
      AUnit.Assertions.Assert (not pile.Has (c), "Has=True");
   end Has_Not_No_Cards;

   procedure Has_Not_1_Card (T : in out Test) is
      pragma Unreferenced (T);
      pile  : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Pile_Of_Cards.FIFO.Construct;
      c_put : constant Card.Card_Type := Card.Construct (Definitions.Ace,
                                                         Definitions.Diamond);
      c_has : constant Card.Card_Type := Card.Construct (Definitions.Two,
                                                         Definitions.Diamond);
   begin
      pile.Put (c_put);
      AUnit.Assertions.Assert (not pile.Has (c_has), "Has=True");
   end Has_Not_1_Card;

   procedure Has_Not_2_Cards (T : in out Test) is
      pragma Unreferenced (T);
      pile   : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Pile_Of_Cards.FIFO.Construct;
      c_put1 : constant Card.Card_Type := Card.Construct (Definitions.Ace,
                                                          Definitions.Diamond);
      c_put2 : constant Card.Card_Type := Card.Construct (Definitions.King,
                                                          Definitions.Diamond);
      c_has  : constant Card.Card_Type := Card.Construct (Definitions.Two,
                                                          Definitions.Diamond);
   begin
      pile.Put (c_put1);
      pile.Put (c_put2);
      AUnit.Assertions.Assert (not pile.Has (c_has), "Has=True");
   end Has_Not_2_Cards;

   procedure Has_1_Card (T : in out Test) is
      pragma Unreferenced (T);
      pile  : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Pile_Of_Cards.FIFO.Construct;
      c_put : constant Card.Card_Type := Card.Construct (Definitions.Ace,
                                                         Definitions.Diamond);
      c_has : constant Card.Card_Type := c_put;
   begin
      pile.Put (c_put);
      AUnit.Assertions.Assert (pile.Has (c_has), "Has=False");
   end Has_1_Card;

   procedure Has_2_Cards_1st_Put (T : in out Test) is
      pragma Unreferenced (T);
      pile   : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Pile_Of_Cards.FIFO.Construct;
      c_put1 : constant Card.Card_Type := Card.Construct (Definitions.Ace,
                                                          Definitions.Diamond);
      c_put2 : constant Card.Card_Type := Card.Construct (Definitions.King,
                                                          Definitions.Diamond);
      c_has  : constant Card.Card_Type := c_put1;
   begin
      pile.Put (c_put1);
      pile.Put (c_put2);
      AUnit.Assertions.Assert (pile.Has (c_has), "Has=False");
   end Has_2_Cards_1st_Put;

   procedure Has_2_Cards_2nd_Put (T : in out Test) is
      pragma Unreferenced (T);
      pile   : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Pile_Of_Cards.FIFO.Construct;
      c_put1 : constant Card.Card_Type := Card.Construct (Definitions.Ace,
                                                          Definitions.Diamond);
      c_put2 : constant Card.Card_Type := Card.Construct (Definitions.King,
                                                          Definitions.Diamond);
      c_has  : constant Card.Card_Type := c_put2;
   begin
      pile.Put (c_put1);
      pile.Put (c_put2);
      AUnit.Assertions.Assert (pile.Has (c_has), "Has=False");
   end Has_2_Cards_2nd_Put;

   --------------------------------------------------------------------
   --  the test suit construction
   package Caller is new AUnit.Test_Caller (Pile_Of_Cards.FIFO.Test.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
      N   : constant String := "Pile_Of_Cards.FIFO.";
   begin
      --  ctor tests
      Ret.Add_Test (Caller.
                      Create (N & "Construct",
                        Construct'Access));

      --  FIFO tests
      Ret.Add_Test (Caller.
                      Create (N & "Put_One_Card",
                        Put_One_Card'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Put_Two_Cards",
                        Put_Two_Cards'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Get_One_Card",
                        Get_One_Card'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Get_One_Card_Put_Two",
                        Get_One_Card_Put_Two'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Get_Two_Cards_Put_Two",
                        Get_Two_Cards_Put_Two'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Get_One_Card_No_Put",
                        Get_One_Card_No_Put'Access));

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
                      Create (N & "Has_2_Cards_1st_Put",
                        Has_2_Cards_1st_Put'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Has_2_Cards_2nd_Put",
                        Has_2_Cards_2nd_Put'Access));

      return Ret;
   end Suite;

end Pile_Of_Cards.FIFO.Test;
