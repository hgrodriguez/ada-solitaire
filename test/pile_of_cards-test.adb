with Ada.Exceptions;

with AUnit.Assertions;
with AUnit.Test_Caller;

with Definitions;
with Cards;
with Pile_Of_Cards;

package body Pile_Of_Cards.Test is

   --------------------------------------------------------------------
   --  all test procedures

   --------------------------------------------------------------------
   --  test constructing a pile
   procedure Construct (T : in out Test) is
      pragma Unreferenced (T);
      pile : Pile_Of_Cards.Pile_Type;

   begin
      pile := Pile_Of_Cards.Construct;
      AUnit.Assertions.Assert (pile.Is_Empty, "should be empty!");
      AUnit.Assertions.Assert (pile.Size = 0,
                               "size should=0, " &
                                 "but is=" & pile.Size'Image);
   end Construct;

   --------------------------------------------------------------------
   --  Peek operations
   --------------------------------------------------------------------
   procedure Peek_No_Cards_Exception;
   --------------------------------------------------------------------
   --
   procedure Peek_No_Cards_Exception is
      pile : constant Pile_Of_Cards.Pile_Type
        := Pile_Of_Cards.Construct;
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

   --------------------------------------------------------------------
   --
   procedure Peek_No_Cards (T : in out Test) is
      pragma Unreferenced (T);
   begin
      AUnit.Assertions.Assert_Exception (Peek_No_Cards_Exception'Access,
                                         "Peek_No_Cards: " &
                                           "no exception raised");
   end Peek_No_Cards;

   --------------------------------------------------------------------
   --
   procedure Peek_One_Card_FIFO (T : in out Test) is
      pragma Unreferenced (T);
      pile : Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;
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
   end Peek_One_Card_FIFO;

   --------------------------------------------------------------------
   --
   procedure Peek_Two_Cards_FIFO (T : in out Test) is
      pragma Unreferenced (T);
      pile : Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;
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
                                 " /= c2:" & c2.Image);
   end Peek_Two_Cards_FIFO;

   --------------------------------------------------------------------
   --  Put
   procedure Peek_One_Card_LIFO (T : in out Test) is
      pragma Unreferenced (T);
      pile : Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;
      c    : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
      r    : Card.Card_Type;
   begin
      pile.Push (c);
      r := pile.Peek;
      AUnit.Assertions.Assert (Cards.Is_Equal_To (r, c),
                               "r:" & r.Image &
                                 " /= c:" & c.Image);
   end Peek_One_Card_LIFO;

   --------------------------------------------------------------------
   --
   procedure Peek_Two_Cards_LIFO (T : in out Test) is
      pragma Unreferenced (T);
      pile : Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;
      c1   : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
      c2   : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Two,
                           Suit => Definitions.Diamond);
      r    : Card.Card_Type;
   begin
      pile.Push (c1);
      pile.Push (c2);
      r := pile.Peek;
      AUnit.Assertions.Assert (Cards.Is_Equal_To (r, c2),
                               "r:" & r.Image &
                                 " /= c2:" & c2.Image);
   end Peek_Two_Cards_LIFO;

   --------------------------------------------------------------------
   --  Has operations
   --------------------------------------------------------------------
   --------------------------------------------------------------------
   --
   procedure Has_Not_No_Cards (T : in out Test) is
      pragma Unreferenced (T);
      pile  : constant Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;
      c_has : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.King,
                           Suit => Definitions.Diamond);
   begin
      AUnit.Assertions.Assert (not pile.Has (c_has), "Has=True");
   end Has_Not_No_Cards;

   --------------------------------------------------------------------
   --
   procedure Has_Not_1_Card_FIFO (T : in out Test) is
      pragma Unreferenced (T);
      pile  : Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;
      c_put : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      c_has : constant Card.Card_Type
        := Card.Construct (Definitions.Two,
                           Definitions.Diamond);
   begin
      pile.Put (c_put);
      AUnit.Assertions.Assert (not pile.Has (c_has), "Has=True");
   end Has_Not_1_Card_FIFO;

   --------------------------------------------------------------------
   --
   procedure Has_Not_2_Cards_FIFO (T : in out Test) is
      pragma Unreferenced (T);
      pile   : Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;
      c_put1 : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      c_put2 : constant Card.Card_Type
        := Card.Construct (Definitions.King,
                           Definitions.Diamond);
      c_has  : constant Card.Card_Type
        := Card.Construct (Definitions.Two,
                           Definitions.Diamond);
   begin
      pile.Put (c_put1);
      pile.Put (c_put2);
      AUnit.Assertions.Assert (not pile.Has (c_has), "Has=True");
   end Has_Not_2_Cards_FIFO;

   --------------------------------------------------------------------
   --
   procedure Has_1_Card_FIFO (T : in out Test) is
      pragma Unreferenced (T);
      pile  : Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;
      c_put : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      c_has : constant Card.Card_Type := c_put;
   begin
      pile.Put (c_put);
      AUnit.Assertions.Assert (pile.Has (c_has), "Has=False");
   end Has_1_Card_FIFO;

   --------------------------------------------------------------------
   --
   procedure Has_2_Cards_1st_Put_FIFO (T : in out Test) is
      pragma Unreferenced (T);
      pile   : Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;
      c_put1 : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      c_put2 : constant Card.Card_Type
        := Card.Construct (Definitions.King,
                           Definitions.Diamond);
      c_has  : constant Card.Card_Type := c_put1;
   begin
      pile.Put (c_put1);
      pile.Put (c_put2);
      AUnit.Assertions.Assert (pile.Has (c_has), "Has=False");
   end Has_2_Cards_1st_Put_FIFO;

   --------------------------------------------------------------------
   --
   procedure Has_2_Cards_2nd_Put_FIFO (T : in out Test) is
      pragma Unreferenced (T);
      pile   : Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;
      c_put1 : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      c_put2 : constant Card.Card_Type
        := Card.Construct (Definitions.King,
                           Definitions.Diamond);
      c_has  : constant Card.Card_Type := c_put2;
   begin
      pile.Put (c_put1);
      pile.Put (c_put2);
      AUnit.Assertions.Assert (pile.Has (c_has), "Has=False");
   end Has_2_Cards_2nd_Put_FIFO;

   --------------------------------------------------------------------
   --
   procedure Has_Not_1_Card_LIFO (T : in out Test) is
      pragma Unreferenced (T);
      pile  : Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;
      c1    : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
      c_has : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.King,
                           Suit => Definitions.Diamond);
   begin
      pile.Push (c1);
      AUnit.Assertions.Assert (not pile.Has (c_has), "Has=True");
   end Has_Not_1_Card_LIFO;

   --------------------------------------------------------------------
   --
   procedure Has_Not_2_Cards_LIFO (T : in out Test) is
      pragma Unreferenced (T);
      pile  : Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;
      c1    : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
      c2    : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Two,
                           Suit => Definitions.Diamond);
      c_has : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.King,
                           Suit => Definitions.Diamond);
   begin
      pile.Push (c1);
      pile.Push (c2);
      AUnit.Assertions.Assert (not pile.Has (c_has), "Has=True");
   end Has_Not_2_Cards_LIFO;

   --------------------------------------------------------------------
   --
   procedure Has_1_Card_LIFO (T : in out Test) is
      pragma Unreferenced (T);
      pile  : Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;
      c1    : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
      c_has : constant Card.Card_Type := c1;
   begin
      pile.Push (c1);
      AUnit.Assertions.Assert (pile.Has (c_has), "Has=False");
   end Has_1_Card_LIFO;

   --------------------------------------------------------------------
   --
   procedure Has_2_Cards_1st_Push_LIFO (T : in out Test) is
      pragma Unreferenced (T);
      pile  : Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;
      c1    : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
      c2    : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Two,
                           Suit => Definitions.Diamond);
      c_has : constant Card.Card_Type := c1;
   begin
      pile.Push (c1);
      pile.Push (c2);
      AUnit.Assertions.Assert (pile.Has (c_has), "Has=False");
   end Has_2_Cards_1st_Push_LIFO;

   --------------------------------------------------------------------
   --
   procedure Has_2_Cards_2nd_Push_LIFO (T : in out Test) is
      pragma Unreferenced (T);
      pile  : Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;
      c1    : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
      c2    : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Two,
                           Suit => Definitions.Diamond);
      c_has : constant Card.Card_Type := c2;
   begin
      pile.Push (c1);
      pile.Push (c2);
      AUnit.Assertions.Assert (pile.Has (c_has), "Has=False");
   end Has_2_Cards_2nd_Push_LIFO;

   --------------------------------------------------------------------
   --  Put operations
   --------------------------------------------------------------------
   --------------------------------------------------------------------
   --
   procedure Put_One_Card_FIFO (T : in out Test) is
      pragma Unreferenced (T);
      c    : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
      pile : Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;

   begin
      pile.Put (c);
      AUnit.Assertions.Assert (not pile.Is_Empty, "should not be empty!");
      AUnit.Assertions.Assert (pile.Size = 1,
                               "size should=1, " &
                                 "but is=" & pile.Size'Image);
   end Put_One_Card_FIFO;

   --------------------------------------------------------------------
   --
   procedure Put_Two_Cards_FIFO (T : in out Test) is
      pragma Unreferenced (T);
      c1   : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
      c2   : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Two,
                           Suit => Definitions.Diamond);
      pile : Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;

   begin
      pile.Put (c1);
      pile.Put (c2);
      AUnit.Assertions.Assert (not pile.Is_Empty, "should not be empty!");
      AUnit.Assertions.Assert (pile.Size = 2,
                               "size should=2, " &
                                 "but is=" & pile.Size'Image);
   end Put_Two_Cards_FIFO;

   --------------------------------------------------------------------
   --  Get operations
   --------------------------------------------------------------------
   --------------------------------------------------------------------
   --
   procedure Get_One_Card_FIFO (T : in out Test) is
      pragma Unreferenced (T);
      c    : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
      pile : Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;
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
   end Get_One_Card_FIFO;

   --------------------------------------------------------------------
   --
   procedure Get_One_Card_Put_Two_FIFO (T : in out Test) is
      pragma Unreferenced (T);
      pile : Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;
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
   end Get_One_Card_Put_Two_FIFO;

   --------------------------------------------------------------------
   --
   procedure Get_Two_Cards_Put_Two_FIFO (T : in out Test) is
      pragma Unreferenced (T);
      pile : Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;
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
   end Get_Two_Cards_Put_Two_FIFO;

   --------------------------------------------------------------------
   --
   procedure Get_One_Card_No_Put_Exception_FIFO;
   procedure Get_One_Card_No_Put_Exception_FIFO is
      pile : Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;
      r    : Card.Card_Type;
      pragma Warnings (Off, r);
   begin
      r := pile.Get;
   exception
      when Pile_Empty_Exception => raise;
      when Exc : others =>
         AUnit.Assertions.Assert (False,
                                  "Get_One_Card_No_Put_Exception_FIFO: " &
                                    "wrong exception raised:" &
                                    Ada.Exceptions.Exception_Name (Exc));
   end Get_One_Card_No_Put_Exception_FIFO;

   procedure Get_One_Card_No_Put_FIFO (T : in out Test) is
      pragma Unreferenced (T);

   begin
      AUnit.Assertions.
        Assert_Exception (Get_One_Card_No_Put_Exception_FIFO'Access,
                          "Get_One_Card_No_Put: " &
                            "no exception raised");
   end Get_One_Card_No_Put_FIFO;

   --------------------------------------------------------------------
   --  Push operations
   --------------------------------------------------------------------
   --------------------------------------------------------------------
   --
   procedure Push_One_Card_LIFO (T : in out Test) is
      pragma Unreferenced (T);
      c    : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
      pile : Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;

   begin
      pile.Push (c);
      AUnit.Assertions.Assert (not pile.Is_Empty, "should not be empty!");
      AUnit.Assertions.Assert (pile.Size = 1,
                               "size should=1, " &
                                 "but is=" & pile.Size'Image);
   end Push_One_Card_LIFO;

   --------------------------------------------------------------------
   --
   procedure Push_Two_Cards_LIFO (T : in out Test) is
      pragma Unreferenced (T);
      c1   : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
      c2   : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Two,
                           Suit => Definitions.Diamond);
      pile : Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;

   begin
      pile.Push (c1);
      pile.Push (c2);
      AUnit.Assertions.Assert (not pile.Is_Empty, "should not be empty!");
      AUnit.Assertions.Assert (pile.Size = 2,
                               "size should=2, " &
                                 "but is=" & pile.Size'Image);
   end Push_Two_Cards_LIFO;

   --------------------------------------------------------------------
   --  Pop operations
   --------------------------------------------------------------------
   --------------------------------------------------------------------
   --
   procedure Pop_One_Pushed_One_LIFO (T : in out Test) is
      pragma Unreferenced (T);
      c    : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
      pile : Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;
      r    : Card.Card_Type;
   begin
      pile.Push (c);
      r := pile.Pop;
      AUnit.Assertions.Assert (Cards.Is_Equal_To (r, c), "pushed: " & c.Image &
                                 " /= r:" & r.Image);
   end Pop_One_Pushed_One_LIFO;

   --------------------------------------------------------------------
   --
   procedure Pop_One_Pushed_Two_LIFO (T : in out Test) is
      pragma Unreferenced (T);
      c1   : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
      c2   : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Two,
                           Suit => Definitions.Diamond);
      pile : Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;
      r    : Card.Card_Type;
   begin
      pile.Push (c1);
      pile.Push (c2);
      r := pile.Pop;
      AUnit.Assertions.Assert (Cards.Is_Equal_To (r, c2),
                               "pushed: " & c2.Image &
                                 " /= r:" & r.Image);
   end Pop_One_Pushed_Two_LIFO;

   --------------------------------------------------------------------
   --
   procedure Pop_Two_Pushed_Two_LIFO (T : in out Test) is
      pragma Unreferenced (T);
      c1   : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
      c2   : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Two,
                           Suit => Definitions.Diamond);
      pile : Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;
      r1   : Card.Card_Type;
      r2   : Card.Card_Type;
   begin
      pile.Push (c1);
      pile.Push (c2);
      r2 := pile.Pop;
      r1 := pile.Pop;
      AUnit.Assertions.Assert (Cards.Is_Equal_To (r1, c1),
                               "pushed: " & c1.Image &
                                 " /= r1:" & r1.Image);
      AUnit.Assertions.Assert (Cards.Is_Equal_To (r2, c2),
                               "pushed: " & c2.Image &
                                 " /= r2:" & r2.Image);
   end Pop_Two_Pushed_Two_LIFO;

   --------------------------------------------------------------------
   --
   procedure Pop_One_Pushed_None_Exception_LIFO;
   procedure Pop_One_Pushed_None_Exception_LIFO is
      pile : Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;
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
   end Pop_One_Pushed_None_Exception_LIFO;

   --------------------------------------------------------------------
   --
   procedure Pop_One_Pushed_None_LIFO (T : in out Test) is
      pragma Unreferenced (T);

   begin
      AUnit.Assertions.
        Assert_Exception (Pop_One_Pushed_None_Exception_LIFO'Access,
                          "Pop_One_Pushed_None: " &
                            "no exception raised");
   end Pop_One_Pushed_None_LIFO;

   --------------------------------------------------------------------
   --
   procedure Pop_Two_Pushed_One_Exception_LIFO;
   procedure Pop_Two_Pushed_One_Exception_LIFO is
      c    : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
      pile : Pile_Of_Cards.Pile_Type := Pile_Of_Cards.Construct;
      r    : Card.Card_Type;
      pragma Warnings (Off, r);
   begin
      pile.Push (c);
      r := pile.Pop;
      r := pile.Pop;
   exception
      when Pile_Empty_Exception => raise;
      when Exc : others =>
         AUnit.Assertions.Assert (False,
                                  "Pop_Two_Pushed_One_Exception_LIFO: " &
                                    "wrong exception raised:" &
                                    Ada.Exceptions.Exception_Name (Exc));
   end Pop_Two_Pushed_One_Exception_LIFO;

   --------------------------------------------------------------------
   --
   procedure Pop_Two_Pushed_One_LIFO (T : in out Test) is
      pragma Unreferenced (T);
   begin
      AUnit.Assertions.
        Assert_Exception (Pop_Two_Pushed_One_Exception_LIFO'Access,
                          "Pop_Two_Pushed_One: " &
                            "no exception raised");
   end Pop_Two_Pushed_One_LIFO;

   --------------------------------------------------------------------
   --  the test suit construction
   package Caller is new AUnit.Test_Caller
     (Pile_Of_Cards.Test.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
      N   : constant String := "Pile_Of_Cards.Test.";
   begin
      --  ctor tests
      Ret.Add_Test (Caller.
                      Create (N & "Construct",
                        Construct'Access));

      --  Peek
      Ret.Add_Test (Caller.
                      Create (N & "Peek_No_Cards",
                        Peek_No_Cards'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Peek_One_Card_FIFO",
                        Peek_One_Card_FIFO'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Peek_Two_Cards_FIFO",
                        Peek_Two_Cards_FIFO'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Peek_One_Card_LIFO",
                        Peek_One_Card_LIFO'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Peek_Two_Cards_LIFO",
                        Peek_Two_Cards_LIFO'Access));

      --  Has
      Ret.Add_Test (Caller.
                      Create (N & "Has_Not_No_Cards",
                        Has_Not_No_Cards'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Has_Not_1_Card_FIFO",
                        Has_Not_1_Card_FIFO'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Has_Not_2_Cards_FIFO",
                        Has_Not_2_Cards_FIFO'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Has_1_Card_FIFO",
                        Has_1_Card_FIFO'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Has_2_Cards_1st_Put_FIFO",
                        Has_2_Cards_1st_Put_FIFO'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Has_2_Cards_2nd_Put_FIFO",
                        Has_2_Cards_2nd_Put_FIFO'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Has_Not_1_Card_LIFO",
                        Has_Not_1_Card_LIFO'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Has_Not_2_Cards_LIFO",
                        Has_Not_2_Cards_LIFO'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Has_1_Card_LIFO",
                        Has_1_Card_LIFO'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Has_2_Cards_1st_Push_LIFO",
                        Has_2_Cards_1st_Push_LIFO'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Has_2_Cards_2nd_Push_LIFO",
                        Has_2_Cards_2nd_Push_LIFO'Access));

      --  Put
      Ret.Add_Test (Caller.
                      Create (N & "Put_One_Card_FIFO",
                        Put_One_Card_FIFO'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Put_Two_Cards_FIFO",
                        Put_Two_Cards_FIFO'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Get_One_Card_FIFO",
                        Get_One_Card_FIFO'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Get_One_Card_Put_Two_FIFO",
                        Get_One_Card_Put_Two_FIFO'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Get_Two_Cards_Put_Two_FIFO",
                        Get_Two_Cards_Put_Two_FIFO'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Get_One_Card_No_Put_FIFO",
                        Get_One_Card_No_Put_FIFO'Access));

      --  Push
      Ret.Add_Test (Caller.
                      Create (N & "Push_One_Card_LIFO",
                        Push_One_Card_LIFO'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Push_Two_Cards_LIFO",
                        Push_Two_Cards_LIFO'Access));

      --  Pop
      Ret.Add_Test (Caller.
                      Create (N & "Pop_One_Pushed_One_LIFO",
                        Pop_One_Pushed_One_LIFO'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Pop_One_Pushed_Two_LIFO",
                        Pop_One_Pushed_Two_LIFO'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Pop_Two_Pushed_Two_LIFO",
                        Pop_Two_Pushed_Two_LIFO'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Pop_One_Pushed_None_LIFO",
                        Pop_One_Pushed_None_LIFO'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Pop_Two_Pushed_One_LIFO",
                        Pop_Two_Pushed_One_LIFO'Access));
      --
      return Ret;
   end Suite;

end Pile_Of_Cards.Test;
