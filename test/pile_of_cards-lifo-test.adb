with Ada.Exceptions;

with Definitions;
with Card;
with Cards;
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
      c    : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
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
      c1   : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
      c2   : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Two,
                           Suit => Definitions.Diamond);
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
      c    : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
      pile : Pile_Of_Cards.LIFO.Pile_Type_LIFO := Pile_Of_Cards.LIFO.Construct;
      r    : Card.Card_Type;
   begin
      pile.Push (c);
      r := pile.Pop;
      AUnit.Assertions.Assert (Cards.Is_Equal_To (r, c), "pushed: " & c.Image &
                                 " /= r:" & r.Image);
   end Pop_One_Pushed_One;

   procedure Pop_One_Pushed_Two (T : in out Test) is
      pragma Unreferenced (T);
      c1   : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
      c2   : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Two,
                           Suit => Definitions.Diamond);
      pile : Pile_Of_Cards.LIFO.Pile_Type_LIFO := Pile_Of_Cards.LIFO.Construct;
      r    : Card.Card_Type;
   begin
      pile.Push (c1);
      pile.Push (c2);
      r := pile.Pop;
      AUnit.Assertions.Assert (Cards.Is_Equal_To (r, c2),
                               "pushed: " & c2.Image &
                                 " /= r:" & r.Image);
   end Pop_One_Pushed_Two;

   procedure Pop_Two_Pushed_Two (T : in out Test) is
      pragma Unreferenced (T);
      c1   : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
      c2   : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Two,
                           Suit => Definitions.Diamond);
      pile : Pile_Of_Cards.LIFO.Pile_Type_LIFO := Pile_Of_Cards.LIFO.Construct;
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
      c    : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
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
   end Peek_One_Card;

   procedure Peek_Two_Cards (T : in out Test) is
      pragma Unreferenced (T);
      pile : Pile_Of_Cards.LIFO.Pile_Type_LIFO := Pile_Of_Cards.LIFO.Construct;
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
   end Peek_Two_Cards;

   --------------------------------------------------------------------
   --  Has operations

   procedure Has_Not_No_Cards (T : in out Test) is
      pragma Unreferenced (T);
      pile  : constant Pile_Of_Cards.LIFO.Pile_Type_LIFO
        := Pile_Of_Cards.LIFO.Construct;
      c_has : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.King,
                           Suit => Definitions.Diamond);
   begin
      AUnit.Assertions.Assert (not pile.Has (c_has), "Has=True");
   end Has_Not_No_Cards;

   procedure Has_Not_1_Card (T : in out Test) is
      pragma Unreferenced (T);
      pile  : Pile_Of_Cards.LIFO.Pile_Type_LIFO
        := Pile_Of_Cards.LIFO.Construct;
      c1    : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
      c_has : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.King,
                           Suit => Definitions.Diamond);
   begin
      pile.Push (c1);
      AUnit.Assertions.Assert (not pile.Has (c_has), "Has=True");
   end Has_Not_1_Card;

   procedure Has_Not_2_Cards (T : in out Test) is
      pragma Unreferenced (T);
      pile  : Pile_Of_Cards.LIFO.Pile_Type_LIFO
        := Pile_Of_Cards.LIFO.Construct;
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
   end Has_Not_2_Cards;

   procedure Has_1_Card (T : in out Test) is
      pragma Unreferenced (T);
      pile  : Pile_Of_Cards.LIFO.Pile_Type_LIFO
        := Pile_Of_Cards.LIFO.Construct;
      c1    : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Ace,
                           Suit => Definitions.Diamond);
      c_has : constant Card.Card_Type := c1;
   begin
      pile.Push (c1);
      AUnit.Assertions.Assert (pile.Has (c_has), "Has=False");
   end Has_1_Card;

   procedure Has_2_Cards_1st_Push (T : in out Test) is
      pragma Unreferenced (T);
      pile  : Pile_Of_Cards.LIFO.Pile_Type_LIFO
        := Pile_Of_Cards.LIFO.Construct;
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
   end Has_2_Cards_1st_Push;

   procedure Has_2_Cards_2nd_Push (T : in out Test) is
      pragma Unreferenced (T);
      pile  : Pile_Of_Cards.LIFO.Pile_Type_LIFO
        := Pile_Of_Cards.LIFO.Construct;
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
   end Has_2_Cards_2nd_Push;

   --------------------------------------------------------------------
   --  Collect tests
   procedure Collect_Is_Empty (T : in out Test) is
      pragma Unreferenced (T);
      pile     : constant Pile_Of_Cards.LIFO.Pile_Type_LIFO
        := Pile_Of_Cards.LIFO.Construct;
      sif      : Short_Image_FIFO.Short_Image_FIFO_Type;
      Expected : constant Natural := 0;
      Actual   : Natural;
   begin
      pile.Collect (sif);
      Actual := sif.Size;
      AUnit.Assertions.Assert (Expected = Actual,
                               "expected=" & Expected'Image &
                                 " /= actual:" & Actual'Image);
   end Collect_Is_Empty;

   procedure Collect_1_Card (T : in out Test) is
      pragma Unreferenced (T);
      pile           : Pile_Of_Cards.LIFO.Pile_Type_LIFO
        := Pile_Of_Cards.LIFO.Construct;
      C1             : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      SI1            : constant Card.Short_Image_Type := C1.Short_Image;
      sif            : Short_Image_FIFO.Short_Image_FIFO_Type;
      Expected_Size  : constant Natural := 1;
      Actual_Size    : Natural;
      Expected_SI1   : constant Card.Short_Image_Type := SI1;
      Actual_SI1     : Card.Short_Image_Type;
   begin
      pile.Push (C1);
      pile.Collect (sif);
      Actual_Size := sif.Size;
      AUnit.Assertions.Assert (Expected_Size = Actual_Size,
                               "expected_size=" & Expected_Size'Image &
                                 " /= actual_size:" & Actual_Size'Image);
      Actual_SI1 := sif.Get;
      AUnit.Assertions.Assert (Expected_SI1 = Actual_SI1,
                               "Expected_SI1=" & Expected_SI1 &
                                 " /= Actual_SI1:" & Actual_SI1);
   end Collect_1_Card;

   procedure Collect_2_Cards (T : in out Test) is
      pragma Unreferenced (T);
      pile           : Pile_Of_Cards.LIFO.Pile_Type_LIFO
        := Pile_Of_Cards.LIFO.Construct;
      C1             : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      SI1            : constant Card.Short_Image_Type := C1.Short_Image;
      C2             : constant Card.Card_Type
        := Card.Construct (Definitions.King,
                           Definitions.Club);
      SI2            : constant Card.Short_Image_Type := C2.Short_Image;
      sif            : Short_Image_FIFO.Short_Image_FIFO_Type;
      Expected_Size  : constant Natural := 2;
      Actual_Size    : Natural;
      Expected_SI1   : constant Card.Short_Image_Type := SI1;
      Actual_SI1     : Card.Short_Image_Type;
      Expected_SI2   : constant Card.Short_Image_Type := SI2;
      Actual_SI2     : Card.Short_Image_Type;
   begin
      pile.Push (C1);
      pile.Push (C2);
      pile.Collect (sif);
      Actual_Size := sif.Size;
      AUnit.Assertions.Assert (Expected_Size = Actual_Size,
                               "expected_size=" & Expected_Size'Image &
                                 " /= actual_size:" & Actual_Size'Image);
      Actual_SI1 := sif.Get;
      AUnit.Assertions.Assert (Expected_SI1 = Actual_SI1,
                               "Expected_SI1=" & Expected_SI1 &
                                 " /= Actual_SI1:" & Actual_SI1);
      Actual_SI2 := sif.Get;
      AUnit.Assertions.Assert (Expected_SI2 = Actual_SI2,
                               "Expected_SI2=" & Expected_SI2 &
                                 " /= Actual_SI2:" & Actual_SI2);
   end Collect_2_Cards;

   --------------------------------------------------------------------
   --  Peek_Bottom operations
   --------------------------------------------------------------------
   procedure Peek_Bottom_No_Cards_Exception;
   --------------------------------------------------------------------
   --
   procedure Peek_Bottom_No_Cards_Exception is
      pile : constant Pile_Of_Cards.LIFO.Pile_Type_LIFO
        := Pile_Of_Cards.LIFO.Construct;
      r    : Card.Card_Type;
      pragma Warnings (Off, r);
   begin
      r := pile.Peek_Bottom;
   exception
      when Pile_Empty_Exception => raise;
      when Exc : others =>
         AUnit.Assertions.Assert (False,
                                  "Peek_Bottom_No_Cards_Exception: " &
                                    "wrong exception raised:" &
                                    Ada.Exceptions.Exception_Name (Exc));
   end Peek_Bottom_No_Cards_Exception;

   --------------------------------------------------------------------
   --
   procedure Peek_Bottom_No_Cards (T : in out Test) is
      pragma Unreferenced (T);
   begin
      AUnit.Assertions.Assert_Exception (Peek_Bottom_No_Cards_Exception'Access,
                                         "Peek_No_Cards: " &
                                           "no exception raised");
   end Peek_Bottom_No_Cards;

   --------------------------------------------------------------------
   --
   procedure Peek_Bottom_One_Card (T : in out Test) is
      pragma Unreferenced (T);
      pile    : Pile_Of_Cards.LIFO.Pile_Type_LIFO
        := Pile_Of_Cards.LIFO.Construct;
      c       : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      Actual  : Card.Card_Type;
   begin
      pile.Push (c);
      Actual := pile.Peek_Bottom;
      AUnit.Assertions.Assert (Cards.Is_Equal_To (Actual, c),
                               "Actual=" & Actual.Short_Image &
                                 " /= Expected=" & c.Short_Image);
   end Peek_Bottom_One_Card;

   --------------------------------------------------------------------
   --
   procedure Peek_Bottom_Two_Cards (T : in out Test) is
      pragma Unreferenced (T);
      pile    : Pile_Of_Cards.LIFO.Pile_Type_LIFO
        := Pile_Of_Cards.LIFO.Construct;
      c1      : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      c2      : constant Card.Card_Type
        := Card.Construct (Definitions.Two,
                           Definitions.Diamond);
      Actual  : Card.Card_Type;
   begin
      pile.Push (c1);
      pile.Push (c2);
      Actual := pile.Peek_Bottom;
      AUnit.Assertions.Assert (Cards.Is_Equal_To (Actual, c1),
                               "Actual=" & Actual.Short_Image &
                                 " /= Expected=" & c1.Short_Image);
   end Peek_Bottom_Two_Cards;

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

      --  Collect tests
      Ret.Add_Test (Caller.
                      Create (N & "Collect_Is_Empty",
                        Collect_Is_Empty'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Collect_1_Card",
                        Collect_1_Card'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Collect_2_Cards",
                        Collect_2_Cards'Access));

      --  Peek_Bottom tests
      Ret.Add_Test (Caller.
                      Create (N & "Peek_Bottom_No_Cards",
                        Peek_Bottom_No_Cards'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Peek_Bottom_One_Card",
                        Peek_Bottom_One_Card'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Peek_Bottom_Two_Cards",
                        Peek_Bottom_Two_Cards'Access));
      --
      return Ret;
   end Suite;

end Pile_Of_Cards.LIFO.Test;
