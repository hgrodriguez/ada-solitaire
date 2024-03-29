with Ada.Exceptions;

with AUnit.Assertions;
with AUnit.Test_Caller;

with Definitions;
with Card;
with Cards;

package body Tableau_Stack.Test is

   NUMBER_FOR_STACK : constant Integer := 1;

   --------------------------------------------------------------------
   --  constructors
   procedure Construct (T : in out Test) is
      pragma Unreferenced (T);
      stack : constant Tableau_Stack.Stack_Type
        := Tableau_Stack.Construct (NUMBER_FOR_STACK);
      pragma Warnings (Off, stack);
   begin
      null;
   end Construct;

   procedure Construct_Check_Size (T : in out Test) is
      pragma Unreferenced (T);
      stack : constant Tableau_Stack.Stack_Type
        := Tableau_Stack.Construct (NUMBER_FOR_STACK);
   begin
      AUnit.Assertions.Assert (stack.Size = 0,
                               "size=0" &
                                 " /= " & stack.Size'Image);
   end Construct_Check_Size;

   procedure Construct_Check_Is_Empty (T : in out Test) is
      pragma Unreferenced (T);
      stack : constant Tableau_Stack.Stack_Type
        := Tableau_Stack.Construct (NUMBER_FOR_STACK);
   begin
      AUnit.Assertions.Assert (stack.Is_Empty, "not stack.Is_Empty");
   end Construct_Check_Is_Empty;

   --------------------------------------------------------------------
   --
   procedure Construct_Check_Number (T : in out Test) is
      pragma Unreferenced (T);
      stack : constant Tableau_Stack.Stack_Type
        := Tableau_Stack.Construct (NUMBER_FOR_STACK);
   begin
      AUnit.Assertions.
        Assert (stack.Number = NUMBER_FOR_STACK,
                "stack.Number=" & stack.Number'Image &
                  " /=" & NUMBER_FOR_STACK'Image);
   end Construct_Check_Number;

   procedure Construct_Check_Peek_Fails_Exception;
   procedure Construct_Check_Peek_Fails_Exception is
      Stack : constant Tableau_Stack.Stack_Type
        := Tableau_Stack.Construct (NUMBER_FOR_STACK);
      C     : Card.Card_Type;
      pragma Warnings (Off, C);
   begin
      C := Stack.Peek;
   exception
      when Tableau_Stack_Empty_Exception => raise;
      when Exc : others =>
         AUnit.
           Assertions.
             Assert (False,
                     "Construct_Check_Pop_Fails_Exception: " &
                       "wrong exception raised:" &
                       Ada.Exceptions.Exception_Name (Exc));
   end Construct_Check_Peek_Fails_Exception;

   procedure Construct_Check_Peek_Fails (T : in out Test) is
      pragma Unreferenced (T);
   begin
      AUnit.
        Assertions.
          Assert_Exception (Construct_Check_Peek_Fails_Exception'Access,
                            "Construct_Check_Peek_Fails: " &
                              "no exception raised");
   end Construct_Check_Peek_Fails;

   procedure Construct_Check_Pop_Fails_Exception;
   procedure Construct_Check_Pop_Fails_Exception is
      Stack : constant Tableau_Stack.Stack_Type
        := Tableau_Stack.Construct (NUMBER_FOR_STACK);
      C     : Card.Card_Type;
      pragma Warnings (Off, C);
   begin
      C := Stack.Pop;
   exception
      when Tableau_Stack_Empty_Exception => raise;
      when Exc : others =>
         AUnit.
           Assertions.
             Assert (False,
                     "Construct_Check_Pop_Fails_Exception: " &
                       "wrong exception raised:" &
                       Ada.Exceptions.Exception_Name (Exc));
   end Construct_Check_Pop_Fails_Exception;

   procedure Construct_Check_Pop_Fails (T : in out Test) is
      pragma Unreferenced (T);
   begin
      AUnit.
        Assertions.
          Assert_Exception (Construct_Check_Pop_Fails_Exception'Access,
                            "Construct_Check_Pop_Fails: " &
                              "no exception raised");
   end Construct_Check_Pop_Fails;

   --------------------------------------------------------------------
   --  Has
   procedure Does_Not_Have_Empty_Stack (T : in out Test)  is
      pragma Unreferenced (T);
      stack : constant Tableau_Stack.Stack_Type
        := Tableau_Stack.Construct (NUMBER_FOR_STACK);
      C_N   : constant Card.Card_Type := Card.Construct (Definitions.Two,
                                                         Definitions.Diamond);
   begin
      AUnit.Assertions.Assert (not stack.Has (C_N), "Has=False /= True");
   end Does_Not_Have_Empty_Stack;

   procedure Does_Not_Have_Not_Empty_Stack (T : in out Test) is
      pragma Unreferenced (T);
      stack : constant Tableau_Stack.Stack_Type
        := Tableau_Stack.Construct (NUMBER_FOR_STACK);
      C_N   : constant Card.Card_Type := Card.Construct (Definitions.Two,
                                                         Definitions.Diamond);
   begin
      stack.Push_Unchecked (Card.Construct
                            (Definitions.Ten, Definitions.Heart));
      AUnit.Assertions.Assert (not stack.Has (C_N), "Has=False /= True");
   end Does_Not_Have_Not_Empty_Stack;

   procedure Does_Have_Not_Empty_Stack (T : in out Test) is
      pragma Unreferenced (T);
      stack : constant Tableau_Stack.Stack_Type
        := Tableau_Stack.Construct (NUMBER_FOR_STACK);
      C_N   : constant Card.Card_Type := Card.Construct (Definitions.Two,
                                                         Definitions.Diamond);
   begin
      stack.Push_Unchecked (C_N);
      AUnit.Assertions.Assert (stack.Has (C_N), "Has=True /= False");
   end Does_Have_Not_Empty_Stack;

   --------------------------------------------------------------------
   --  Push
   procedure Empty_Stack_Push_King_Check_Size (T : in out Test) is
      pragma Unreferenced (T);
      stack : constant Tableau_Stack.Stack_Type
        := Tableau_Stack.Construct (NUMBER_FOR_STACK);
   begin
      stack.Push_Checked (Card.Construct
                          (Definitions.King, Definitions.Diamond));
      AUnit.Assertions.Assert (stack.Size = 1,
                               "size=1" &
                                 " /= " & stack.Size'Image);
   end Empty_Stack_Push_King_Check_Size;

   procedure Empty_Stack_Push_King_Check_Peek (T : in out Test) is
      pragma Unreferenced (T);
      stack : constant Tableau_Stack.Stack_Type
        := Tableau_Stack.Construct (NUMBER_FOR_STACK);
      C_E   : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.King,
                           Suit => Definitions.Diamond);
      C     : Card.Card_Type;
   begin
      stack.Push_Checked (C_E);
      C := stack.Peek;
      AUnit.Assertions.Assert (Cards.Is_Equal_To (C, C_E),
                               "C_E=" & C_E.Image &
                                 " /= " & C.Image);
   end Empty_Stack_Push_King_Check_Peek;

   procedure Empty_Stack_Push_King_Check_Pop (T : in out Test) is
      pragma Unreferenced (T);
      stack : constant Tableau_Stack.Stack_Type
        := Tableau_Stack.Construct (NUMBER_FOR_STACK);
      C_E   : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.King,
                           Suit => Definitions.Diamond);
      C     : Card.Card_Type;
   begin
      stack.Push_Checked (C_E);
      C := stack.Pop;
      AUnit.Assertions.Assert (Cards.Is_Equal_To (C, C_E),
                               "C_E=" & C_E.Image &
                                 " /= " & C.Image);
   end Empty_Stack_Push_King_Check_Pop;

   procedure Empty_Stack_Push_Other_Than_King_Exception;
   procedure Empty_Stack_Push_Other_Than_King_Exception is
      Stack : constant Tableau_Stack.Stack_Type
        := Tableau_Stack.Construct (NUMBER_FOR_STACK);
      C     : constant Card.Card_Type
        := Card.Construct (Definitions.Ace, Definitions.Diamond);
   begin
      Stack.Push_Checked (C);
   exception
      when Tableau_Stack_Wrong_Card_Exception => raise;
      when Exc : others =>
         AUnit.
           Assertions.
             Assert (False,
                     "Empty_Stack_Push_Other_Than_King_Exception: " &
                       "wrong exception raised:" &
                       Ada.Exceptions.Exception_Name (Exc));
   end Empty_Stack_Push_Other_Than_King_Exception;

   procedure Empty_Stack_Push_Other_Than_King (T : in out Test) is
      pragma Unreferenced (T);
   begin
      AUnit.
        Assertions.
          Assert_Exception (Empty_Stack_Push_Other_Than_King_Exception'Access,
                            "Pop_One_Pushed_None: " &
                              "no exception raised");
   end Empty_Stack_Push_Other_Than_King;

   procedure Push_Multiple_Cards_Check_Size (T : in out Test) is
      pragma Unreferenced (T);
      stack : constant Tableau_Stack.Stack_Type
        := Tableau_Stack.Construct (NUMBER_FOR_STACK);
   begin
      stack.Push_Unchecked (Card.Construct (Rank => Definitions.Three,
                                            Suit => Definitions.Diamond));
      stack.Push_Unchecked (Card.Construct (Rank => Definitions.Three,
                                            Suit => Definitions.Club));
      stack.Push_Unchecked (Card.Construct (Rank => Definitions.Ten,
                                            Suit => Definitions.Spade));
      AUnit.Assertions.Assert (stack.Size = 3,
                               "size=1" &
                                 " /= " & stack.Size'Image);
   end Push_Multiple_Cards_Check_Size;

   --------------------------------------------------------------------
   --  Accepts
   procedure Empty_Stack_Accepts_All_Kings (T : in out Test) is
      pragma Unreferenced (T);
      stack       : constant Tableau_Stack.Stack_Type
        := Tableau_Stack.Construct (NUMBER_FOR_STACK);
      Acceptables : Tableau_Stack.Acceptable_Type := stack.Accepts;
      Seen        : array (Definitions.Suits_Valid_Range) of Boolean;
      C           : Card.Card_Type;
      use Definitions;
   begin
      AUnit.Assertions.Assert (Acceptables.Size = 4,
                               "size=4" &
                                 " /= " & Acceptables.Size'Image);
      for suit in Definitions.Suits_Valid_Range loop
         Seen (suit) := False;
      end loop;

      while Acceptables.Size > 0 loop
         C := Acceptables.Get;
         AUnit.Assertions.Assert (C.Get_Rank = Definitions.King,
                                  "rank=King" &
                                    " /= " & C.Get_Rank'Image);
         if Seen (C.Get_Suit) then
            AUnit.Assertions.Assert (False,
                                     "suit=" & C.Get_Suit'Image &
                                       " duplicated");
         else
            Seen (C.Get_Suit) := True;
         end if;

      end loop;
   end Empty_Stack_Accepts_All_Kings;

   procedure Bottom_Ace_Does_Not_Accept_AnyCard (T : in out Test) is
      pragma Unreferenced (T);
      stack         : constant Tableau_Stack.Stack_Type
        := Tableau_Stack.Construct (NUMBER_FOR_STACK);
      Card_King     : constant Card.Card_Type
        := Card.Construct (Definitions.King, Definitions.Diamond);
      Card_Ace      : constant Card.Card_Type
        := Card.Construct (Definitions.Ace, Definitions.Diamond);
      Acceptables   : Tableau_Stack.Acceptable_Type;
   begin
      stack.Push_Checked (Card_King);
      stack.Push_Unchecked (Card_Ace);
      AUnit.Assertions.Assert (stack.Size = 2,
                               "size=2" &
                                 " /= " & stack.Size'Image);
      Acceptables := stack.Accepts;
      AUnit.Assertions.Assert (Acceptables.Size = 0,
                               "size=0" &
                                 " /= " & Acceptables.Size'Image);
   end Bottom_Ace_Does_Not_Accept_AnyCard;

   procedure Bottom_Ten_Accepts_Two_Nines (T : in out Test) is
      pragma Unreferenced (T);
      stack           : constant Tableau_Stack.Stack_Type
        := Tableau_Stack.Construct (NUMBER_FOR_STACK);
      Card_King       : constant Card.Card_Type
        := Card.Construct (Definitions.King, Definitions.Diamond);
      Card_Ten        : constant Card.Card_Type
        := Card.Construct (Definitions.Ten, Definitions.Diamond);
      Card_Nine_Club  : constant Card.Card_Type
        := Card.Construct (Definitions.Nine, Definitions.Club);
      Card_Nine_Spade : constant Card.Card_Type
        := Card.Construct (Definitions.Nine, Definitions.Spade);
      Acceptables     : Tableau_Stack.Acceptable_Type;

   begin
      stack.Push_Checked (Card_King);
      stack.Push_Unchecked (Card_Ten);
      AUnit.Assertions.Assert (stack.Size = 2,
                               "size=2" &
                                 " /= " & stack.Size'Image);
      Acceptables := stack.Accepts;
      AUnit.Assertions.Assert (Acceptables.Size = 2,
                               "size=2" &
                                 " /= " & Acceptables.Size'Image);
      AUnit.Assertions.Assert (Acceptables.Has (Card_Nine_Club),
                               "does not have:" & Card_Nine_Club.Image);
      AUnit.Assertions.Assert (Acceptables.Has (Card_Nine_Spade),
                               "does not have:" & Card_Nine_Spade.Image);
   end Bottom_Ten_Accepts_Two_Nines;

   --------------------------------------------------------------------
   --  Collect tests
   procedure Collect_Empty_Stack (T : in out Test) is
      pragma Unreferenced (T);
      stack : constant Tableau_Stack.Stack_Type
        := Tableau_Stack.Construct (NUMBER_FOR_STACK);

      SIF           : Short_Image_FIFO.Short_Image_FIFO_Type;

      Expected_Size : constant Natural := 1;
      Actual_Size   : Natural;

      Expected_SI   : constant Card.Short_Image_Type := Card.Empty_Short_Image;
      Actual_SI     : Card.Short_Image_Type;
   begin
      SIF := stack.Short_Images;
      Actual_Size := SIF.Size;
      AUnit.Assertions.Assert (Expected_Size = Actual_Size,
                               "Expected_Size=" & Expected_Size'Image &
                                 "/= Actual_Size=" & Actual_Size'Image);
      Actual_SI := SIF.Get;
      AUnit.Assertions.Assert (Expected_SI = Actual_SI,
                               "Expected_SI=" & Expected_SI &
                                 "/= Actual_SI=" & Actual_SI);
   end Collect_Empty_Stack;

   procedure Collect_1_Card (T : in out Test) is
      pragma Unreferenced (T);
      stack : constant Tableau_Stack.Stack_Type
        := Tableau_Stack.Construct (NUMBER_FOR_STACK);

      SIF           : Short_Image_FIFO.Short_Image_FIFO_Type;

      Expected_Size : constant Natural := 1;
      Actual_Size   : Natural;

      C1            : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      Expected_SI1  : constant Card.Short_Image_Type := C1.Short_Image;
      Actual_SI1    : Card.Short_Image_Type;
   begin
      stack.Push_Unchecked (C1);
      SIF := stack.Short_Images;
      Actual_Size := SIF.Size;
      AUnit.Assertions.Assert (Expected_Size = Actual_Size,
                               "Expected_Size=" & Expected_Size'Image &
                                 "/= Actual_Size=" & Actual_Size'Image);
      Actual_SI1 := SIF.Get;
      AUnit.Assertions.Assert (Expected_SI1 = Actual_SI1,
                               "Expected_SI1=" & Expected_SI1 &
                                 "/= Actual_SI1=" & Actual_SI1);
   end Collect_1_Card;

   procedure Collect_2_Cards (T : in out Test) is
      pragma Unreferenced (T);
      stack : constant Tableau_Stack.Stack_Type
        := Tableau_Stack.Construct (NUMBER_FOR_STACK);

      SIF           : Short_Image_FIFO.Short_Image_FIFO_Type;

      Expected_Size : constant Natural := 2;
      Actual_Size   : Natural;

      C1            : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      Expected_SI1  : constant Card.Short_Image_Type := C1.Short_Image;
      Actual_SI1    : Card.Short_Image_Type;

      C2            : constant Card.Card_Type
        := Card.Construct (Definitions.King,
                           Definitions.Spade);
      Expected_SI2  : constant Card.Short_Image_Type := C2.Short_Image;
      Actual_SI2    : Card.Short_Image_Type;
   begin
      stack.Push_Unchecked (C1);
      stack.Push_Unchecked (C2);
      SIF := stack.Short_Images;
      Actual_Size := SIF.Size;
      AUnit.Assertions.Assert (Expected_Size = Actual_Size,
                               "Expected_Size=" & Expected_Size'Image &
                                 "/= Actual_Size=" & Actual_Size'Image);
      Actual_SI1 := SIF.Get;
      AUnit.Assertions.Assert (Expected_SI1 = Actual_SI1,
                               "Expected_SI1=" & Expected_SI1 &
                                 "/= Actual_SI1=" & Actual_SI1);
      Actual_SI2 := SIF.Get;
      AUnit.Assertions.Assert (Expected_SI2 = Actual_SI2,
                               "Expected_SI2=" & Expected_SI2 &
                                 " /= Actual_SI2=" & Actual_SI2);
   end Collect_2_Cards;

   --------------------------------------------------------------------
   --  Has_King_As_Bottom_Card
   --------------------------------------------------------------------
   --
   procedure HasKingAsBottomCard_EmptyStack (T : in out Test) is
      pragma Unreferenced (T);
      Stack    : constant Tableau_Stack.Stack_Type
        := Tableau_Stack.Construct (NUMBER_FOR_STACK);
      Expected : constant Boolean := False;
      Actual   : constant Boolean := Stack.Has_King_As_Bottom_Card;
   begin
      AUnit.Assertions.Assert (Expected = Actual,
                               "Expected=" & Expected'Image &
                                 "/= Actual=" & Actual'Image);
   end HasKingAsBottomCard_EmptyStack;

   --------------------------------------------------------------------
   --
   procedure HasKingAsBottomCard_NoKing_At_All (T : in out Test) is
      pragma Unreferenced (T);
      stack    : constant Tableau_Stack.Stack_Type
        := Tableau_Stack.Construct (NUMBER_FOR_STACK);
      Expected : constant Boolean := False;
      Actual   : Boolean;
   begin
      stack.Push_Unchecked (Card.Construct (Rank => Definitions.Three,
                                            Suit => Definitions.Diamond));
      stack.Push_Unchecked (Card.Construct (Rank => Definitions.Three,
                                            Suit => Definitions.Club));
      stack.Push_Unchecked (Card.Construct (Rank => Definitions.Ten,
                                            Suit => Definitions.Spade));
      Actual := stack.Has_King_As_Bottom_Card;
      AUnit.Assertions.Assert (Expected = Actual,
                               "Expected=" & Expected'Image &
                                 "/= Actual=" & Actual'Image);
   end HasKingAsBottomCard_NoKing_At_All;

   --------------------------------------------------------------------
   --
   procedure HasKingAsBottomCard_1KingNotAtBottom (T : in out Test) is
      pragma Unreferenced (T);
      stack    : constant Tableau_Stack.Stack_Type
        := Tableau_Stack.Construct (NUMBER_FOR_STACK);
      Expected : constant Boolean := False;
      Actual   : Boolean;
   begin
      stack.Push_Unchecked (Card.Construct (Rank => Definitions.Three,
                                            Suit => Definitions.Diamond));
      stack.Push_Unchecked (Card.Construct (Rank => Definitions.King,
                                            Suit => Definitions.Club));
      stack.Push_Unchecked (Card.Construct (Rank => Definitions.Ten,
                                            Suit => Definitions.Spade));
      Actual := stack.Has_King_As_Bottom_Card;
      AUnit.Assertions.Assert (Expected = Actual,
                               "Expected=" & Expected'Image &
                                 "/= Actual=" & Actual'Image);
   end HasKingAsBottomCard_1KingNotAtBottom;

   --------------------------------------------------------------------
   --
   procedure HasKingAsBottomCard_KingAtBottomOnlyCard (T : in out Test) is
      pragma Unreferenced (T);
      stack    : constant Tableau_Stack.Stack_Type
        := Tableau_Stack.Construct (NUMBER_FOR_STACK);
      Expected : constant Boolean := True;
      Actual   : Boolean;
   begin
      stack.Push_Unchecked (Card.Construct (Rank => Definitions.King,
                                            Suit => Definitions.Club));
      Actual := stack.Has_King_As_Bottom_Card;
      AUnit.Assertions.Assert (Expected = Actual,
                               "Expected=" & Expected'Image &
                                 "/= Actual=" & Actual'Image);
   end HasKingAsBottomCard_KingAtBottomOnlyCard;

   procedure HasKingAsBottomCard_KingAtBottomMultipleCardsNoOtherKings
     (T : in out Test) is
      pragma Unreferenced (T);
      stack    : constant Tableau_Stack.Stack_Type
        := Tableau_Stack.Construct (NUMBER_FOR_STACK);
      Expected : constant Boolean := True;
      Actual   : Boolean;
   begin
      stack.Push_Unchecked (Card.Construct (Rank => Definitions.King,
                                            Suit => Definitions.Club));
      stack.Push_Unchecked (Card.Construct (Rank => Definitions.Three,
                                            Suit => Definitions.Diamond));
      stack.Push_Unchecked (Card.Construct (Rank => Definitions.Ten,
                                            Suit => Definitions.Spade));
      Actual := stack.Has_King_As_Bottom_Card;
      AUnit.Assertions.Assert (Expected = Actual,
                               "Expected=" & Expected'Image &
                                 "/= Actual=" & Actual'Image);
   end HasKingAsBottomCard_KingAtBottomMultipleCardsNoOtherKings;

   procedure HasKingAsBottomCard_KingAtBottomMultipleCardsW_OtherKings
     (T : in out Test) is
      pragma Unreferenced (T);
      stack    : constant Tableau_Stack.Stack_Type
        := Tableau_Stack.Construct (NUMBER_FOR_STACK);
      Expected : constant Boolean := True;
      Actual   : Boolean;
   begin
      stack.Push_Unchecked (Card.Construct (Rank => Definitions.King,
                                            Suit => Definitions.Club));
      stack.Push_Unchecked (Card.Construct (Rank => Definitions.Three,
                                            Suit => Definitions.Diamond));
      stack.Push_Unchecked (Card.Construct (Rank => Definitions.King,
                                            Suit => Definitions.Spade));
      stack.Push_Unchecked (Card.Construct (Rank => Definitions.King,
                                            Suit => Definitions.Heart));
      stack.Push_Unchecked (Card.Construct (Rank => Definitions.Four,
                                            Suit => Definitions.Diamond));
      Actual := stack.Has_King_As_Bottom_Card;
      AUnit.Assertions.Assert (Expected = Actual,
                               "Expected=" & Expected'Image &
                                 "/= Actual=" & Actual'Image);
   end HasKingAsBottomCard_KingAtBottomMultipleCardsW_OtherKings;

   --------------------------------------------------------------------
   --  the test suit construction
   package Caller is new AUnit.Test_Caller (Tableau_Stack.Test.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
      N   : constant String := "Tableau_Stack.";
   begin
      --  ctor tests
      Ret.Add_Test (Caller.
                      Create (N & "Construct",
                        Construct'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Construct_Check_Size",
                        Construct_Check_Size'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Construct_Check_Is_Empty",
                        Construct_Check_Is_Empty'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Construct_Check_Number",
                        Construct_Check_Number'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Construct_Check_Peek_Fails",
                        Construct_Check_Peek_Fails'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Construct_Check_Pop_Fails",
                        Construct_Check_Pop_Fails'Access));

      --  Has tests
      Ret.Add_Test (Caller.
                      Create (N & "Does_Not_Have_Empty_Stack",
                        Does_Not_Have_Empty_Stack'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Does_Not_Have_Not_Empty_Stack",
                        Does_Not_Have_Not_Empty_Stack'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Does_Have_Not_Empty_Stack",
                        Does_Have_Not_Empty_Stack'Access));

      --  Push tests
      Ret.Add_Test (Caller.
                      Create (N & "Empty_Stack_Push_King_Check_Size",
                        Empty_Stack_Push_King_Check_Size'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Empty_Stack_Push_King_Check_Peek",
                        Empty_Stack_Push_King_Check_Peek'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Empty_Stack_Push_King_Check_Pop",
                        Empty_Stack_Push_King_Check_Pop'Access));

      Ret.Add_Test (Caller.
                      Create (N & "Empty_Stack_Push_Other_Than_King",
                        Empty_Stack_Push_Other_Than_King'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Push_Multiple_Cards_Check_Size",
                        Push_Multiple_Cards_Check_Size'Access));

      --  Accepts tests
      Ret.Add_Test (Caller.
                      Create (N & "Empty_Stack_Accepts_All_Kings",
                        Empty_Stack_Accepts_All_Kings'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Bottom_Ace_Does_Not_Accept_AnyCard",
                        Bottom_Ace_Does_Not_Accept_AnyCard'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Bottom_Ten_Accepts_Two_Nines",
                        Bottom_Ten_Accepts_Two_Nines'Access));

      --  Collect tests
      Ret.Add_Test (Caller.
                      Create (N & "Collect_Empty_Stack",
                        Collect_Empty_Stack'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Collect_1_Card",
                        Collect_1_Card'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Collect_2_Cards",
                        Collect_2_Cards'Access));

      --  HasKingAsBottomCard tests
      Ret.Add_Test (Caller.
                      Create (N & "HasKingAsBottomCard_EmptyStack",
                        HasKingAsBottomCard_EmptyStack'Access));
      Ret.Add_Test (Caller.
                      Create (N & "HasKingAsBottomCard_NoKing_At_All",
                        HasKingAsBottomCard_NoKing_At_All'Access));
      Ret.Add_Test (Caller.
                      Create (N & "HasKingAsBottomCard_1KingNotAtBottom",
                        HasKingAsBottomCard_1KingNotAtBottom'Access));
      Ret.Add_Test (Caller.
                      Create (N & "HasKingAsBottomCard_KingAtBottomOnlyCard",
                        HasKingAsBottomCard_KingAtBottomOnlyCard'Access));
      Ret.
        Add_Test (Caller.
                    Create (
                      N &
                        "HasKingAsBottomCard" &
                        "_KingAtBottomMultipleCardsNoOtherKings",
                      HasKingAsBottomCard_KingAtBottomMultipleCardsNoOtherKings
                      'Access));
      Ret.
        Add_Test (Caller.
                    Create (
                      N &
                        "HasKingAsBottomCard" &
                        "_KingAtBottomMultipleCardsWithOtherKings",
                      HasKingAsBottomCard_KingAtBottomMultipleCardsW_OtherKings
                      'Access));
      --
      return Ret;
   end Suite;

end Tableau_Stack.Test;
