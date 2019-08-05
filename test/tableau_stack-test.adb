with Ada.Exceptions;

with AUnit.Assertions;
with AUnit.Test_Caller;

with Card;
with Deck; use Deck;

package body Tableau_Stack.Test is

   --------------------------------------------------------------------
   --  constructors
   procedure Construct (T : in out Test) is
      pragma Unreferenced (T);
      stack : Tableau_Stack.Stack_Type := Tableau_Stack.Construct;
      pragma Warnings (Off, stack);
   begin
      null;
   end Construct;

   procedure Construct_Check_Size (T : in out Test) is
      pragma Unreferenced (T);
      stack : constant Tableau_Stack.Stack_Type := Tableau_Stack.Construct;
   begin
      AUnit.Assertions.Assert (stack.Size = 0,
                               "size=0" &
                                 " /= " & stack.Size'Image);
   end Construct_Check_Size;

   procedure Construct_Check_Is_Empty (T : in out Test) is
      pragma Unreferenced (T);
      stack : constant Tableau_Stack.Stack_Type := Tableau_Stack.Construct;
   begin
      AUnit.Assertions.Assert (stack.Is_Empty, "not stack.Is_Empty");
   end Construct_Check_Is_Empty;

   procedure Construct_Check_Peek_Fails_Exception;
   procedure Construct_Check_Peek_Fails_Exception is
      Stack : constant Tableau_Stack.Stack_Type := Tableau_Stack.Construct;
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
      Stack : constant Tableau_Stack.Stack_Type := Tableau_Stack.Construct;
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
      stack : constant Tableau_Stack.Stack_Type := Tableau_Stack.Construct;
      C_N   : constant Card.Card_Type := Card.Construct (Deck.Two,
                                                         Deck.Diamond);
   begin
      AUnit.Assertions.Assert (not stack.Has (C_N), "Has=False /= True");
   end Does_Not_Have_Empty_Stack;

   procedure Does_Not_Have_Not_Empty_Stack (T : in out Test) is
      pragma Unreferenced (T);
      stack : constant Tableau_Stack.Stack_Type := Tableau_Stack.Construct;
      C_N   : constant Card.Card_Type := Card.Construct (Deck.Two,
                                                         Deck.Diamond);
   begin
      stack.Push_Unchecked (Card.Construct (Deck.Ten, Deck.Heart));
      AUnit.Assertions.Assert (not stack.Has (C_N), "Has=False /= True");
   end Does_Not_Have_Not_Empty_Stack;

   procedure Does_Have_Not_Empty_Stack (T : in out Test) is
      pragma Unreferenced (T);
      stack : constant Tableau_Stack.Stack_Type := Tableau_Stack.Construct;
      C_N   : constant Card.Card_Type := Card.Construct (Deck.Two,
                                                         Deck.Diamond);
   begin
      stack.Push_Unchecked (C_N);
      AUnit.Assertions.Assert (stack.Has (C_N), "Has=True /= False");
   end Does_Have_Not_Empty_Stack;

   --------------------------------------------------------------------
   --  Push
   procedure Empty_Stack_Push_King_Check_Size (T : in out Test) is
      pragma Unreferenced (T);
      stack : constant Tableau_Stack.Stack_Type := Tableau_Stack.Construct;
   begin
      stack.Push_Checked (Card.Construct (Deck.King, Deck.Diamond));
      AUnit.Assertions.Assert (stack.Size = 1,
                               "size=1" &
                                 " /= " & stack.Size'Image);
   end Empty_Stack_Push_King_Check_Size;

   procedure Empty_Stack_Push_King_Check_Peek (T : in out Test) is
      pragma Unreferenced (T);
      stack : constant Tableau_Stack.Stack_Type := Tableau_Stack.Construct;
      C_E   : constant Card.Card_Type := Card.Construct (Rank => Deck.King,
                                                         Suit => Deck.Diamond);
      C     : Card.Card_Type;
   begin
      stack.Push_Checked (C_E);
      C := stack.Peek;
      AUnit.Assertions.Assert (C.Is_Equal_To (C_E),
                               "C_E=" & C_E.Image &
                                 " /= " & C.Image);
   end Empty_Stack_Push_King_Check_Peek;

   procedure Empty_Stack_Push_King_Check_Pop (T : in out Test) is
      pragma Unreferenced (T);
      stack : constant Tableau_Stack.Stack_Type := Tableau_Stack.Construct;
      C_E   : constant Card.Card_Type := Card.Construct (Rank => Deck.King,
                                                         Suit => Deck.Diamond);
      C     : Card.Card_Type;
   begin
      stack.Push_Checked (C_E);
      C := stack.Pop;
      AUnit.Assertions.Assert (C.Is_Equal_To (C_E),
                               "C_E=" & C_E.Image &
                                 " /= " & C.Image);
   end Empty_Stack_Push_King_Check_Pop;

   procedure Empty_Stack_Push_Other_Than_King_Exception;
   procedure Empty_Stack_Push_Other_Than_King_Exception is
      Stack : constant Tableau_Stack.Stack_Type := Tableau_Stack.Construct;
      C     : constant Card.Card_Type
        := Card.Construct (Deck.Ace, Deck.Diamond);
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
      stack       : constant Tableau_Stack.Stack_Type
        := Tableau_Stack.Construct;
   begin
      stack.Push_Unchecked (Card.Construct (Rank => Deck.Three,
                                            Suit => Deck.Diamond));
      stack.Push_Unchecked (Card.Construct (Rank => Deck.Three,
                                            Suit => Deck.Club));
      stack.Push_Unchecked (Card.Construct (Rank => Deck.Ten,
                                            Suit => Deck.Spade));
      AUnit.Assertions.Assert (stack.Size = 3,
                               "size=1" &
                                 " /= " & stack.Size'Image);
   end Push_Multiple_Cards_Check_Size;

   --------------------------------------------------------------------
   --  Accepts
   procedure Empty_Stack_Accepts_All_Kings (T : in out Test) is
      pragma Unreferenced (T);
      stack       : constant Tableau_Stack.Stack_Type
        := Tableau_Stack.Construct;
      Acceptables : Tableau_Stack.Acceptable_Type := stack.Accepts;
      Seen        : array (Deck.Suit_Type_Valid_Range) of Boolean;
      C           : Card.Card_Type;
   begin
      AUnit.Assertions.Assert (Acceptables.Size = 4,
                               "size=4" &
                                 " /= " & Acceptables.Size'Image);
      for suit in Deck.Suit_Type_Valid_Range loop
         Seen (suit) := False;
      end loop;

      while Acceptables.Size > 0 loop
         C := Acceptables.Get;
         AUnit.Assertions.Assert (C.Get_Rank = Deck.King,
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
        := Tableau_Stack.Construct;
      Card_King     : constant Card.Card_Type
        := Card.Construct (Deck.King, Deck.Diamond);
      Card_Ace      : constant Card.Card_Type
        := Card.Construct (Deck.Ace, Deck.Diamond);
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
        := Tableau_Stack.Construct;
      Card_King       : constant Card.Card_Type
        := Card.Construct (Deck.King, Deck.Diamond);
      Card_Ten        : constant Card.Card_Type
        := Card.Construct (Deck.Ten, Deck.Diamond);
      Card_Nine_Club  : constant Card.Card_Type
        := Card.Construct (Deck.Nine, Deck.Club);
      Card_Nine_Spade : constant Card.Card_Type
        := Card.Construct (Deck.Nine, Deck.Spade);
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

      return Ret;
   end Suite;

end Tableau_Stack.Test;
