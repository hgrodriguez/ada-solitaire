with AUnit.Assertions;
with AUnit.Test_Caller;

with Card;
with Deck;
with Pile_Of_Cards.FIFO;

package body Tableau.Test is

   procedure Construct (T : in out Test) is
      pragma Unreferenced (T);
      Tab : Tableau.Tableau_Type := Tableau.Construct;
      pragma Warnings (Off, Tab);
   begin
      null;
   end Construct;

   procedure Construct_Check_Size (T : in out Test) is
      pragma Unreferenced (T);
      Tab : constant Tableau.Tableau_Type := Tableau.Construct;
   begin
      AUnit.Assertions.Assert (Tab.Size = 0,
                               "t.Size=" & Tab.Size'Image &
                                 " /= 0");
   end Construct_Check_Size;

   procedure Add_1_Card_Check_Size (T : in out Test) is
      pragma Unreferenced (T);
      Tab    : constant Tableau.Tableau_Type := Tableau.Construct;
      Pile   : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Pile_Of_Cards.FIFO.Construct;
      C_Push : constant Card.Card_Type := Card.Construct (Deck.Six,
                                                          Deck.Diamond);
   begin
      Pile.Put (C_Push);
      Tab.Push (Pile);
      AUnit.Assertions.Assert (Tab.Size = 1,
                               "t.Size=" & Tab.Size'Image &
                                 " /= 1");
   end Add_1_Card_Check_Size;

   procedure Add_1_Card_Check_Stack (T : in out Test) is
      pragma Unreferenced (T);
      Tab            : constant Tableau.Tableau_Type := Tableau.Construct;
      Pile           : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Pile_Of_Cards.FIFO.Construct;
      C_Push         : constant Card.Card_Type := Card.Construct (Deck.Six,
                                                          Deck.Diamond);
      Stack          : Tableau_Stack.Stack_Type_Access;
      Expected_Sizes : constant array (Valid_Stacks_Range) of Natural
        := (1, 0, 0, 0, 0, 0, 0);
      Stack_Size     : Natural;
   begin
      Pile.Put (C_Push);
      Tab.Push (Pile);
      for J in Valid_Stacks_Range loop
         Stack := Tab.Get_Stack (J);
         Stack_Size := Stack.all.Size;
         AUnit.Assertions.Assert (Stack_Size = Expected_Sizes (J),
                                  "Size(" & J'Image & ")=" &
                                    Expected_Sizes (J)'Image &
                                    " /= " & Stack_Size'Image);
      end loop;
   end Add_1_Card_Check_Stack;

   procedure Add_3_Cards_Check_Size (T : in out Test) is
      pragma Unreferenced (T);
      Tab     : constant Tableau.Tableau_Type := Tableau.Construct;
      Pile    : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Pile_Of_Cards.FIFO.Construct;
      C_Push1 : constant Card.Card_Type := Card.Construct (Deck.Six,
                                                           Deck.Diamond);
      C_Push2 : constant Card.Card_Type := Card.Construct (Deck.Seven,
                                                           Deck.Club);
      C_Push3 : constant Card.Card_Type := Card.Construct (Deck.King,
                                                           Deck.Heart);
   begin
      Pile.Put (C_Push1);
      Pile.Put (C_Push2);
      Pile.Put (C_Push3);
      Tab.Push (Pile);
      AUnit.Assertions.Assert (Tab.Size = 3,
                               "t.Size=" & Tab.Size'Image &
                                 " /= 3");
   end Add_3_Cards_Check_Size;

   procedure Add_3_Cards_Check_Stack (T : in out Test) is
      pragma Unreferenced (T);
      Tab            : constant Tableau.Tableau_Type := Tableau.Construct;
      Pile           : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Pile_Of_Cards.FIFO.Construct;
      C_Push1        : constant Card.Card_Type := Card.Construct (Deck.Six,
                                                                  Deck.Diamond);
      C_Push2        : constant Card.Card_Type := Card.Construct (Deck.Seven,
                                                                  Deck.Club);
      C_Push3        : constant Card.Card_Type := Card.Construct (Deck.King,
                                                                  Deck.Heart);
      Stack          : Tableau_Stack.Stack_Type_Access;
      Expected_Sizes : constant array (Valid_Stacks_Range) of Natural
        := (1, 1, 1, 0, 0, 0, 0);
      Stack_Size     : Natural;
   begin
      Pile.Put (C_Push1);
      Pile.Put (C_Push2);
      Pile.Put (C_Push3);
      Tab.Push (Pile);
      for J in Valid_Stacks_Range loop
         Stack := Tab.Get_Stack (J);
         Stack_Size := Stack.all.Size;
         AUnit.Assertions.Assert (Stack_Size = Expected_Sizes (J),
                                  "Size(" & J'Image & ")=" &
                                    Expected_Sizes (J)'Image &
                                    " /= " & Stack_Size'Image);
      end loop;
   end Add_3_Cards_Check_Stack;

   procedure Add_7_Cards_Check_Size (T : in out Test) is
      pragma Unreferenced (T);
      Tab   : constant Tableau.Tableau_Type := Tableau.Construct;
      Pile  : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Pile_Of_Cards.FIFO.Construct;
      Cards : constant array (1 .. 7) of Card.Card_Type
        := (Card.Construct (Deck.Three, Deck.Diamond),
            Card.Construct (Deck.Seven, Deck.Heart),
            Card.Construct (Deck.Ace,   Deck.Spade),
            Card.Construct (Deck.King,  Deck.Club),
            Card.Construct (Deck.Eight, Deck.Diamond),
            Card.Construct (Deck.Three, Deck.Spade),
            Card.Construct (Deck.Jack,  Deck.Heart));
   begin
      for J in Integer range 1 .. 7 loop
         Pile.Put (Cards (J));
      end loop;
      Tab.Push (Pile);
      AUnit.Assertions.Assert (Tab.Size = 7,
                               "t.Size=" & Tab.Size'Image &
                                 " /= 7");
   end Add_7_Cards_Check_Size;

   procedure Add_7_Cards_Check_Stack (T : in out Test) is
      pragma Unreferenced (T);
      Tab            : constant Tableau.Tableau_Type := Tableau.Construct;
      Pile           : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Pile_Of_Cards.FIFO.Construct;
      Cards          : constant array (1 .. 7) of Card.Card_Type
        := (Card.Construct (Deck.Three, Deck.Diamond),
            Card.Construct (Deck.Seven, Deck.Heart),
            Card.Construct (Deck.Ace,   Deck.Spade),
            Card.Construct (Deck.King,  Deck.Club),
            Card.Construct (Deck.Eight, Deck.Diamond),
            Card.Construct (Deck.Three, Deck.Spade),
            Card.Construct (Deck.Jack,  Deck.Heart));
      Stack          : Tableau_Stack.Stack_Type_Access;
      Expected_Sizes : constant array (Valid_Stacks_Range) of Natural
        := (1, 1, 1, 1, 1, 1, 1);
      Stack_Size     : Natural;
   begin
      for J in Integer range 1 .. 7 loop
         Pile.Put (Cards (J));
      end loop;
      Tab.Push (Pile);
      for J in Valid_Stacks_Range loop
         Stack := Tab.Get_Stack (J);
         Stack_Size := Stack.all.Size;
         AUnit.Assertions.Assert (Stack_Size = Expected_Sizes (J),
                                  "Size(" & J'Image & ")=" &
                                    Expected_Sizes (J)'Image &
                                    " /= " & Stack_Size'Image);
      end loop;
   end Add_7_Cards_Check_Stack;

   --------------------------------------------------------------------
   --  the test suit construction
   package Caller is new AUnit.Test_Caller (Tableau.Test.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
      N   : constant String := "Tableau.Test.";
   begin
      --  ctor tests
      Ret.Add_Test (Caller.
                      Create (N & "Construct",
                        Construct'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Construct_Check_Size",
                        Construct_Check_Size'Access));

      Ret.Add_Test (Caller.
                      Create (N & "Add_1_Card_Check_Size",
                        Add_1_Card_Check_Size'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Add_1_Card_Check_Stack",
                        Add_1_Card_Check_Stack'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Add_3_Cards_Check_Size",
                        Add_3_Cards_Check_Size'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Add_3_Cards_Check_Stack",
                        Add_3_Cards_Check_Stack'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Add_7_Cards_Check_Size",
                        Add_7_Cards_Check_Size'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Add_7_Cards_Check_Stack",
                        Add_7_Cards_Check_Stack'Access));

      return Ret;
   end Suite;

end Tableau.Test;
