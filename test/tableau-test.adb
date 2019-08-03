with Ada.Exceptions;

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

   procedure Construct_Check_Pop_Fails_Exception;
   procedure Construct_Check_Pop_Fails_Exception is
      Tab : constant Tableau.Tableau_Type := Tableau.Construct;
      C   : Card.Card_Type;
      pragma Warnings (Off, C);
   begin
      C := Tab.Pop_From_Stack (Tableau.Valid_Stacks_Range'First);
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

   type Some_Cards is array (Positive range <>) of Card.Card_Type;

   procedure Add_Cards_And_Check_Size (Cards : Some_Cards);
   procedure Add_Cards_And_Check_Size (Cards : Some_Cards) is
      Expected_Size : constant Natural := Cards'Last;
      Tab           : constant Tableau.Tableau_Type := Tableau.Construct;
      Pile          : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Pile_Of_Cards.FIFO.Construct;
   begin
      for J in Natural range 1 .. Expected_Size loop
         Pile.Put (Cards (J));
      end loop;
      --
      Tab.Push (Pile);
      AUnit.Assertions.Assert (Tab.Size = Expected_Size,
                               "t.Size=" & Tab.Size'Image &
                                 " /= " & Expected_Size'Image);
   end Add_Cards_And_Check_Size;

   procedure Add_1_Card_Check_Size (T : in out Test) is
      pragma Unreferenced (T);
      Cards         : constant Some_Cards
        := (1 => Card.Construct (Deck.Three, Deck.Diamond));
   begin
      Add_Cards_And_Check_Size (Cards);
   end Add_1_Card_Check_Size;

   procedure Add_3_Cards_Check_Size (T : in out Test) is
      pragma Unreferenced (T);
      Cards         : constant Some_Cards
        := (Card.Construct (Deck.Three, Deck.Diamond),
            Card.Construct (Deck.Three, Deck.Spade),
            Card.Construct (Deck.Jack,  Deck.Heart));
   begin
      Add_Cards_And_Check_Size (Cards);
   end Add_3_Cards_Check_Size;

   procedure Add_7_Cards_Check_Size (T : in out Test) is
      pragma Unreferenced (T);
      Cards : constant Some_Cards
        := (Card.Construct (Deck.Three, Deck.Diamond),
            Card.Construct (Deck.Seven, Deck.Heart),
            Card.Construct (Deck.Ace,   Deck.Spade),
            Card.Construct (Deck.King,  Deck.Club),
            Card.Construct (Deck.Eight, Deck.Diamond),
            Card.Construct (Deck.Three, Deck.Spade),
            Card.Construct (Deck.Jack,  Deck.Heart));
   begin
      Add_Cards_And_Check_Size (Cards);
   end Add_7_Cards_Check_Size;

   procedure Add_Cards_And_Check_Stack (Cards : Some_Cards);
   procedure Add_Cards_And_Check_Stack (Cards : Some_Cards) is
      Tab            : constant Tableau.Tableau_Type := Tableau.Construct;
      Pile           : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Pile_Of_Cards.FIFO.Construct;
      Stack          : Tableau_Stack.Stack_Type_Access;
      Expected_Sizes : array (Valid_Stacks_Range) of Natural;
      Stack_Size     : Natural;
      I              : Integer;
   begin
      for J in Valid_Stacks_Range loop
         I := Integer (J);
         if I > Cards'Last then
            Expected_Sizes (J) := 0;
         else
            Expected_Sizes (J) := 1;
         end if;
      end loop;
      for J in Natural range Cards'First .. Cards'Last loop
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
   end Add_Cards_And_Check_Stack;

   procedure Add_1_Card_Check_Stack (T : in out Test) is
      pragma Unreferenced (T);
      Cards          : constant Some_Cards
        := (1 => Card.Construct (Deck.Three, Deck.Diamond));
   begin
      Add_Cards_And_Check_Stack (Cards);
   end Add_1_Card_Check_Stack;

   procedure Add_3_Cards_Check_Stack (T : in out Test) is
      pragma Unreferenced (T);
      Cards          : constant Some_Cards
        := (Card.Construct (Deck.Three, Deck.Diamond),
            Card.Construct (Deck.Three, Deck.Spade),
            Card.Construct (Deck.Jack,  Deck.Heart));
   begin
      Add_Cards_And_Check_Stack (Cards);
   end Add_3_Cards_Check_Stack;

   procedure Add_7_Cards_Check_Stack (T : in out Test) is
      pragma Unreferenced (T);
      Cards          : constant Some_Cards
        := (Card.Construct (Deck.Three, Deck.Diamond),
            Card.Construct (Deck.Seven, Deck.Heart),
            Card.Construct (Deck.Ace,   Deck.Spade),
            Card.Construct (Deck.King,  Deck.Club),
            Card.Construct (Deck.Eight, Deck.Diamond),
            Card.Construct (Deck.Three, Deck.Spade),
            Card.Construct (Deck.Jack,  Deck.Heart));
   begin
      Add_Cards_And_Check_Stack (Cards);
   end Add_7_Cards_Check_Stack;

   procedure Add_Cards_And_Check_Pop (Cards : Some_Cards);
   procedure Add_Cards_And_Check_Pop (Cards : Some_Cards) is
      Tab            : constant Tableau.Tableau_Type := Tableau.Construct;
      Pile           : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Pile_Of_Cards.FIFO.Construct;
      Expected_Sizes : array (Valid_Stacks_Range) of Natural;
      I              : Integer;
      C_A            : Card.Card_Type;
      C_E            : Card.Card_Type;
   begin
      for J in Valid_Stacks_Range loop
         I := Integer (J);
         if I > Cards'Last then
            Expected_Sizes (J) := 0;
         else
            Expected_Sizes (J) := 1;
         end if;
      end loop;
      for J in Natural range Cards'First .. Cards'Last loop
         Pile.Put (Cards (J));
      end loop;
      Tab.Push (Pile);
      for J in Valid_Stacks_Range loop
         if Expected_Sizes (J) > 0 then
            C_A := Tab.Pop_From_Stack (J);
            I := Integer (J);
            C_E := Cards (I);
            AUnit.Assertions.Assert (C_A.Is_Equal_To (C_E),
                                     "Card (" & J'Image & ")=" &
                                       C_E.Image &
                                       " /= " & C_A.Image);
         end if;
      end loop;
   end Add_Cards_And_Check_Pop;

   procedure Add_1_Card_Check_Pop (T : in out Test) is
      pragma Unreferenced (T);
      Cards          : constant Some_Cards
        := (1 => Card.Construct (Deck.Three, Deck.Diamond));
   begin
      Add_Cards_And_Check_Pop (Cards);
   end Add_1_Card_Check_Pop;

   procedure Add_3_Cards_Check_Pop (T : in out Test) is
      pragma Unreferenced (T);
      Cards          : constant Some_Cards
        := (Card.Construct (Deck.Three, Deck.Diamond),
            Card.Construct (Deck.Three, Deck.Spade),
            Card.Construct (Deck.Jack,  Deck.Heart));
   begin
      Add_Cards_And_Check_Pop (Cards);
   end Add_3_Cards_Check_Pop;

   procedure Add_7_Cards_Check_Pop (T : in out Test) is
      pragma Unreferenced (T);
      Cards          : constant Some_Cards
        := (Card.Construct (Deck.Three, Deck.Diamond),
            Card.Construct (Deck.Seven, Deck.Heart),
            Card.Construct (Deck.Ace,   Deck.Spade),
            Card.Construct (Deck.King,  Deck.Club),
            Card.Construct (Deck.Eight, Deck.Diamond),
            Card.Construct (Deck.Three, Deck.Spade),
            Card.Construct (Deck.Jack,  Deck.Heart));
   begin
      Add_Cards_And_Check_Pop (Cards);
   end Add_7_Cards_Check_Pop;

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
                      Create (N & "Construct_Check_Pop_Fails",
                        Construct_Check_Pop_Fails'Access));

      Ret.Add_Test (Caller.
                      Create (N & "Add_1_Card_Check_Size",
                        Add_1_Card_Check_Size'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Add_3_Cards_Check_Size",
                        Add_3_Cards_Check_Size'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Add_7_Cards_Check_Size",
                        Add_7_Cards_Check_Size'Access));

      Ret.Add_Test (Caller.
                      Create (N & "Add_1_Card_Check_Stack",
                        Add_1_Card_Check_Stack'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Add_3_Cards_Check_Stack",
                        Add_3_Cards_Check_Stack'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Add_7_Cards_Check_Stack",
                        Add_7_Cards_Check_Stack'Access));

      Ret.Add_Test (Caller.
                      Create (N & "Add_1_Card_Check_Pop",
                        Add_1_Card_Check_Pop'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Add_3_Cards_Check_Pop",
                        Add_3_Cards_Check_Pop'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Add_7_Cards_Check_Pop",
                        Add_7_Cards_Check_Pop'Access));

      return Ret;
   end Suite;

end Tableau.Test;
