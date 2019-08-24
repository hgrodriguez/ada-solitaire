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
   --  Move_To tests
   procedure Move_To_Src_Stack_Equals_Trgt_Stack_Exc;
   procedure Move_To_Src_Stack_Equals_Trgt_Stack_Exc is
      Tab       : constant Tableau.Tableau_Type := Tableau.Construct;
      Src_Stack : constant Tableau_Stack.Stack_Type_Access := Tab.Get_Stack (1);
      Cards     : constant Some_Cards
        := (Card.Construct (Deck.Three, Deck.Diamond),
            Card.Construct (Deck.Seven, Deck.Heart),
            Card.Construct (Deck.Ace,   Deck.Spade),
            Card.Construct (Deck.King,  Deck.Club),
            Card.Construct (Deck.Eight, Deck.Diamond),
            Card.Construct (Deck.Three, Deck.Spade),
            Card.Construct (Deck.Jack,  Deck.Heart));
      Exists : constant Card.Card_Type := Cards (1);
   begin
      for J in Natural range Cards'First .. Cards'Last loop
         Src_Stack.all.Push_Unchecked (Cards (J));
      end loop;
      Tab.Move_To (1, 1, Exists);
   exception
      when Tableau_Target_Stack_Equals_Source_Stack_Exception => raise;
      when Exc : others =>
         AUnit.
           Assertions.
             Assert (False,
                     "Move_To_Src_Stack_Equals_Trgt_Stack_Exc: " &
                       "wrong exception raised:" &
                       Ada.Exceptions.Exception_Name (Exc));
   end Move_To_Src_Stack_Equals_Trgt_Stack_Exc;

   procedure Move_To_Src_Stack_Equals_Trgt_Stack (T : in out Test) is
      pragma Unreferenced (T);
   begin
      AUnit.
        Assertions.
          Assert_Exception (Move_To_Src_Stack_Equals_Trgt_Stack_Exc'Access,
                            "Move_To_Src_Stack_Equals_Trgt_Stack_Exc: " &
                              "no exception raised");
   end Move_To_Src_Stack_Equals_Trgt_Stack;

   procedure Move_To_Source_Selection_Does_Not_Exist_Exc;
   procedure Move_To_Source_Selection_Does_Not_Exist_Exc is
      Tab       : constant Tableau.Tableau_Type := Tableau.Construct;
      Src_Stack : constant Tableau_Stack.Stack_Type_Access := Tab.Get_Stack (1);
      Cards     : constant Some_Cards
        := (Card.Construct (Deck.Three, Deck.Diamond),
            Card.Construct (Deck.Seven, Deck.Heart),
            Card.Construct (Deck.Ace,   Deck.Spade),
            Card.Construct (Deck.King,  Deck.Club),
            Card.Construct (Deck.Eight, Deck.Diamond),
            Card.Construct (Deck.Three, Deck.Spade),
            Card.Construct (Deck.Jack,  Deck.Heart));
      Not_Exist : constant Card.Card_Type
        := Card.Construct (Deck.Queen, Deck.Club);
   begin
      for J in Natural range Cards'First .. Cards'Last loop
         Src_Stack.all.Push_Unchecked (Cards (J));
      end loop;
      Tab.Move_To (1, 2, Not_Exist);
   exception
      when Tableau_Source_Card_Does_Not_Exist_Exception => raise;
      when Exc : others =>
         AUnit.
           Assertions.
             Assert (False,
                     "Move_To_Source_Selection_Does_Not_Exist_Exception: " &
                       "wrong exception raised:" &
                       Ada.Exceptions.Exception_Name (Exc));
   end Move_To_Source_Selection_Does_Not_Exist_Exc;

   procedure Move_To_Source_Selection_Does_Not_Exist (T : in out Test) is
      pragma Unreferenced (T);
   begin
      AUnit.
        Assertions.
          Assert_Exception (Move_To_Source_Selection_Does_Not_Exist_Exc'Access,
                            "Construct_Check_Pop_Fails: " &
                              "no exception raised");
   end Move_To_Source_Selection_Does_Not_Exist;

   procedure Move_To_1_Trgt_Does_Not_Accept_Exc;
   procedure Move_To_1_Trgt_Does_Not_Accept_Exc is
      Tab       : constant Tableau.Tableau_Type := Tableau.Construct;
      --
      Src_Stack : constant Tableau_Stack.Stack_Type_Access := Tab.Get_Stack (1);
      Src_Cards : constant Some_Cards
        := (1 => Card.Construct (Deck.Three, Deck.Diamond));
      Src_Exist : constant Card.Card_Type := Src_Cards (1);
      --
      Dst_Stack : constant Tableau_Stack.Stack_Type_Access := Tab.Get_Stack (2);
      Dst_Cards : constant Some_Cards
        := (1 => Card.Construct (Deck.Ten, Deck.Diamond));
   begin
      for J in Natural range Src_Cards'First .. Src_Cards'Last loop
         Src_Stack.all.Push_Unchecked (Src_Cards (J));
      end loop;
      for J in Natural range Dst_Cards'First .. Dst_Cards'Last loop
         Dst_Stack.all.Push_Unchecked (Dst_Cards (J));
      end loop;
      Tab.Move_To (1, 2, Src_Exist);
   exception
      when Tableau_Destination_Stack_Does_Not_Accept_Exception => raise;
      when Exc : others =>
         AUnit.
           Assertions.
             Assert (False,
                     "Move_To_1_Trgt_Does_Not_Accept_Exc: " &
                       "wrong exception raised:" &
                       Ada.Exceptions.Exception_Name (Exc));
   end Move_To_1_Trgt_Does_Not_Accept_Exc;

   procedure Move_To_1_Trgt_Does_Not_Accept (T : in out Test) is
      pragma Unreferenced (T);
   begin
      AUnit.
        Assertions.
          Assert_Exception (Move_To_1_Trgt_Does_Not_Accept_Exc'Access,
                            "Construct_Check_Pop_Fails: " &
                              "no exception raised");
   end Move_To_1_Trgt_Does_Not_Accept;

   procedure Move_To_1_Src_Not_E_Trgt_E (T : in out Test) is
      pragma Unreferenced (T);
      Tab        : constant Tableau.Tableau_Type := Tableau.Construct;
      --
      Diff       : constant Integer := 1;
      --
      Src_Stack  : constant Tableau_Stack.Stack_Type_Access
        := Tab.Get_Stack (1);
      Src_Cards  : constant Some_Cards
        := (Card.Construct (Deck.Three, Deck.Diamond),
            Card.Construct (Deck.Seven, Deck.Heart),
            Card.Construct (Deck.King,  Deck.Heart));
      Src_Exist  : constant Card.Card_Type := Src_Cards (Src_Cards'Last);
      Src_O_Size : Natural;
      Src_N_Size : Natural;
      --
      Dst_Stack  : constant Tableau_Stack.Stack_Type_Access
        := Tab.Get_Stack (2);
      Dst_O_Size : Natural;
      Dst_N_Size : Natural;
      --
      Aux_Diff   : Integer;
   begin
      for J in Natural range Src_Cards'First .. Src_Cards'Last loop
         Src_Stack.all.Push_Unchecked (Src_Cards (J));
      end loop;
      Src_O_Size := Src_Stack.all.Size;
      Dst_O_Size := Dst_Stack.all.Size;
      --
      Tab.Move_To (1, 2, Src_Exist);
      --
      Src_N_Size := Src_Stack.all.Size;
      Dst_N_Size := Dst_Stack.all.Size;
      --
      Aux_Diff := Src_O_Size - Diff;
      --
      AUnit.Assertions.Assert (Src_N_Size = Src_O_Size - Diff,
                               "Src_N_Size=" & Aux_Diff'Image &
                                 " /= " & Src_N_Size'Image);
      AUnit.Assertions.Assert (not Src_Stack.all.Has (Src_Exist),
                               "Src_Stack still has " & Src_Exist.Image);
      --
      Aux_Diff := Src_O_Size + Diff;
      AUnit.Assertions.Assert (Dst_N_Size = Dst_O_Size + Diff,
                               "Dst_N_Size=" & Aux_Diff'Image &
                                 " /= " & Dst_N_Size'Image);
      AUnit.Assertions.Assert (Dst_Stack.all.Has (Src_Exist),
                               "Dst_Stack has NOT " & Src_Exist.Image);
   end Move_To_1_Src_Not_E_Trgt_E;

   procedure Move_To_1_Src_Not_E_Trgt_Not_E (T : in out Test) is
      pragma Unreferenced (T);
      Tab        : constant Tableau.Tableau_Type := Tableau.Construct;
      --
      Diff       : constant Integer := 1;
      --
      Src_Stack  : constant Tableau_Stack.Stack_Type_Access
        := Tab.Get_Stack (1);
      Src_Cards  : constant Some_Cards
        := (Card.Construct (Deck.Three, Deck.Diamond),
            Card.Construct (Deck.Seven, Deck.Heart),
            Card.Construct (Deck.Ten,   Deck.Heart));
      Src_Exist  : constant Card.Card_Type := Src_Cards (Src_Cards'Last);
      Src_O_Size : Natural;
      Src_N_Size : Natural;
      --
      Dst_Stack  : constant Tableau_Stack.Stack_Type_Access
        := Tab.Get_Stack (2);
      Dst_Cards  : constant Some_Cards
        := (Card.Construct (Deck.Four, Deck.Heart),
            Card.Construct (Deck.Jack, Deck.Club));
      Dst_O_Size : Natural;
      Dst_N_Size : Natural;
      --
      Aux_Diff   : Integer;
   begin
      for J in Natural range Src_Cards'First .. Src_Cards'Last loop
         Src_Stack.all.Push_Unchecked (Src_Cards (J));
      end loop;
      Src_O_Size := Src_Stack.all.Size;
      --
      for J in Natural range Dst_Cards'First .. Dst_Cards'Last loop
         Dst_Stack.all.Push_Unchecked (Dst_Cards (J));
      end loop;
      Dst_O_Size := Dst_Stack.all.Size;
      --
      Tab.Move_To (1, 2, Src_Exist);
      --
      Src_N_Size := Src_Stack.all.Size;
      Dst_N_Size := Dst_Stack.all.Size;
      --
      Aux_Diff := Src_O_Size - Diff;
      --
      AUnit.Assertions.Assert (Src_N_Size = Src_O_Size - Diff,
                               "Src_N_Size=" & Aux_Diff'Image &
                                 " /= " & Src_N_Size'Image);
      AUnit.Assertions.Assert (not Src_Stack.all.Has (Src_Exist),
                               "Src_Stack still has " & Src_Exist.Image);
      --
      Aux_Diff := Src_O_Size + Diff;
      AUnit.Assertions.Assert (Dst_N_Size = Dst_O_Size + Diff,
                               "Dst_N_Size=" & Aux_Diff'Image &
                                 " /= " & Dst_N_Size'Image);
      AUnit.Assertions.Assert (Dst_Stack.all.Has (Src_Exist),
                               "Dst_Stack has NOT " & Src_Exist.Image);
   end Move_To_1_Src_Not_E_Trgt_Not_E;

   --  EXCEPTION HERE!!!!!, src empty does not work
   procedure Move_To_1_Src_E_Trgt_E_Exc;
   procedure Move_To_1_Src_E_Trgt_E_Exc is
      Tab       : constant Tableau.Tableau_Type := Tableau.Construct;
      Not_Exist : constant Card.Card_Type
        := Card.Construct (Deck.Queen, Deck.Club);
   begin
      Tab.Move_To (1, 1, Not_Exist);
   exception
      when Tableau_Stack_Empty_Exception => raise;
      when Exc : others =>
         AUnit.
           Assertions.
             Assert (False,
                     "Move_To_1_Src_E_Trgt_E_Exc: " &
                       "wrong exception raised:" &
                       Ada.Exceptions.Exception_Name (Exc));
   end Move_To_1_Src_E_Trgt_E_Exc;

   procedure Move_To_1_Src_E_Trgt_E (T : in out Test) is
      pragma Unreferenced (T);
   begin
      AUnit.
        Assertions.
          Assert_Exception (Move_To_1_Src_E_Trgt_E_Exc'Access,
                            "Move_To_1_Src_E_Trgt_E_Exc: " &
                              "no exception raised");
   end Move_To_1_Src_E_Trgt_E;

   procedure Move_To_1_Src_E_Trgt_Not_E (T : in out Test) is
      pragma Unreferenced (T);
      Tab        : constant Tableau.Tableau_Type := Tableau.Construct;
      --
      Diff       : constant Integer := 1;
      --
      Src_Stack  : constant Tableau_Stack.Stack_Type_Access
        := Tab.Get_Stack (1);
      Src_Cards  : constant Some_Cards
        := (1 => Card.Construct (Deck.Ten,  Deck.Heart));
      Src_Exist  : constant Card.Card_Type := Src_Cards (Src_Cards'Last);
      Src_O_Size : Natural;
      Src_N_Size : Natural;
      --
      Dst_Stack  : constant Tableau_Stack.Stack_Type_Access
        := Tab.Get_Stack (2);
      Dst_Cards  : constant Some_Cards
        := (Card.Construct (Deck.Four, Deck.Heart),
            Card.Construct (Deck.Jack, Deck.Club));
      Dst_O_Size : Natural;
      Dst_N_Size : Natural;
      --
      Aux_Diff   : Integer;
   begin
      for J in Natural range Src_Cards'First .. Src_Cards'Last loop
         Src_Stack.all.Push_Unchecked (Src_Cards (J));
      end loop;
      Src_O_Size := Src_Stack.all.Size;
      --
      for J in Natural range Dst_Cards'First .. Dst_Cards'Last loop
         Dst_Stack.all.Push_Unchecked (Dst_Cards (J));
      end loop;
      Dst_O_Size := Dst_Stack.all.Size;
      --
      Tab.Move_To (1, 2, Src_Exist);
      --
      Src_N_Size := Src_Stack.all.Size;
      Dst_N_Size := Dst_Stack.all.Size;
      --
      Aux_Diff := Src_O_Size - Diff;
      --
      AUnit.Assertions.Assert (Src_N_Size = Src_O_Size - Diff,
                               "Src_N_Size=" & Aux_Diff'Image &
                                 " /= " & Src_N_Size'Image);
      AUnit.Assertions.Assert (not Src_Stack.all.Has (Src_Exist),
                               "Src_Stack still has " & Src_Exist.Image);
      --
      Aux_Diff := Src_O_Size + Diff;
      AUnit.Assertions.Assert (Dst_N_Size = Dst_O_Size + Diff,
                               "Dst_N_Size=" & Aux_Diff'Image &
                                 " /= " & Dst_N_Size'Image);
      AUnit.Assertions.Assert (Dst_Stack.all.Has (Src_Exist),
                               "Dst_Stack has NOT " & Src_Exist.Image);
   end Move_To_1_Src_E_Trgt_Not_E;

   procedure Move_To_X_Trgt_Does_Not_Accept_Exc;
   procedure Move_To_X_Trgt_Does_Not_Accept_Exc is
      Tab       : constant Tableau.Tableau_Type := Tableau.Construct;
      --
      Src_Stack : constant Tableau_Stack.Stack_Type_Access := Tab.Get_Stack (1);
      Src_Cards : constant Some_Cards
        := (Card.Construct (Deck.Three, Deck.Diamond),
            Card.Construct (Deck.Seven, Deck.Heart),
            Card.Construct (Deck.Jack,  Deck.Heart));
      Src_Exist : constant Card.Card_Type := Src_Cards (1);
      --
      Dst_Stack : constant Tableau_Stack.Stack_Type_Access := Tab.Get_Stack (2);
      Dst_Cards : constant Some_Cards
        := (1 => Card.Construct (Deck.Ten, Deck.Diamond));
   begin
      for J in Natural range Src_Cards'First .. Src_Cards'Last loop
         Src_Stack.all.Push_Unchecked (Src_Cards (J));
      end loop;
      for J in Natural range Dst_Cards'First .. Dst_Cards'Last loop
         Dst_Stack.all.Push_Unchecked (Dst_Cards (J));
      end loop;
      Tab.Move_To (1, 2, Src_Exist);
   exception
      when Tableau_Destination_Stack_Does_Not_Accept_Exception => raise;
      when Exc : others =>
         AUnit.
           Assertions.
             Assert (False,
                     "Move_To_X_Trgt_Does_Not_Accept_Exc: " &
                       "wrong exception raised:" &
                       Ada.Exceptions.Exception_Name (Exc));
   end Move_To_X_Trgt_Does_Not_Accept_Exc;

   procedure Move_To_X_Trgt_Does_Not_Accept (T : in out Test) is
      pragma Unreferenced (T);
   begin
      AUnit.
        Assertions.
          Assert_Exception (Move_To_X_Trgt_Does_Not_Accept_Exc'Access,
                            "Move_To_X_Trgt_Does_Not_Accept: " &
                              "no exception raised");
   end Move_To_X_Trgt_Does_Not_Accept;

   procedure Move_To_X_Src_Not_E_Trgt_E (T : in out Test) is
      pragma Unreferenced (T);
      Tab        : constant Tableau.Tableau_Type := Tableau.Construct;
      --
      Diff       : constant Integer := 2;
      --
      Src_Stack  : constant Tableau_Stack.Stack_Type_Access
        := Tab.Get_Stack (1);
      Src_Cards  : constant Some_Cards
        := (Card.Construct (Deck.Three, Deck.Diamond),
            Card.Construct (Deck.King,  Deck.Heart),
            Card.Construct (Deck.Seven, Deck.Heart));
      Src_Exist  : constant Card.Card_Type := Src_Cards (2);
      Src_O_Size : Natural;
      Src_N_Size : Natural;
      --
      Dst_Stack  : constant Tableau_Stack.Stack_Type_Access
        := Tab.Get_Stack (2);
      Dst_O_Size : Natural;
      Dst_N_Size : Natural;
      --
      Aux_Diff   : Integer;
   begin
      for J in Natural range Src_Cards'First .. Src_Cards'Last loop
         Src_Stack.all.Push_Unchecked (Src_Cards (J));
      end loop;
      Src_O_Size := Src_Stack.all.Size;
      Dst_O_Size := Dst_Stack.all.Size;
      --
      Tab.Move_To (1, 2, Src_Exist);
      --
      Src_N_Size := Src_Stack.all.Size;
      Dst_N_Size := Dst_Stack.all.Size;
      --
      Aux_Diff := Src_O_Size - Diff;
      --
      AUnit.Assertions.Assert (Src_N_Size = Src_O_Size - Diff,
                               "Src_N_Size=" & Aux_Diff'Image &
                                 " /= " & Src_N_Size'Image);
      AUnit.Assertions.Assert (not Src_Stack.all.Has (Src_Exist),
                               "Src_Stack still has " & Src_Exist.Image);
      --
      Aux_Diff := Src_O_Size + Diff;
      AUnit.Assertions.Assert (Dst_N_Size = Dst_O_Size + Diff,
                               "Dst_N_Size=" & Aux_Diff'Image &
                                 " /= " & Dst_N_Size'Image);
      AUnit.Assertions.Assert (Dst_Stack.all.Has (Src_Exist),
                               "Dst_Stack has NOT " & Src_Exist.Image);
   end Move_To_X_Src_Not_E_Trgt_E;

   procedure Move_To_X_Src_Not_E_Trgt_Not_E (T : in out Test) is
      pragma Unreferenced (T);
      Tab        : constant Tableau.Tableau_Type := Tableau.Construct;
      --
      Diff       : constant Integer := 2;
      --
      Src_Stack  : constant Tableau_Stack.Stack_Type_Access
        := Tab.Get_Stack (1);
      Src_Cards  : constant Some_Cards
        := (Card.Construct (Deck.Three, Deck.Diamond),
            Card.Construct (Deck.Ten,   Deck.Heart),
            Card.Construct (Deck.Seven, Deck.Heart));
      Src_Exist  : constant Card.Card_Type := Src_Cards (2);
      Src_O_Size : Natural;
      Src_N_Size : Natural;
      --
      Dst_Stack  : constant Tableau_Stack.Stack_Type_Access
        := Tab.Get_Stack (2);
      Dst_Cards  : constant Some_Cards
        := (Card.Construct (Deck.Four, Deck.Heart),
            Card.Construct (Deck.Jack, Deck.Club));
      Dst_O_Size : Natural;
      Dst_N_Size : Natural;
      --
      Aux_Diff   : Integer;
   begin
      for J in Natural range Src_Cards'First .. Src_Cards'Last loop
         Src_Stack.all.Push_Unchecked (Src_Cards (J));
      end loop;
      Src_O_Size := Src_Stack.all.Size;
      --
      for J in Natural range Dst_Cards'First .. Dst_Cards'Last loop
         Dst_Stack.all.Push_Unchecked (Dst_Cards (J));
      end loop;
      Dst_O_Size := Dst_Stack.all.Size;
      --
      Tab.Move_To (1, 2, Src_Exist);
      --
      Src_N_Size := Src_Stack.all.Size;
      Dst_N_Size := Dst_Stack.all.Size;
      --
      Aux_Diff := Src_O_Size - Diff;
      --
      AUnit.Assertions.Assert (Src_N_Size = Src_O_Size - Diff,
                               "Src_N_Size=" & Aux_Diff'Image &
                                 " /= " & Src_N_Size'Image);
      AUnit.Assertions.Assert (not Src_Stack.all.Has (Src_Exist),
                               "Src_Stack still has " & Src_Exist.Image);
      --
      Aux_Diff := Src_O_Size + Diff;
      AUnit.Assertions.Assert (Dst_N_Size = Dst_O_Size + Diff,
                               "Dst_N_Size=" & Aux_Diff'Image &
                                 " /= " & Dst_N_Size'Image);
      AUnit.Assertions.Assert (Dst_Stack.all.Has (Src_Exist),
                               "Dst_Stack has NOT " & Src_Exist.Image);
   end Move_To_X_Src_Not_E_Trgt_Not_E;

   --  EXCEPTION HERE!!!!!, src empty does not work
   procedure Move_To_X_Src_E_Trgt_Not_E_Exc;
   procedure Move_To_X_Src_E_Trgt_Not_E_Exc is
      Tab       : constant Tableau.Tableau_Type := Tableau.Construct;
      Not_Exist : constant Card.Card_Type
        := Card.Construct (Deck.Queen, Deck.Club);
   begin
      Tab.Move_To (1, 1, Not_Exist);
   exception
      when Tableau_Stack_Empty_Exception => raise;
      when Exc : others =>
         AUnit.
           Assertions.
             Assert (False,
                     "Move_To_1_Src_E_Trgt_E_Exc: " &
                       "wrong exception raised:" &
                       Ada.Exceptions.Exception_Name (Exc));
   end Move_To_X_Src_E_Trgt_Not_E_Exc;

   procedure Move_To_X_Src_E_Trgt_Not_E (T : in out Test) is
      pragma Unreferenced (T);
   begin
      AUnit.
        Assertions.
          Assert_Exception (Move_To_X_Src_E_Trgt_Not_E_Exc'Access,
                            "Move_To_X_Src_E_Trgt_Not_E_Exc: " &
                              "no exception raised");
   end Move_To_X_Src_E_Trgt_Not_E;

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

      Ret.Add_Test (Caller.
                     Create (N & "Move_To_Src_Stack_Equals_Trgt_Stack",
                       Move_To_Src_Stack_Equals_Trgt_Stack'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Move_To_Source_Selection_Does_Not_Exist",
                        Move_To_Source_Selection_Does_Not_Exist'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Move_To_1_Trgt_Does_Not_Accept",
                        Move_To_1_Trgt_Does_Not_Accept'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Move_To_1_Src_Not_E_Trgt_E",
                        Move_To_1_Src_Not_E_Trgt_E'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Move_To_1_Src_Not_E_Trgt_Not_E",
                        Move_To_1_Src_Not_E_Trgt_Not_E'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Move_To_1_Src_E_Trgt_E",
                        Move_To_1_Src_E_Trgt_E'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Move_To_1_Src_E_Trgt_Not_E",
                        Move_To_1_Src_E_Trgt_Not_E'Access));

      Ret.Add_Test (Caller.
                      Create (N & "Move_To_X_Trgt_Does_Not_Accept",
                        Move_To_X_Trgt_Does_Not_Accept'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Move_To_X_Src_Not_E_Trgt_E",
                        Move_To_X_Src_Not_E_Trgt_E'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Move_To_X_Src_Not_E_Trgt_Not_E",
                        Move_To_X_Src_Not_E_Trgt_Not_E'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Move_To_X_Src_E_Trgt_Not_E",
                        Move_To_X_Src_E_Trgt_Not_E'Access));

      return Ret;
   end Suite;

end Tableau.Test;
