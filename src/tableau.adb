with Pile_Of_Cards.LIFO;

package body Tableau is

   function Construct return Tableau_Type is
      T : Tableau_Type;
      S : Tableau_Stack.Stack_Type_Access;
   begin
      for J in Valid_Stacks_Range loop
         S            := new Tableau_Stack.Stack_Type;
         S.all        := Tableau_Stack.Construct;
         T.Stacks (J) := S;
      end loop;
      return T;
   end Construct;

   function Size (T : Tableau_Type) return Natural is
      S : Natural := 0;
   begin
      for J in Valid_Stacks_Range loop
         S := S + T.Stacks (J).Size;
      end loop;
      return S;
   end Size;

   procedure Push
     (T : Tableau_Type; Pile : in out Pile_Of_Cards.FIFO.Pile_Type_FIFO)
   is
      J : Valid_Stacks_Range := Valid_Stacks_Range'First;
      C : Card.Card_Type;
   begin
      while not Pile.Is_Empty loop
         C := Pile.Get;
         T.Stacks (J).all.Push_Unchecked (C);
         if J = Valid_Stacks_Range'Last then
            J := Valid_Stacks_Range'First;
         else
            J := J + 1;
         end if;
      end loop;
   end Push;

   procedure Move_To
     (T         : Tableau_Type; Src_Index : Valid_Stacks_Range;
      Dst_Index : Valid_Stacks_Range; Card_To_Include : Card.Card_Type)
   is
      Src_Stack    : constant Tableau_Stack.Stack_Type_Access :=
        T.Stacks (Src_Index);
      Dst_Stack    : constant Tableau_Stack.Stack_Type_Access :=
        T.Stacks (Dst_Index);
      Acceptable   : constant Tableau_Stack.Acceptable_Type :=
        Dst_Stack.all.Accepts;
      Transfer     : Pile_Of_Cards.LIFO.Pile_Type_LIFO :=
        Pile_Of_Cards.LIFO.Construct;
      Card_To_Xfer : Card.Card_Type;
   begin
      if not Src_Stack.all.Has (Card_To_Include) then
         raise Tableau_Source_Card_Does_Not_Exist_Exception;
      end if;
      if not Acceptable.Has (Card_To_Include) then
         raise Tableau_Destination_Stack_Does_Not_Accept_Exception;
      end if;
      loop
         Card_To_Xfer := Src_Stack.all.Pop;
         Transfer.Push (Card_To_Xfer);
         exit when Card_To_Xfer.Is_Equal_To (Card_To_Include);
      end loop;
      while not Transfer.Is_Empty loop
         Card_To_Xfer := Transfer.Pop;
         Dst_Stack.Push_Unchecked (Card_To_Xfer);
      end loop;
   end Move_To;

   function Remove_Mandatory_Cards
     (T : Tableau_Type; Candidates : Pile_Of_Cards.FIFO.Pile_Type_FIFO)
      return Pile_Of_Cards.FIFO.Pile_Type_FIFO
   is
      Stack           : Tableau_Stack.Stack_Type_Access;
      Peek_Card       : Card.Card_Type;
      Move_Card       : Card.Card_Type;
      Mandatory_Cards : Pile_Of_Cards.FIFO.Pile_Type_FIFO :=
        Pile_Of_Cards.FIFO.Construct;
   begin
      for J in Valid_Stacks_Range loop
         Stack := T.Get_Stack (J);
         if not Stack.all.Is_Empty then
            Peek_Card := Stack.all.Peek;
            if Candidates.Has (Peek_Card) then
               Move_Card := Stack.all.Pop;
               Mandatory_Cards.Put (Move_Card);
            end if;
         end if;
      end loop;
      return Mandatory_Cards;
   end Remove_Mandatory_Cards;

   --------------------------------------------------------------------
   --
   function Pop_From_Stack
     (T : Tableau_Type; J : Valid_Stacks_Range) return Card.Card_Type
   is
   begin
      return T.Stacks (J).all.Pop;
   exception
      when Tableau_Stack.Tableau_Stack_Empty_Exception =>
         raise Tableau_Stack_Empty_Exception;
   end Pop_From_Stack;

   function Get_Stack
     (T : Tableau_Type; J : Valid_Stacks_Range) return Tableau_Stack
     .Stack_Type_Access
   is
   begin
      return T.Stacks (J);
   end Get_Stack;

end Tableau;
