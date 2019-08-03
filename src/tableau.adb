package body Tableau is

   function Construct return Tableau_Type is
      T : Tableau_Type;
      S : Tableau_Stack.Stack_Type_Access;
   begin
      for J in Valid_Stacks_Range loop
         S := new Tableau_Stack.Stack_Type;
         S.all := Tableau_Stack.Construct;
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

   procedure Push (T    : Tableau_Type;
                   Pile : in out Pile_Of_Cards.FIFO.Pile_Type_FIFO) is
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

   function Pop_From_Stack
     (T : Tableau_Type;
      J : Valid_Stacks_Range)
      return Card.Card_Type is
   begin
      return T.Stacks (J).all.Pop;
   exception
      when Tableau_Stack.Tableau_Stack_Empty_Exception =>
         raise Tableau_Stack_Empty_Exception;
   end Pop_From_Stack;

   --------------------------------------------------------------------
   --
   function Get_Stack (T : Tableau_Type; J : Valid_Stacks_Range)
                       return Tableau_Stack.Stack_Type_Access is
   begin
      return T.Stacks (J);
   end Get_Stack;

end Tableau;
