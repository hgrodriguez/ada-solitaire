package body Foundation_Stack is

   function Construct (Suit : in Deck.Suit_Type)
                       return Foundation_Stack.Stack_Type is
      ca   : Card_Array;
      ret  : Foundation_Stack.Stack_Type;
   begin
      for rank in Deck.Ace .. Deck.King loop
         ca(rank) := Card.Construct (rank, Suit);
      end loop;
      ret.Suit := Suit;
      ret.Cards := ca;
      ret.Top := 0;
      return ret;
   end Construct;
   
   function Get_Suit (cs : in Foundation_Stack.Stack_Type)
                      return Deck.Suit_Type is
   begin
      return cs.Suit;
   end Get_Suit;
   
   function Is_Empty (cs : in Foundation_Stack.Stack_Type) return Boolean is
   begin
      return cs.Top = 0;
   end Is_Empty;
   
end Foundation_Stack;
