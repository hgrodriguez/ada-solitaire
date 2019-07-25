with Deck; use Deck;

package body Foundation_Stack is

   function Construct (Suit : Deck.Suit_Type)
                       return Foundation_Stack.Stack_Type is
      ca   : Card_Array;
      ret  : Foundation_Stack.Stack_Type;
   begin
      for rank in Deck.Ace .. Deck.King loop
         ca (rank) := Card.Construct (rank, Suit);
      end loop;
      ret.Suit := Suit;
      ret.Cards := ca;
      ret.Top_Rank := Deck.Bottom;
      ret.Size := 0;
      return ret;
   end Construct;

   function Get_Suit (cs : Foundation_Stack.Stack_Type)
                      return Deck.Suit_Type is
   begin
      return cs.Suit;
   end Get_Suit;

   function Is_Empty (cs : Foundation_Stack.Stack_Type) return Boolean is
   begin
      return cs.Top_Rank = Deck.Bottom;
   end Is_Empty;

   function Size (cs :  Foundation_Stack.Stack_Type) return Integer is
   begin
      return cs.Size;
   end Size;

   function Accepts (cs : Foundation_Stack.Stack_Type)
                     return Card.Card_Type is
   begin
      return Card.Construct (Deck.Rank_Type'Succ (cs.Top_Rank), cs.Suit);
   end Accepts;

   procedure Push (cs : in out Foundation_Stack.Stack_Type;
                   c  : Card.Card_Type) is
   begin
      cs.Size := cs.Size + 1;
      cs.Top_Rank := Deck.Rank_Type'Succ (cs.Top_Rank);
      cs.Cards (Deck.Rank_Type'Val (cs.Size)) := c;
   end Push;

end Foundation_Stack;
