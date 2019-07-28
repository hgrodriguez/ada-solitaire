package body Foundation is

   Foundation_Has_Been_Created : Boolean := False;
   The_Foundation : Foundation_Type;

   function Construct return Foundation_Type is
      ST_Diamond : Foundation_Stack.Stack_Type_Access;
      ST_Club    : Foundation_Stack.Stack_Type_Access;
      ST_Heart   : Foundation_Stack.Stack_Type_Access;
      ST_Spade   : Foundation_Stack.Stack_Type_Access;
   begin
      if not Foundation_Has_Been_Created then
         ST_Diamond := new Foundation_Stack.Stack_Type;
         ST_Diamond.all := Foundation_Stack.Construct (Deck.Diamond);
         The_Foundation.Stacks (Deck.Diamond) := ST_Diamond;

         ST_Club    := new Foundation_Stack.Stack_Type;
         ST_Club.all    := Foundation_Stack.Construct (Deck.Club);
         The_Foundation.Stacks (Deck.Club)    := ST_Club;

         ST_Heart   := new Foundation_Stack.Stack_Type;
         ST_Heart.all   := Foundation_Stack.Construct (Deck.Heart);
         The_Foundation.Stacks (Deck.Heart)   := ST_Heart;

         ST_Spade   := new Foundation_Stack.Stack_Type;
         ST_Spade.all   := Foundation_Stack.Construct (Deck.Spade);
         The_Foundation.Stacks (Deck.Spade)   := ST_Spade;

         Foundation_Has_Been_Created := False;
      end if;
      return The_Foundation;
   end Construct;

   function Accepts (F : Foundation_Type) return Acceptable_Type is
      ret : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
   begin
      for suit in Deck.Suit_Type_Valid_Range loop
         ret.Put (F.Stacks (suit).Accepts);
      end loop;
      return ret;
   end Accepts;

   function Size (F : Foundation_Type) return Natural is
      S : Natural := 0;
   begin
      for suit in Deck.Suit_Type_Valid_Range loop
         S := S + F.Stacks (suit).Size;
      end loop;
      return S;
   end Size;

   procedure Put (F : Foundation_Type; c : Card.Card_Type) is
   begin
      F.Stacks (c.Get_Suit).Push (c);
   end Put;

end Foundation;
