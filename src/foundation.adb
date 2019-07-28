package body Foundation is

   Foundation_Has_Been_Created : Boolean := False;
   The_Foundation : Foundation_Type;

   function Construct return Foundation_Type is
      ST_Diamond : Foundation_Stack.Stack_Type;
      ST_Club    : Foundation_Stack.Stack_Type;
      ST_Heart   : Foundation_Stack.Stack_Type;
      ST_Spade   : Foundation_Stack.Stack_Type;
   begin
      if not Foundation_Has_Been_Created then
         ST_Diamond := Foundation_Stack.Construct (Deck.Diamond);
         ST_Club    := Foundation_Stack.Construct (Deck.Club);
         ST_Heart   := Foundation_Stack.Construct (Deck.Heart);
         ST_Spade   := Foundation_Stack.Construct (Deck.Spade);

         The_Foundation.Stacks (Deck.Diamond) := ST_Diamond;
         The_Foundation.Stacks (Deck.Club)    := ST_Club;
         The_Foundation.Stacks (Deck.Heart)   := ST_Heart;
         The_Foundation.Stacks (Deck.Spade)   := ST_Spade;

         Foundation_Has_Been_Created := True;
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

   function Size  (F : Foundation_Type) return Natural is
      S : Natural := 0;
   begin
      for suit in Deck.Suit_Type_Valid_Range loop
         S := S + F.Stacks (suit).Size;
      end loop;
      return S;
   end Size;

   procedure Put (F : Foundation_Type; c : Card.Card_Type) is
      FS : Foundation_Stack.Stack_Type := F.Stacks (c.Get_Suit);
   begin
      FS.Push (c);
   end Put;

end Foundation;
