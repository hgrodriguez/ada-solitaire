with Cards;

package body Foundation_Stack is

   --------------------------------------------------------------------
   --
   function Construct (Suit : Definitions.Suit)
                       return Foundation_Stack.Stack_Type is
      ret  : Foundation_Stack.Stack_Type;
   begin
      ret.Suit := Suit;
      ret.Cards := Pile_Of_Cards.LIFO.Construct;
      return ret;
   end Construct;

   --------------------------------------------------------------------
   --
   function Get_Suit (cs : Foundation_Stack.Stack_Type)
                      return Definitions.Suit is
   begin
      return cs.Suit;
   end Get_Suit;

   --------------------------------------------------------------------
   --
   function Is_Empty (cs : Foundation_Stack.Stack_Type) return Boolean is
   begin
      return cs.Cards.Is_Empty;
   end Is_Empty;

   --------------------------------------------------------------------
   --
   function Is_Full (cs : Foundation_Stack.Stack_Type) return Boolean is
   begin
      return cs.Cards.Size = 13;
   end Is_Full;

   --------------------------------------------------------------------
   --
   function Size (cs :  Foundation_Stack.Stack_Type)
                  return Natural is
   begin
      return cs.Cards.Size;
   end Size;

   --------------------------------------------------------------------
   --
   function Accepts (cs : Foundation_Stack.Stack_Type)
                     return Card.Card_Type is
      tos : Card.Card_Type;
      ret : Card.Card_Type;
   begin
      if cs.Cards.Is_Empty then
         ret := Card.Construct (Rank => Definitions.Ace,
                                Suit => cs.Suit);
      else
         tos := cs.Cards.Peek;
         ret := Card.Construct (Rank => Definitions.Rank'Succ (tos.Get_Rank),
                                Suit => cs.Suit);
      end if;
      return ret;
   end Accepts;

   --------------------------------------------------------------------
   --
   procedure Push (cs : in out Foundation_Stack.Stack_Type;
                   c  : Card.Card_Type) is
      acceptable_card : constant Card.Card_Type := cs.Accepts;
      use Definitions;
   begin
      if Cards.Is_Equal_To (acceptable_card, c) then
         cs.Cards.Push (c);
      else
         --  separate between wrong suit and wrong rank
         if cs.Suit /= c.Get_Suit then
            raise Wrong_Suit_Exception;
         else
            raise Wrong_Rank_Exception;
         end if;
      end if;
   end Push;

   --------------------------------------------------------------------
   --
   function To_String (cs : Foundation_Stack.Stack_Type)
                       return Card.Short_Image_Type is
      C : Card.Card_Type;
   begin
      if cs.Cards.Is_Empty then
         return Card.Empty_Short_Image;
      else
         C := cs.Cards.Peek;
         return C.Short_Image;
      end if;
   end To_String;

   function Ansi_To_String (cs : Foundation_Stack.Stack_Type)
                            return Unbounded_String is
      C : Card.Card_Type;
   begin
      if cs.Cards.Is_Empty then
         return To_Unbounded_String (Card.Empty_Short_Image);
      else
         C := cs.Cards.Peek;
         return C.Ansi_Image;
      end if;
   end Ansi_To_String;

end Foundation_Stack;
