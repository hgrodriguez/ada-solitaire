with Deck;
with Card;
with Pile_Of_Cards.LIFO;

package Foundation_Stack is
   type Stack_Type is tagged private;
   type Stack_Type_Access is access Stack_Type;

   --  constructs a Foundation_Stack with a suit
   function Construct (Suit : Deck.Suit_Type)
                       return Foundation_Stack.Stack_Type;

   function Get_Suit (cs : Foundation_Stack.Stack_Type)
                      return Deck.Suit_Type;

   function Is_Empty (cs : Foundation_Stack.Stack_Type) return Boolean;
   function Is_Full (cs : Foundation_Stack.Stack_Type) return Boolean;

   function Size (cs :  Foundation_Stack.Stack_Type) return Integer;

   function Accepts (cs :  Foundation_Stack.Stack_Type)
                     return Card.Card_Type;

   procedure Push (cs : in out Foundation_Stack.Stack_Type;
                   c  : Card.Card_Type);

private
   type Stack_Type is tagged record
      Suit  : Deck.Suit_Type;
      Cards : Pile_Of_Cards.LIFO.Pile_Type_LIFO;
   end record;

end Foundation_Stack;
