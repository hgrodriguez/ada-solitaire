with Deck;
with Card;

package Foundation_Stack is
   type Stack_Type is tagged private;

   --  constructs a Foundation_Stack with a suit
   function Construct (Suit : Deck.Suit_Type)
                       return Foundation_Stack.Stack_Type;

   function Get_Suit (cs : Foundation_Stack.Stack_Type)
                      return Deck.Suit_Type;

   function Is_Empty (cs : Foundation_Stack.Stack_Type) return Boolean;

   function Size (cs :  Foundation_Stack.Stack_Type) return Integer;

   function Accepts (cs :  Foundation_Stack.Stack_Type)
                     return Card.Card_Type;

   procedure Push (cs : in out Foundation_Stack.Stack_Type;
                   c  : Card.Card_Type);

private
   type Card_Array is array (Deck.Rank_Type) of Card.Card_Type;

   type Stack_Type is tagged record
      Suit     : Deck.Suit_Type;
      Cards    : Card_Array;
      Top_Rank : Deck.Rank_Type;
      Size     : Integer;
   end record;

end Foundation_Stack;
