with Deck;
with Card;

package Foundation_Stack is
   type Stack_Type is tagged private;

   -- constructs a Foundation_Stack with a suit
   function Construct (Suit : in Deck.Suit_Type)
                       return Foundation_Stack.Stack_Type;
   
   function Get_Suit (cs : in Foundation_Stack.Stack_Type)
                      return Deck.Suit_Type;
   
   function Is_Empty (cs : in Foundation_Stack.Stack_Type) return Boolean;
   
private
   type Card_Array is array (Deck.Rank_Type) of Card.Card_Type;
   
   type Stack_Type is tagged record
      Suit  : Deck.Suit_Type;
      Cards : Card_Array;
      Top   : Integer := 0;
   end record;

end Foundation_Stack;
