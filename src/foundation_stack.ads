with Definitions;
with Card;
with Pile_Of_Cards.LIFO;

package Foundation_Stack is

   type Stack_Type is tagged private;
   type Stack_Type_Access is access Stack_Type;

   --------------------------------------------------------------------
   --  constructs a Foundation_Stack with a suit
   function Construct (Suit : Definitions.Suit)
                       return Foundation_Stack.Stack_Type;

   --------------------------------------------------------------------
   --  returns the Suit of the Foundation_Stack
   function Get_Suit (cs : Foundation_Stack.Stack_Type)
                      return Definitions.Suit;

   --------------------------------------------------------------------
   --  checks, if Foundation_Stack is empty
   function Is_Empty (cs : Foundation_Stack.Stack_Type) return Boolean;

   --------------------------------------------------------------------
   --  checks, if Foundation_Stack is full
   function Is_Full (cs : Foundation_Stack.Stack_Type) return Boolean;

   --------------------------------------------------------------------
   --  returns count of Cards in Foundation_Stack
   function Size (cs :  Foundation_Stack.Stack_Type)
                  return Natural;

   --------------------------------------------------------------------
   --  returns Card, which can be accepted by Foundation_Stack
   function Accepts (cs :  Foundation_Stack.Stack_Type)
                     return Card.Card_Type;

   --------------------------------------------------------------------
   --  pushes the Card onto the Foundation_Stack
   --  raises exception if wrong Rank or wrong Suit
   Wrong_Suit_Exception : exception;
   Wrong_Rank_Exception : exception;
   procedure Push (cs : in out Foundation_Stack.Stack_Type;
                   c  : Card.Card_Type);

   --------------------------------------------------------------------
   --  converts the top card into readable format
   function To_String (cs : Foundation_Stack.Stack_Type)
                       return Card.Short_Image_Type;

private
   type Stack_Type is tagged record
      Suit  : Definitions.Suit;
      Cards : Pile_Of_Cards.LIFO.Pile_Type_LIFO;
   end record;

end Foundation_Stack;
