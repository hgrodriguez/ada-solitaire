with Card;
with Deck;
with Foundation_Stack;
with Pile_Of_Cards.FIFO;

package Foundation is

   type Foundation_Type is tagged private;
   subtype Acceptable_Type is Pile_Of_Cards.FIFO.Pile_Type_FIFO;

   Foundation_Wrong_Card_Exception : exception;

   To_String_Separator : String (1 .. 1) := " ";
   subtype To_String_Type is String (1 .. 4 * 3 - 1);

   function Construct return Foundation_Type;

   function Is_Empty (F : Foundation_Type) return Boolean;

   function Size (F : Foundation_Type) return Natural;

   function Accepts (F : Foundation_Type) return Acceptable_Type;

   procedure Put (F : Foundation_Type; c : Card.Card_Type);

   function To_String (F : Foundation_Type) return To_String_Type;

private

   type Stack_Array is array (Deck.Suit_Type)
     of Foundation_Stack.Stack_Type_Access;
   type Foundation_Type is tagged record
      Stacks : Stack_Array;
   end record;

end Foundation;
