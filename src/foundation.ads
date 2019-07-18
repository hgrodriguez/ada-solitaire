with Deck;
with Card;
with Foundation_Stack;

package Foundation is

   type Foundation_Type is limited private;
   
   --   function Construct return Foundation_Type;
   procedure toremovelater;
   
private
   
   type Stack_Array is array (Deck.Suit_Type) of Foundation_Stack.Stack_Type;
   type Foundation_Type is limited record
      Stacks : Stack_Array;
   end record;
   

end Foundation;
