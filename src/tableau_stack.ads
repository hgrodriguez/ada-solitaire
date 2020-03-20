with Card;

with Short_Image_FIFO;

with Pile_Of_Cards.FIFO;
with Pile_Of_Cards.LIFO;

package Tableau_Stack is

   type Stack_Type is tagged private;
   type Stack_Type_Access is access Stack_Type;
   subtype Acceptable_Type is Pile_Of_Cards.FIFO.Pile_Type_FIFO;

   --------------------------------------------------------------------
   --  construct a Stack
   function Construct (Number : Integer) return Stack_Type;

   --------------------------------------------------------------------
   --  returns number of Stack
   function Number (T : Stack_Type) return Integer;

   --------------------------------------------------------------------
   --  returns count of Cards in Stack
   function Size (T : Stack_Type) return Natural;

   --------------------------------------------------------------------
   --  checks, if the Stack has the Card
   function Has (T : Stack_Type; C : Card.Card_Type) return Boolean;

   --------------------------------------------------------------------
   --  checks, if Stack is empty
   function Is_Empty (T : Stack_Type) return Boolean;

   --------------------------------------------------------------------
   --  pushes a Card, and checks if Card is in acceptable set
   --  exception raised, if wrong/not acceptable Card is pushed
   Tableau_Stack_Wrong_Card_Exception : exception;
   procedure Push_Checked (T : Stack_Type; C : Card.Card_Type);

   --------------------------------------------------------------------
   --  pushes the Card independent if acceptable or not
   --  this happens, when Cards are moved from the Stock
   procedure Push_Unchecked (T : Stack_Type; C : Card.Card_Type);

   --------------------------------------------------------------------
   --  shared Exception for Peek/Pop
   Tableau_Stack_Empty_Exception : exception;
   --------------------------------------------------------------------
   --  returns the bottom most Card of Stack without removing
   --  raises exception, if Stack is empty
   function Peek (T : Stack_Type) return Card.Card_Type;

   --------------------------------------------------------------------
   --  returns the bottom most Card of Stack with removing it
   --  raises exception, if Stack is empty
   function Pop (T : Stack_Type) return Card.Card_Type;

   --------------------------------------------------------------------
   --  returns the set of acceptable Cards
   function Accepts (T : Stack_Type) return Acceptable_Type;

   --------------------------------------------------------------------
   --  returns the Stack as readable format top down
   function Short_Images (T : Stack_Type)
                          return Short_Image_FIFO.Short_Image_FIFO_Type;

private
   type Stack_Type is tagged record
      Number : Integer;
      Cards  : Pile_Of_Cards.LIFO.Pile_Type_LIFO_Access;
   end record;

end Tableau_Stack;
