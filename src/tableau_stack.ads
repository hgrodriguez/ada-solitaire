with Card;

with Pile_Of_Cards.FIFO;
with Pile_Of_Cards.LIFO;

package Tableau_Stack is

   type Stack_Type is tagged private;
   type Stack_Type_Access is access Stack_Type;
   subtype Acceptable_Type is Pile_Of_Cards.FIFO.Pile_Type_FIFO;

   Tableau_Stack_Empty_Exception      : exception;
   Tableau_Stack_Wrong_Card_Exception : exception;

   function Construct return Stack_Type;

   function Size (T : Stack_Type) return Natural;
   function Has (T : Stack_Type; C : Card.Card_Type) return Boolean;

   function Is_Empty (T : Stack_Type) return Boolean;

   procedure Push_Checked (T : Stack_Type; C : Card.Card_Type);
   procedure Push_Unchecked (T : Stack_Type; C : Card.Card_Type);

   function Pop (T : Stack_Type) return Card.Card_Type;

   function Accepts (T : Stack_Type) return Acceptable_Type;

private
   type Stack_Type is tagged record
      Cards : Pile_Of_Cards.LIFO.Pile_Type_LIFO_Access;
   end record;

end Tableau_Stack;
