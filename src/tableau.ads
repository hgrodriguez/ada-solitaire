with Card;
with Pile_Of_Cards.FIFO;
with Tableau_Stack;

package Tableau is

   type Tableau_Type is tagged private;

   type Valid_Stacks_Range is new Integer range 1 .. 7;

   Tableau_Stack_Empty_Exception                       : exception;
   Tableau_Source_Card_Does_Not_Exist_Exception        : exception;
   Tableau_Destination_Stack_Does_Not_Accept_Exception : exception;

   function Construct return Tableau_Type;

   function Size (T : Tableau_Type) return Natural;

   procedure Push
     (T    : Tableau_Type;
      Pile : in out Pile_Of_Cards.FIFO.Pile_Type_FIFO);

   procedure Move_To
     (T               : Tableau_Type;
      Src_Index       : Valid_Stacks_Range;
      Dst_Index       : Valid_Stacks_Range;
      Card_To_Include : Card.Card_Type);

private
   type Stack_Array is array (Valid_Stacks_Range)
     of Tableau_Stack.Stack_Type_Access;

   type Tableau_Type is tagged record
      Stacks : Stack_Array;
   end record;

   function Pop_From_Stack
     (T : Tableau_Type;
      J : Valid_Stacks_Range)
      return Card.Card_Type;

   function Get_Stack
     (T : Tableau_Type;
      J : Valid_Stacks_Range)
      return Tableau_Stack.Stack_Type_Access;

end Tableau;
