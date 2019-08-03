with Pile_Of_Cards.FIFO;
with Tableau_Stack;

package Tableau is

   type Tableau_Type is tagged private;

   type Valid_Stacks_Range is new Integer range 1 .. 7;

   function Construct return Tableau_Type;

   function Size (T : Tableau_Type) return Natural;

   procedure Push
     (T    : Tableau_Type;
      Pile : in out Pile_Of_Cards.FIFO.Pile_Type_FIFO);

   function Get_Stack
     (T : Tableau_Type;
      J : Valid_Stacks_Range)
      return Tableau_Stack.Stack_Type_Access;

private
   type Stack_Array is array (Valid_Stacks_Range)
     of Tableau_Stack.Stack_Type_Access;

   type Tableau_Type is tagged record
      Stacks : Stack_Array;
   end record;

end Tableau;
