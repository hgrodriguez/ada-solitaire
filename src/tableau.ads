with Tableau_Stack;

package Tableau is

   type Tableau_Type is tagged private;

   function Construct return Tableau_Type;

   function Size (T : Tableau_Type) return Natural;

private
   type Stack_Array is array (1 .. 7)
     of Tableau_Stack.Stack_Type_Access;

   type Tableau_Type is tagged record
      Stacks : Stack_Array;
   end record;

end Tableau;
