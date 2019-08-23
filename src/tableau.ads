with Card;
with Pile_Of_Cards.FIFO;
with Short_Image_FIFO;
with Tableau_Stack;

package Tableau is

   type Tableau_Type is tagged private;

   type Valid_Stacks_Range is new Integer range 1 .. 7;

   Tableau_Stack_Empty_Exception                       : exception;
   Tableau_Target_Stack_Equals_Source_Stack_Exception  : exception;
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

   function Remove_Mandatory_Cards
     (T          : Tableau_Type;
      Candidates : Pile_Of_Cards.FIFO.Pile_Type_FIFO)
      return Pile_Of_Cards.FIFO.Pile_Type_FIFO;

--   function To_String (T : Tableau_Type) return String;

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

   type Stack_Images is array (Valid_Stacks_Range)
     of Short_Image_FIFO.Short_Image_FIFO_Type_Access;

   type One_Line_String is new String (1 .. 7 * 3);

   EMPTY_ONE_LINE : constant One_Line_String := "                     ";

   function To_String_One_Line (SIs : Stack_Images) return One_Line_String;

end Tableau;
