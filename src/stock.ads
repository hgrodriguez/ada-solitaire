with Card;
with Pile_Of_Cards.FIFO;

package Stock is

   type Stock_Type is tagged private;
   Stock_Empty_Exception : exception;

   function Construct return Stock_Type;

   function Size (S : Stock_Type) return Natural;

   function Fetch (S : Stock_Type) return Pile_Of_Cards.FIFO.Pile_Type_FIFO;

   function To_String (S : Stock_Type; Peek : Boolean := False)
                       return Card.Short_Image_Type;

private
   type Stock_Type is tagged record
      Pile : Pile_Of_Cards.FIFO.Pile_Type_FIFO_Access;
   end record;

   function Peek (S : Stock_Type) return Card.Card_Type;
end Stock;
