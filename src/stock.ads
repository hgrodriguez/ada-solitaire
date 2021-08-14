--  represents a stock of poker cards

with Card;
with Pile_Of_Cards.FIFO;

package Stock is

   type Stock_Type is tagged private;

   --------------------------------------------------------------------
   --  construct a stock, this includes randomize the cards
   function Construct return Stock_Type;

   --------------------------------------------------------------------
   --  size of stock measured in cards
   function Size (S : Stock_Type) return Natural;

   --------------------------------------------------------------------
   --  fetch up to seven cards
   --  if less than seven cards left, fetch all -> stock empty
   --  exception if stock is empty
   Stock_Empty_Exception : exception;
   function Fetch (S : Stock_Type) return Pile_Of_Cards.FIFO.Pile_Type_FIFO;

   --------------------------------------------------------------------
   --  fetch exactly one card from stock
   --  exception raised if stock is empty
   function Fetch_One (S : Stock_Type) return Card.Card_Type;

   --------------------------------------------------------------------
   --  convert top card into an image
   --  if Peek is True -> then the card image will come up, else Obscured
   function To_String (S : Stock_Type; Peek : Boolean := False)
                       return Card.Short_Image_Type;

private
   --------------------------------------------------------------------
   --  implementation of stock
   type Stock_Type is tagged record
      Pile : Pile_Of_Cards.FIFO.Pile_Type_FIFO_Access;
   end record;

   --------------------------------------------------------------------
   --  peek into top card
   --  if stock is empty -> Stock_Empty_Exception
   --  else return top card of stock
   function Peek (S : Stock_Type) return Card.Card_Type;
end Stock;
