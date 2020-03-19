with Card;
with Pile_Of_Cards;

package Pile_Of_Cards.FIFO is
   --------------------------------------------------------------------
   --  FIFO operations, simulating a queue
   --------------------------------------------------------------------

   type Pile_Type_FIFO is new Pile_Of_Cards.Pile_Type with private;
   type Pile_Type_FIFO_Access is access Pile_Type_FIFO;

   --------------------------------------------------------------------
   --  construct a FIFO pile
   overriding
   function Construct return Pile_Type_FIFO;

   --------------------------------------------------------------------
   --  gets the first of the FIFO
   --  exception, if FIFO is empty
   function Get (Pile : in out Pile_Type_FIFO) return Card.Card_Type;

   --------------------------------------------------------------------
   --  puts one card into the FIFO
   procedure Put (Pile : in out Pile_Type_FIFO; C : Card.Card_Type);

   --------------------------------------------------------------------
   --  peeks the first FIFO element
   --  exception, if FIFO is empty
   overriding
   function Peek (Pile : Pile_Type_FIFO) return Card.Card_Type;

   --------------------------------------------------------------------
   --  check if pile contains card
   overriding
   function Has (Pile : Pile_Type_FIFO;
                 C    : Card.Card_Type) return Boolean;

private
   type Pile_Type_FIFO is new Pile_Of_Cards.Pile_Type with null record;

end Pile_Of_Cards.FIFO;
