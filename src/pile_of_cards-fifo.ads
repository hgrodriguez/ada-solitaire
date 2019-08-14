with Card;
with Pile_Of_Cards;

package Pile_Of_Cards.FIFO is
   --------------------------------------------------------------------
   --  FIFO operations, simulating a queue
   --------------------------------------------------------------------

   type Pile_Type_FIFO is new Pile_Of_Cards.Pile_Type with private;
   type Pile_Type_FIFO_Access is access Pile_Type_FIFO;

   function Construct return Pile_Type_FIFO;

   function Get (Pile : in out Pile_Type_FIFO) return Card.Card_Type;
   procedure Put (Pile : in out Pile_Type_FIFO; C : Card.Card_Type);

   overriding
   function Peek (Pile : Pile_Type_FIFO) return Card.Card_Type;

   overriding
   function Has (Pile : Pile_Type_FIFO;
                 C    : Card.Card_Type) return Boolean;

private
   type Pile_Type_FIFO is new Pile_Of_Cards.Pile_Type with null record;

end Pile_Of_Cards.FIFO;
