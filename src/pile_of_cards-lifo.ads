with Card;
with Pile_Of_Cards;

package Pile_Of_Cards.LIFO is
   --------------------------------------------------------------------
   --  LIFO operations, simulating a queue
   --------------------------------------------------------------------

   type Pile_Type_LIFO is new Pile_Of_Cards.Pile_Type with private;
   type Pile_Type_LIFO_Access is access Pile_Type_LIFO;

   function Construct return Pile_Type_LIFO;

   function Pop (pile : in out Pile_Type_LIFO) return Card.Card_Type;
   procedure Push (pile : in out Pile_Type_LIFO; c : Card.Card_Type);

   overriding
   function Peek  (pile : Pile_Type_LIFO) return Card.Card_Type;

   overriding
   function Has (Pile : Pile_Type_LIFO;
                 C    : Card.Card_Type) return Boolean;

private
   type Pile_Type_LIFO is new Pile_Of_Cards.Pile_Type with null record;

end Pile_Of_Cards.LIFO;
