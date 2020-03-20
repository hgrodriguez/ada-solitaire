with Card;
with Short_Image_FIFO;
with Pile_Of_Cards;

package Pile_Of_Cards.LIFO is
   --------------------------------------------------------------------
   --  LIFO operations, simulating a stack
   --------------------------------------------------------------------

   --------------------------------------------------------------------
   --  the derived type behaving like a LIFO
   type Pile_Type_LIFO is new Pile_Of_Cards.Pile_Type with private;
   type Pile_Type_LIFO_Access is access Pile_Type_LIFO;

   --------------------------------------------------------------------
   --  construct a LIFO pile
   overriding
   function Construct return Pile_Type_LIFO;

   --------------------------------------------------------------------
   --  pops the top of the LIFO
   --  exception, if LIFO is empty
   function Pop (Pile : in out Pile_Type_LIFO) return Card.Card_Type;

   --------------------------------------------------------------------
   --  pushes one card onto the LIFO
   procedure Push (Pile : in out Pile_Type_LIFO; C : Card.Card_Type);

   --------------------------------------------------------------------
   --  collects all short images of all card in the LIFO
   procedure Collect (Pile : Pile_Type_LIFO;
                      SIF  : in out Short_Image_FIFO.Short_Image_FIFO_Type);

private
   type Pile_Type_LIFO is new Pile_Of_Cards.Pile_Type with null record;

end Pile_Of_Cards.LIFO;
