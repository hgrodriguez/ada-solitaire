with Card;
with Pile_Of_Cards;

package Pile_Of_Cards.FIFO is
   --------------------------------------------------------------------
   --  FIFO operations, simulating a queue
   --------------------------------------------------------------------

   type Pile_Type_FIFO is new Pile_Of_Cards.Pile_Type with private;
   type Pile_Type_FIFO_Access is access Pile_Type_FIFO;

   function Construct return Pile_Type_FIFO;

   --  Put:Semantics
   --  put c in front of any other card in the pile
   function Get (pile : in out Pile_Type_FIFO) return Card.Card_Type;
   procedure Put (pile : in out Pile_Type_FIFO; c : Card.Card_Type);

private
   --------------------------------------------------------------------
   --  the dynamic list of cards
   --  I know, that there are Containers in Ada, just exercise for me
   type List_Element;
   type List_Element_Access is access List_Element;

   type Pile_Type_FIFO is new Pile_Of_Cards.Pile_Type with record
      Head  : List_Element_Access := null;
      Tail  : List_Element_Access := null;
   end record;

end Pile_Of_Cards.FIFO;
