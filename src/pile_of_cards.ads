with Card;

package Pile_Of_Cards is
   --------------------------------------------------------------------
   --  It does exactly what is says on the tin.
   --  It handles a pile of cards without any further restrictions,
   --  e.g. particular order, suit, rank etc.

   type Pile_Type is abstract tagged private;

   Pile_Empty_Exception : exception;

   function Is_Empty (pile : Pile_Type) return Boolean;
   function Size (pile : Pile_Type) return Natural;
   function Peek (pile : Pile_Type) return Card.Card_Type is abstract;
private
   type Pile_Type is abstract tagged record
      Count : Natural := 0;
   end record;

end Pile_Of_Cards;
