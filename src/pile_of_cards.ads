with Ada.Containers.Doubly_Linked_Lists;

with Card;

package Pile_Of_Cards is
   --------------------------------------------------------------------
   --  It does exactly what is says on the tin.
   --  It handles a pile of cards without any further restrictions,
   --  e.g. particular order, suit, rank etc.

   type Pile_Type is abstract tagged private;

   --------------------------------------------------------------------
   --  construct a pile
   function Construct return Pile_Type is abstract;

   --------------------------------------------------------------------
   --  check if pile is empty
   function Is_Empty (pile : Pile_Type) return Boolean;

   --------------------------------------------------------------------
   --  returns size of pile
   function Size (pile : Pile_Type) return Natural;

   --------------------------------------------------------------------
   --  check if pile contains card
   function Has (pile : Pile_Type;
                 C    : Card.Card_Type) return Boolean is abstract;

   --------------------------------------------------------------------
   --  Peek into pile
   --  exception if pile is empty
   Pile_Empty_Exception : exception;
   function Peek (pile : Pile_Type) return Card.Card_Type is abstract;

private
   package Card_Type_DLL is new Ada.Containers.
     Doubly_Linked_Lists (Element_Type => Card.Card_Type, "=" => Card."=");

   type Pile_Type is abstract tagged record
      Cards : Card_Type_DLL.List;
   end record;

end Pile_Of_Cards;
