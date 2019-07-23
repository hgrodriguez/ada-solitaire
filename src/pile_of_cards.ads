with Card;

package Pile_Of_Cards is
   --------------------------------------------------------------------
   -- It does exactly what is says on the tin.
   -- It handles a pile of cards without any further restrictions,
   -- e.g. particular order, suit, rank etc.

   type Pile_Type is tagged private;

   function Construct return Pile_Type;

   function Is_Empty (pile : in Pile_Type) return Boolean;
   function Size (pile : in Pile_Type) return Natural;

   --------------------------------------------------------------------
   -- FIFO operations, simulating a queue
   --------------------------------------------------------------------

   -- Put:Semantics
   -- put c in front of any other card in the pile
   procedure Put (pile : in out Pile_Type; c : in Card.Card_Type);


private
   --------------------------------------------------------------------
   -- the dynamic list of cards
   -- I know, that there are Containers in Ada, just exercise for me
   type List_Element;
   type List_Element_Access is access List_Element;

   type Pile_Type is tagged record
      Count : Natural := 0;
      Head  : List_Element_Access := null;
      Tail  : List_Element_Access := null;
   end record;


end Pile_Of_Cards;
