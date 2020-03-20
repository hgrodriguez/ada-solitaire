with Ada.Containers.Doubly_Linked_Lists;

with Card;

package Pile_Of_Cards is
   --------------------------------------------------------------------
   --  It does exactly what is says on the tin.
   --  It handles a pile of cards without any further restrictions,
   --  e.g. particular order, suit, rank etc.

   type Pile_Type is tagged private;

   --------------------------------------------------------------------
   --  construct a pile
   function Construct return Pile_Type;

   --------------------------------------------------------------------
   --  check if pile is empty
   function Is_Empty (Pile : Pile_Type) return Boolean;

   --------------------------------------------------------------------
   --  returns size of pile
   function Size (Pile : Pile_Type) return Natural;

   --------------------------------------------------------------------
   --  check if pile contains card
   function Has (Pile : Pile_Type;
                 C    : Card.Card_Type) return Boolean;

   --------------------------------------------------------------------
   --  Peek into pile
   --  exception if pile is empty
   Pile_Empty_Exception : exception;
   function Peek (Pile : Pile_Type) return Card.Card_Type;

   --------------------------------------------------------------------
   --  Duplicate a pile
--    function Duplicate (pile : Pile_Type) return Pile_Type;

private

   --------------------------------------------------------------------
   --  FIFO Operations
   --------------------------------------------------------------------
   --  gets the first of the FIFO
   --  exception, if FIFO is empty
   function Get (Pile : in out Pile_Type) return Card.Card_Type;

   --------------------------------------------------------------------
   --  puts one card into the FIFO
   procedure Put (Pile : in out Pile_Type; C : Card.Card_Type);

   --------------------------------------------------------------------
   --  LIFO Operations
   --------------------------------------------------------------------
   --------------------------------------------------------------------
   --  pushes one card onto the LIFO
   procedure Push (pile : in out Pile_Type; c : Card.Card_Type);

   --------------------------------------------------------------------
   --  pops the top of the LIFO
   --  exception, if LIFO is empty
   function Pop (pile : in out Pile_Type) return Card.Card_Type;

   package Card_Type_DLL is new Ada.Containers.
     Doubly_Linked_Lists (Element_Type => Card.Card_Type, "=" => Card."=");

   type Pile_Type is tagged record
      Cards : Card_Type_DLL.List;
   end record;

end Pile_Of_Cards;
