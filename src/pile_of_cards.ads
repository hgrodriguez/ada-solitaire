package Pile_Of_Cards is
   --------------------------------------------------------------------
   -- It does exactly what is says on the tin.
   -- It handles a pile of cards without any further restrictions,
   -- e.g. particular order, suit, rank etc.

   type Pile_Type is tagged private;

   function Construct return Pile_Type;

private
   type Pile_Type is tagged record
      null;
   end record;


end Pile_Of_Cards;
