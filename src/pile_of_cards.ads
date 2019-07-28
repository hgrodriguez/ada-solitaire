package Pile_Of_Cards is
   --------------------------------------------------------------------
   --  It does exactly what is says on the tin.
   --  It handles a pile of cards without any further restrictions,
   --  e.g. particular order, suit, rank etc.

   type Pile_Type is tagged private;

   function Is_Empty (pile : Pile_Type) return Boolean;
   function Size (pile : Pile_Type) return Natural;
private
   type Pile_Type is tagged record
      Count : Natural := 0;
   end record;

end Pile_Of_Cards;
