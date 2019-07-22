package body Pile_Of_Cards is

   type List_Element is record
      C        : Card.Card_Type;
      Next     : List_Element_Access;
      Previous : List_Element_Access;
   end record;

   function Construct return Pile_Type is
      pile : Pile_Type;
   begin
      return pile;
   end Construct;

   function Is_Empty (pile : in Pile_Type) return Boolean is
   begin
      return pile.Count = 0;
   end Is_Empty;

   function Size (pile : in Pile_Type) return Natural is
   begin
      return pile.Count;
   end Size;

   --------------------------------------------------------------------
   -- FIFO operations
   procedure Put (pile : in out Pile_Type; c : Card.Card_Type) is
      -- Put
   begin
      null;
   end Put;



end Pile_Of_Cards;
