package body Pile_Of_Cards is

   function Construct return Pile_Type is
      pile : Pile_Type;
   begin
      return pile;
   end Construct;

   function Is_Empty (pile : in Pile_Type) return Boolean is
   begin
      return pile.Count = 0;
   end Is_Empty;

end Pile_Of_Cards;
