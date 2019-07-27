with Pile_Of_Cards;

package Stock is

   type Stock_Type is tagged private;

   function Construct return Stock_Type;

   function Size (S : Stock_Type) return Integer;

private
   type Stock_Type is tagged record
      Pile : Pile_Of_Cards.Pile_Type;
   end record;

   procedure Shuffle (S : in out Stock_Type);

end Stock;
