with Pile_Of_Cards_FIFO;

package Stock is

   type Stock_Type is tagged private;

   function Construct return Stock_Type;

   function Size (S : Stock_Type) return Natural;

private
   type Stock_Type is tagged record
      Pile : Pile_Of_Cards_FIFO.Pile_Type_FIFO;
   end record;

   procedure Shuffle (S : in out Stock_Type);

end Stock;
