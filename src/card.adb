with Deck;

package body Card is

   function Construct (Rank : Definitions.Rank;
                       Suit : Definitions.Suit)
                       return Card_Type is
   begin
      return (Rank, Suit);
   end Construct;

   function Construct_Top_Rank (Suit : Definitions.Suit)
                                return Card_Type is
   begin
      return Construct (Definitions.Top, Suit);
   end Construct_Top_Rank;

   function Get_Rank (A_Card : Card_Type) return Definitions.Rank is
   begin
      return A_Card.Rank;
   end Get_Rank;

   function Get_Suit (A_Card : Card_Type) return Definitions.Suit is
   begin
      return A_Card.Suit;
   end Get_Suit;

   function Suit_Is_Red (c : Card_Type) return Boolean is
   begin
      return Deck.Is_Red (c.Get_Suit);
   end Suit_Is_Red;

   function Suit_Is_Black (c : Card_Type) return Boolean is
   begin
      return Deck.Is_Black (c.Get_Suit);
   end Suit_Is_Black;

   function Image (c : Card_Type) return String is
   begin
      return "(" & c.Rank'Image & "," & c.Suit'Image & ")";
   end Image;

   function Short_Image (c : Card_Type) return Short_Image_Type is
      R : Short_Image_Type;
   begin
      R (1) := Deck.Short_Image (c.Get_Rank)(1);
      R (2) := Deck.Short_Image (c.Get_Suit)(1);
      return R;
   end Short_Image;

end Card;
