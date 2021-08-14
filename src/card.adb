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
   function Rank_Is_Equal_To (c1 : Card_Type; c2 : Card_Type) return Boolean is
      use Definitions;
   begin
      return c1.Get_Rank = c2.Get_Rank;
   end Rank_Is_Equal_To;

   function Rank_Is_Higher_Than (c1 : Card_Type;
                                 c2 : Card_Type)
                                 return Boolean is
      use Definitions;
   begin
      return c1.Get_Rank > c2.Get_Rank;
   end Rank_Is_Higher_Than;

   function Rank_Is_Lower_Than (c1 : Card_Type;
                                c2 : Card_Type)
                                return Boolean is
      use Definitions;
   begin
      return c1.Rank < c2.Rank;
   end Rank_Is_Lower_Than;

   function Suit_Is_Equal_To (c1 : Card_Type;
                              c2 : Card_Type)
                              return Boolean is
      use Definitions;
   begin
      return c1.Get_Suit = c2.Get_Suit;
   end Suit_Is_Equal_To;

   function Suit_Is_Red (c : Card_Type) return Boolean is
   begin
      return Deck.Is_Red (c.Get_Suit);
   end Suit_Is_Red;

   function Suit_Is_Black (c : Card_Type) return Boolean is
   begin
      return Deck.Is_Black (c.Get_Suit);
   end Suit_Is_Black;

   function Is_Equal_To (c1 : Card_Type; c2 : Card_Type) return Boolean is
   begin
      return c1 = c2;
   end Is_Equal_To;

   function "=" (Left, Right : Card_Type) return Boolean is
   begin
      return Left.Rank_Is_Equal_To (Right) and Left.Suit_Is_Equal_To (Right);
   end "=";

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
