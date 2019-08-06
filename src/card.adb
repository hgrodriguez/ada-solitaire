with Deck; use Deck;

package body Card is

   function Construct (Rank : Deck.Rank_Type;
                       Suit : Deck.Suit_Type)
                       return Card_Type is
   begin
      return (Rank, Suit);
   end Construct;

   function Construct_Top_Rank (Suit : Deck.Suit_Type)
                                return Card_Type is
   begin
      return (Deck.Top, Suit);
   end Construct_Top_Rank;

   function Get_Rank (A_Card : Card_Type) return Deck.Rank_Type is
   begin
      return A_Card.Rank;
   end Get_Rank;

   function Get_Suit (A_Card : Card_Type) return Deck.Suit_Type is
   begin
      return A_Card.Suit;
   end Get_Suit;

   function Rank_Is_Equal_To (c1 : Card_Type; c2 : Card_Type) return Boolean is
   begin
      return c1.Get_Rank = c2.Get_Rank;
   end Rank_Is_Equal_To;

   function Rank_Is_Higher_Than (c1 : Card_Type;
                                 c2 : Card_Type)
                                 return Boolean is
   begin
      return c1.Get_Rank > c2.Get_Rank;
   end Rank_Is_Higher_Than;

   function Rank_Is_Lower_Than (c1 : Card_Type;
                                c2 : Card_Type)
                                return Boolean is
   begin
      return c1.Rank < c2.Rank;
   end Rank_Is_Lower_Than;

   function Suit_Is_Equal_To (c1 : Card_Type;
                              c2 : Card_Type)
                              return Boolean is
   begin
      return c1.Get_Suit = c2.Get_Suit;
   end Suit_Is_Equal_To;

   function Suit_Is_Red (c : Card_Type) return Boolean is
   begin
      return Deck.Is_Red (c.Get_Suit);
   end Suit_Is_Red;

   function Is_Equal_To (c1 : Card_Type; c2 : Card_Type) return Boolean is
   begin
      return c1.Rank_Is_Equal_To (c2) and c1.Suit_Is_Equal_To (c2);
   end Is_Equal_To;

   function Image (c : Card_Type) return String is
   begin
      return "(" & c.Rank'Image & "," & c.Suit'Image & ")";
   end Image;

   function Short_Image (c : Card_Type) return Short_Image_Type is
   begin
      return Deck.Short_Ranks (c.Get_Rank) & Deck.Short_Suits (c.Get_Suit);
   end Short_Image;

end Card;
