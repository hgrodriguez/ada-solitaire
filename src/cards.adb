with Definitions; use Definitions;

package body Cards is

   function Rank_Is_Equal_To (c1 : Card.Card_Type; c2 : Card.Card_Type)
                              return Boolean is
   begin
      return c1.Get_Rank = c2.Get_Rank;
   end Rank_Is_Equal_To;

   function Rank_Is_Higher_Than (c1 : Card.Card_Type;
                                 c2 : Card.Card_Type)
                                 return Boolean is
   begin
      return c1.Get_Rank > c2.Get_Rank;
   end Rank_Is_Higher_Than;

   function Rank_Is_Lower_Than (c1 : Card.Card_Type;
                                c2 : Card.Card_Type)
                                return Boolean is
   begin
      return c1.Get_Rank < c2.Get_Rank;
   end Rank_Is_Lower_Than;

   function Suit_Is_Equal_To (c1 : Card.Card_Type;
                              c2 : Card.Card_Type)
                              return Boolean is
   begin
      return c1.Get_Suit = c2.Get_Suit;
   end Suit_Is_Equal_To;

   function Is_Equal_To (c1 : Card.Card_Type;
                         c2 : Card.Card_Type) return Boolean is
   begin
      return c1 = c2;
   end Is_Equal_To;

   function "=" (Left, Right : Card.Card_Type) return Boolean is
   begin
      return Rank_Is_Equal_To (Left, Right) and Suit_Is_Equal_To (Left, Right);
   end "=";

end Cards;
