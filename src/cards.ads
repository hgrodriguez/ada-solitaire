with Card;

package Cards is

   --------------------------------------------------------------------
   --  rank comparison functions
   --  semantics: c1.Rank = c2.Rank
   function Rank_Is_Equal_To (c1 : Card.Card_Type;
                              c2 : Card.Card_Type)
                              return Boolean;
   --  semantics: c1.Rank > c2.Rank
   function Rank_Is_Higher_Than (c1 : Card.Card_Type;
                                 c2 : Card.Card_Type)
                                 return Boolean;
   --  semantics: c1.Rank < c2.Rank
   function Rank_Is_Lower_Than (c1 : Card.Card_Type;
                                c2 : Card.Card_Type)
                                return Boolean;

   --------------------------------------------------------------------
   --  suit comparison functions
   --  semantics: c1.Suit = c2.Suit
   function Suit_Is_Equal_To (c1 : Card.Card_Type;
                              c2 : Card.Card_Type)
                              return Boolean;

   --------------------------------------------------------------------
   --  card comparison functions
   --  semantics: c1.Rank = c2.Rank AND c1.Suit = c2.Suit
   function Is_Equal_To (c1 : Card.Card_Type;
                         c2 : Card.Card_Type) return Boolean;
   function "=" (Left, Right : Card.Card_Type) return Boolean;

end Cards;
