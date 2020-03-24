--  Represents one card in a deck of (poker) cards

with Deck;

package Card is
   type Card_Type is tagged private;

   --------------------------------------------------------------------
   --  short ASCII representation of a Card_Type
   --  1 = Rank
   --  2 = Suit
   subtype Short_Image_Type is String (1 .. 2);

   Empty_Short_Image   : constant Short_Image_Type := "  ";
   Obscure_Short_Image : constant Short_Image_Type := "XX";

   --------------------------------------------------------------------
   --  Create one card
   function Construct (Rank : Deck.Rank_Type;
                       Suit : Deck.Suit_Type)
                       return Card_Type;
   function Construct_Top_Rank (Suit : Deck.Suit_Type)
                                return Card_Type;

   --------------------------------------------------------------------
   --  Getters for attributes of a card
   function Get_Rank (A_Card : Card_Type) return Deck.Rank_Type;
   function Get_Suit (A_Card : Card_Type) return Deck.Suit_Type;

   --------------------------------------------------------------------
   --  rank comparison functions
   --  semantics: c1.Rank = c2.Rank
   function Rank_Is_Equal_To (c1 : Card_Type;
                              c2 : Card_Type)
                              return Boolean;
   --  semantics: c1.Rank > c2.Rank
   function Rank_Is_Higher_Than (c1 : Card_Type;
                                 c2 : Card_Type)
                                 return Boolean;
   --  semantics: c1.Rank < c2.Rank
   function Rank_Is_Lower_Than (c1 : Card_Type;
                                c2 : Card_Type)
                                return Boolean;

   --------------------------------------------------------------------
   --  suit comparison functions
   --  semantics: c1.Suit = c2.Suit
   function Suit_Is_Equal_To (c1 : Card_Type;
                              c2 : Card_Type)
                              return Boolean;

   --------------------------------------------------------------------
   --  colour checks
   function Suit_Is_Red (c : Card_Type) return Boolean;
   function Suit_Is_Black (c : Card_Type) return Boolean;

   --------------------------------------------------------------------
   --  card comparison functions
   --  semantics: c1.Rank = c2.Rank AND c1.Suit = c2.Suit
   function Is_Equal_To (c1 : Card_Type; c2 : Card_Type) return Boolean;
   function "=" (Left, Right : Card_Type) return Boolean;

   --------------------------------------------------------------------
   --  images of a Card_Type
   --  short image of a Card_Type: RS
   function Short_Image (c : Card_Type) return Short_Image_Type;
   --  the representation with parantheses for debugging etc.
   function Image (c : Card_Type) return String;

private
   type Card_Type is tagged
      record
         Rank : Deck.Rank_Type;
         Suit : Deck.Suit_Type;
      end record;

end Card;
