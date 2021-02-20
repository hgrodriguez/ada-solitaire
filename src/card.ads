--  Represents one card in a deck of (poker) cards

with Definitions;

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
   function Construct (Rank : Definitions.Rank;
                       Suit : Definitions.Suit)
                       return Card_Type;
   function Construct_Top_Rank (Suit : Definitions.Suit)
                                return Card_Type;

   --------------------------------------------------------------------
   --  Getters for attributes of a card
   function Get_Rank (A_Card : Card_Type) return Definitions.Rank;
   function Get_Suit (A_Card : Card_Type) return Definitions.Suit;

   --------------------------------------------------------------------
   --  images of a Card_Type
   --  short image of a Card_Type: RS
   function Short_Image (c : Card_Type) return Short_Image_Type;
   --  the representation with parantheses for debugging etc.
   function Image (c : Card_Type) return String;

   --------------------------------------------------------------------
   --  colour checks
   function Suit_Is_Red (c : Card_Type) return Boolean;
   function Suit_Is_Black (c : Card_Type) return Boolean;

private
   type Card_Type is tagged
      record
         Rank : Definitions.Rank;
         Suit : Definitions.Suit;
      end record;

end Card;
