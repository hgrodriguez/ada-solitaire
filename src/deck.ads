--  represents a deck of standard poker cards with the standard
--  suits and ranks

with Definitions;

package Deck is

   --------------------------------------------------------------------
   --  check for Red/Black colour
   function Is_Red (Suit : Definitions.Suit) return Boolean;
   function Is_Black (Suit : Definitions.Suit) return Boolean;

   --------------------------------------------------------------------
   --  convert Rank/Suit into short images
   type Short_Image_Rank_Type is new String (1 .. 1);
   function Short_Image (Rank : Definitions.Ranks_Valid_Range)
                         return Short_Image_Rank_Type;

   type Short_Image_Suit_Type is new String (1 .. 1);
   function Short_Image (Suit : Definitions.Suits_Valid_Range)
                         return Short_Image_Suit_Type;

   --------------------------------------------------------------------
   --  check if Short_Image_Rank_Type is a valid Rank_Type
   function Is_Valid_Rank_Short_Image (SIRT : Short_Image_Rank_Type)
                                       return Boolean;
   function Is_Valid_Rank_Short_Image (C : Character)
                                       return Boolean;

   --------------------------------------------------------------------
   --  check if Short_Image_Suit_Type is a valid Suit_Type
   function Is_Valid_Suit_Short_Image (SIST : Short_Image_Suit_Type)
                                       return Boolean;
   function Is_Valid_Suit_Short_Image (C : Character)
                                       return Boolean;

   --------------------------------------------------------------------
   --  convert a Short_Image_Rank_Type into a Rank_Type
   Deck_Invalid_Short_Image_Rank_Type : exception;
   function Get_Rank_For_Short_Image (SIRT : Short_Image_Rank_Type)
                                      return Definitions.Ranks_Valid_Range;
   function Get_Rank_For_Short_Image (C : Character)
                                      return Definitions.Ranks_Valid_Range;

   --------------------------------------------------------------------
   --  convert a Short_Image_Suit_Type into a Suit_Type
   Deck_Invalid_Short_Image_Suit_Type : exception;
   function Get_Suit_For_Short_Image (SIST : Short_Image_Suit_Type)
                                      return Definitions.Suits_Valid_Range;
   function Get_Suit_For_Short_Image (C : Character)
                                      return Definitions.Suits_Valid_Range;

end Deck;
