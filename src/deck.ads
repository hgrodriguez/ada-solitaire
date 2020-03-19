--  represents a deck of standard poker cards with the standard
--  suits and ranks

package Deck is

   --------------------------------------------------------------------
   --  all rank related declarations
   type Rank_Type is (Ace,
                      Two,
                      Three,
                      Four,
                      Five,
                      Six,
                      Seven,
                      Eight,
                      Nine,
                      Ten,
                      Jack,
                      Queen,
                      King,
                      Top);

   subtype Rank_Type_Valid_Range is Rank_Type range Ace .. King;

   --------------------------------------------------------------------
   --  all suit related declarations
   type Suit_Type is (Diamond,
                      Heart,
                      Club,
                      Spade);
   subtype Suit_Type_Red is Suit_Type range Diamond .. Heart;
   subtype Suit_Type_Black is Suit_Type range Club .. Spade;

   subtype Suit_Type_Valid_Range is Suit_Type range Diamond .. Spade;

   --------------------------------------------------------------------
   --  check for Red/Black colour
   function Is_Red (Suit : Suit_Type) return Boolean;
   function Is_Black (Suit : Suit_Type) return Boolean;

   --------------------------------------------------------------------
   --  convert Rank/Suit into short images
   type Short_Image_Rank_Type is new String (1 .. 1);
   function Short_Image (Rank : Rank_Type_Valid_Range)
                         return Short_Image_Rank_Type;

   type Short_Image_Suit_Type is new String (1 .. 1);
   function Short_Image (Suit : Suit_Type_Valid_Range)
                         return Short_Image_Suit_Type;

   --------------------------------------------------------------------
   --  check if Short_Image_Rank_Type is a valid Rank_Type
   function Is_Valid_Rank_Short_Image (SIRT : Short_Image_Rank_Type)
                                       return Boolean;

   --------------------------------------------------------------------
   --  check if Short_Image_Suit_Type is a valid Suit_Type
   function Is_Valid_Suit_Short_Image (SIST : Short_Image_Suit_Type)
                                       return Boolean;

   --------------------------------------------------------------------
   --  convert a Short_Image_Rank_Type into a Rank_Type
   Deck_Invalid_Short_Image_Rank_Type : exception;
   function Get_Rank_For_Short_Image (SIRT : Short_Image_Rank_Type)
                                      return Rank_Type_Valid_Range;

   --------------------------------------------------------------------
   --  convert a Short_Image_Suit_Type into a Suit_Type
   Deck_Invalid_Short_Image_Suit_Type : exception;
   function Get_Suit_For_Short_Image (SIST : Short_Image_Suit_Type)
                                      return Suit_Type_Valid_Range;

end Deck;
