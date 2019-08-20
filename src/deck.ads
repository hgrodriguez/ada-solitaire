--  represents a deck of standard poker cards with the standard
--  suits and ranks

package Deck is

   type Rank_Type is (Bottom,
                      Ace,
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

   subtype Rank_Type_Valid_Range is
      Rank_Type range Ace .. King;
   type Short_Image_Rank_Type is new String (1 .. 1);

   type Suit_Type is (Below,
                      Diamond,
                      Heart,
                      Club,
                      Spade,
                      Above);

   subtype Suit_Type_Valid_Range is
     Suit_Type range Diamond .. Spade;
   type Short_Image_Suit_Type is new String (1 .. 1);

   subtype Suit_Type_Red is Suit_Type range Diamond .. Heart;
   subtype Suit_Type_Black is Suit_Type range Club .. Spade;

   Deck_Invalid_Short_Image_Rank_Type : exception;
   Deck_Invalid_Short_Image_Suit_Type : exception;

   --  some functions to check attributes
   function Is_Red (a_suit : Suit_Type) return Boolean;
   function Is_Black (a_suit : Suit_Type) return Boolean;

   --  some functions to get the short images
   function Short_Image (R : Rank_Type_Valid_Range)
                         return Short_Image_Rank_Type;
   function Short_Image (S : Suit_Type_Valid_Range)
                         return Short_Image_Suit_Type;

   type Ranks_Short_Images_Type is new String (1 .. 2 * 13 - 1);
   Ranks_Short_Images : constant Ranks_Short_Images_Type
     := "A 2 3 4 5 6 7 8 9 T J Q K";

   function Is_Valid_Rank_Short_Image (SI : Short_Image_Rank_Type)
                                       return Boolean;
   function Get_Rank_For_Short_Image (SI : Short_Image_Rank_Type)
                                      return Rank_Type_Valid_Range;

   type Suits_Short_Images_Type is new String (1 .. 2 * 4 - 1);
   Suits_Short_Images : constant Suits_Short_Images_Type
     := "D C H S";

   function Is_Valid_Suit_Short_Image (SI : Short_Image_Suit_Type)
                                       return Boolean;
   function Get_Suit_For_Short_Image (SI : Short_Image_Suit_Type)
                                      return Suit_Type_Valid_Range;

private

   type Short_Ranks_Type is array (Rank_Type_Valid_Range)
     of Short_Image_Rank_Type;
   Short_Ranks : constant Short_Ranks_Type
       := (Ace => "A",
           Two  => "2",
           Three => "3",
           Four => "4",
           Five => "5",
           Six => "6",
           Seven => "7",
           Eight => "8",
           Nine => "9",
           Ten => "T",
           Jack => "J",
           Queen => "Q",
           King => "K");

   type Short_Suits_Type is array (Suit_Type_Valid_Range)
     of Short_Image_Suit_Type;
   Short_Suits : constant array (Suit_Type_Valid_Range)
     of Short_Image_Suit_Type
       := (Diamond => "D",
           Heart => "H",
           Club => "C",
           Spade => "S");

end Deck;
