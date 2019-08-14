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

   --  some functions to check attributes
   function Is_Red (a_suit : Suit_Type) return Boolean;
   function Is_Black (a_suit : Suit_Type) return Boolean;

   --  some functions to get the short images
   function Short_Image (R : Rank_Type_Valid_Range)
                         return Short_Image_Rank_Type;
   function Short_Image (S : Suit_Type_Valid_Range)
                         return Short_Image_Suit_Type;

end Deck;
