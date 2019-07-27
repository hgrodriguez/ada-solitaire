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

   type Suit_Type is (Below,
                      Diamond,
                      Club,
                      Heart,
                      Spade,
                      Above);

   subtype Suit_Type_Valid_Range is
      Suit_Type range Diamond .. Spade;

   --  some functions to check attributes
   function Is_Red (a_suit : Suit_Type) return Boolean;
   function Is_Black (a_suit : Suit_Type) return Boolean;

end Deck;
