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
                      King);

   type Rank_Type_Range is
     new Rank_Type range Rank_Type'First .. Rank_Type'Last;

   type Suit_Type is (Diamond,
                      Club,
                      Heart,
                      Spade);

   type Suit_Type_Range is
     new Suit_Type range Suit_Type'First .. Suit_Type'Last;

   --  some functions to check attributes
   function Is_Red (a_suit : Suit_Type) return Boolean;
   function Is_Black (a_suit : Suit_Type) return Boolean;

end Deck;
