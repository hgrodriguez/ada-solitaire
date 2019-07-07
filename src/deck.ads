-- represents a deck of standard poker cards with the standard
-- suits and ranks

package Deck is

   type Suit_Type is (Diamond,
                      Club,
                      Heart,
                      Spade);

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
                      King);

   -- some functions to check attributes
   function Is_Red (a_suit : Suit_Type) return Boolean;
   function Is_Black (a_suit : Suit_Type) return Boolean;

end Deck;
