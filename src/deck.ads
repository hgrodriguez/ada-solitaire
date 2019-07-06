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
   
   function Is_Red (a_suit : Suit_Type) return Boolean;
   function Is_Black (a_suit : Suit_Type) return Boolean;

end Deck;
