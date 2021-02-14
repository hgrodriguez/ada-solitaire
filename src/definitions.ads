package Definitions is

   --------------------------------------------------------------------
   --  all rank related declarations
   type Rank is (Ace,
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

   subtype Ranks_Valid_Range is Rank range Ace .. King;

   --------------------------------------------------------------------
   --  all suit related declarations
   type Suit is (Diamond,
                 Heart,
                 Club,
                 Spade);
   subtype Suits_Red is Suit range Diamond .. Heart;
   subtype Suits_Black is Suit range Club .. Spade;

   subtype Suits_Valid_Range is Suit range Diamond .. Spade;

end Definitions;
