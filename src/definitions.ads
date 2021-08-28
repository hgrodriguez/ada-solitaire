--  ANSI CODES:
--  https://en.wikipedia.org/wiki/ANSI_escape_code#3-bit_and_4-bit
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

   Ansi_Red_Head : String := ASCII.ESC & "[31m";
   Ansi_Red_Tail : String := ASCII.ESC & "[0m";
end Definitions;
