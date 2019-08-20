
package body Deck is
   function Is_Red (a_suit : Suit_Type) return Boolean is
   begin
      return a_suit = Diamond or else a_suit = Heart;
   end Is_Red;

   function Is_Black (a_suit : Suit_Type) return Boolean is
   begin
      return not Is_Red (a_suit);
   end Is_Black;

   function Short_Image (R : Rank_Type_Valid_Range)
                         return Short_Image_Rank_Type is
      Short_Ranks : constant array (Rank_Type_Valid_Range)
        of Short_Image_Rank_Type
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
   begin
      return Short_Ranks (R);
   end Short_Image;

   function Short_Image (S : Suit_Type_Valid_Range)
                         return Short_Image_Suit_Type is
      Short_Suits : constant array (Suit_Type_Valid_Range)
        of Short_Image_Suit_Type
          := (Diamond => "D",
              Heart => "H",
              Club => "C",
              Spade => "S");
   begin
      return Short_Suits (S);
   end Short_Image;

   function Get_Rank_For_Short_Image (SI : Short_Image_Rank_Type)
                                      return Rank_Type_Valid_Range is
      R : Rank_Type_Valid_Range;
   begin
      if SI = "A" then
         R := Ace;
      elsif SI = "2" then
         R := Two;
      elsif SI = "3" then
         R := Three;
      elsif SI = "4" then
         R := Four;
      elsif SI = "5" then
         R := Five;
      elsif SI = "6" then
         R := Six;
      elsif SI = "7" then
         R := Seven;
      elsif SI = "8" then
         R := Eight;
      elsif SI = "9" then
         R := Nine;
      elsif SI = "T" then
         R := Ten;
      elsif SI = "J" then
         R := Jack;
      elsif SI = "Q" then
         R := Queen;
      elsif SI = "K" then
         R := King;
      else
         raise Deck_Invalid_Short_Image_Rank_Type;
      end if;
      return R;
   end Get_Rank_For_Short_Image;

   function Get_Suit_For_Short_Image (SI : Short_Image_Suit_Type)
                                      return Suit_Type_Valid_Range is
      R : Suit_Type_Valid_Range;
   begin
      if SI = "D" then
         R := Diamond;
      elsif SI = "H" then
         R := Heart;
      elsif SI = "C" then
         R := Club;
      elsif SI = "S" then
         R := Spade;
      else
         raise Deck_Invalid_Short_Image_Suit_Type;
      end if;
      return R;
   end Get_Suit_For_Short_Image;

end Deck;
