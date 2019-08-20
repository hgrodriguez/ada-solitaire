
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
   begin
      return Short_Ranks (R);
   end Short_Image;

   function Short_Image (S : Suit_Type_Valid_Range)
                         return Short_Image_Suit_Type is
   begin
      return Short_Suits (S);
   end Short_Image;

   function Is_Valid_Rank_Short_Image (SI : Short_Image_Rank_Type)
                                       return Boolean is
   begin
      if SI = "A" then
         return True;
      elsif SI = "2" then
         return True;
      elsif SI = "3" then
         return True;
      elsif SI = "4" then
         return True;
      elsif SI = "5" then
         return True;
      elsif SI = "6" then
         return True;
      elsif SI = "7" then
         return True;
      elsif SI = "8" then
         return True;
      elsif SI = "9" then
         return True;
      elsif SI = "T" then
         return True;
      elsif SI = "J" then
         return True;
      elsif SI = "Q" then
         return True;
      elsif SI = "K" then
         return True;
      else
         return False;
      end if;
   end Is_Valid_Rank_Short_Image;

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

   function Is_Valid_Suit_Short_Image (SI : Short_Image_Suit_Type)
                                       return Boolean is
   begin
      if SI = "D" then
         return True;
      elsif SI = "H" then
         return True;
      elsif SI = "C" then
         return True;
      elsif SI = "S" then
         return True;
      else
         return False;
      end if;
   end Is_Valid_Suit_Short_Image;

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
