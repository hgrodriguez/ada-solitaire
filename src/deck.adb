
with Definitions; use Definitions;

package body Deck is

   --------------------------------------------------------------------
   --
   function Is_Red (Suit : Definitions.Suit) return Boolean is
   begin
      return Suit = Definitions.Diamond or else Suit = Definitions.Heart;
   end Is_Red;

   --------------------------------------------------------------------
   --
   function Is_Black (Suit : Definitions.Suit) return Boolean is
   begin
      return Suit = Definitions.Club or else Suit = Definitions.Spade;
   end Is_Black;

   --------------------------------------------------------------------
   --
   function Short_Image (Rank : Definitions.Ranks_Valid_Range)
                         return Short_Image_Rank_Type is
      type Short_Ranks_Type is array (Definitions.Ranks_Valid_Range)
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

   begin
      return Short_Ranks (Rank);
   end Short_Image;

   --------------------------------------------------------------------
   --
   function Short_Image (Suit : Definitions.Suits_Valid_Range)
                         return Short_Image_Suit_Type is
      type Short_Suits_Type is array (Definitions.Suits_Valid_Range)
        of Short_Image_Suit_Type;
      Short_Suits : constant Short_Suits_Type
        := (Diamond => "D",
            Heart => "H",
            Club => "C",
            Spade => "S");

   begin
      return Short_Suits (Suit);
   end Short_Image;

   --------------------------------------------------------------------
   --
   function Is_Valid_Rank_Short_Image (SIRT : Short_Image_Rank_Type)
                                       return Boolean is
   begin
      if SIRT = "A" then
         return True;
      elsif SIRT = "2" then
         return True;
      elsif SIRT = "3" then
         return True;
      elsif SIRT = "4" then
         return True;
      elsif SIRT = "5" then
         return True;
      elsif SIRT = "6" then
         return True;
      elsif SIRT = "7" then
         return True;
      elsif SIRT = "8" then
         return True;
      elsif SIRT = "9" then
         return True;
      elsif SIRT = "T" then
         return True;
      elsif SIRT = "J" then
         return True;
      elsif SIRT = "Q" then
         return True;
      elsif SIRT = "K" then
         return True;
      else
         return False;
      end if;
   end Is_Valid_Rank_Short_Image;

   --------------------------------------------------------------------
   --
   function Get_Rank_For_Short_Image (SIRT : Short_Image_Rank_Type)
                                      return Definitions.Ranks_Valid_Range is
      R : Definitions.Ranks_Valid_Range;
   begin
      if SIRT = "A" then
         R := Ace;
      elsif SIRT = "2" then
         R := Two;
      elsif SIRT = "3" then
         R := Three;
      elsif SIRT = "4" then
         R := Four;
      elsif SIRT = "5" then
         R := Five;
      elsif SIRT = "6" then
         R := Six;
      elsif SIRT = "7" then
         R := Seven;
      elsif SIRT = "8" then
         R := Eight;
      elsif SIRT = "9" then
         R := Nine;
      elsif SIRT = "T" then
         R := Ten;
      elsif SIRT = "J" then
         R := Jack;
      elsif SIRT = "Q" then
         R := Queen;
      elsif SIRT = "K" then
         R := King;
      else
         raise Deck_Invalid_Short_Image_Rank_Type;
      end if;
      return R;
   end Get_Rank_For_Short_Image;

   --------------------------------------------------------------------
   --
   function Is_Valid_Suit_Short_Image (SIST : Short_Image_Suit_Type)
                                       return Boolean is
   begin
      if SIST = "D" then
         return True;
      elsif SIST = "H" then
         return True;
      elsif SIST = "C" then
         return True;
      elsif SIST = "S" then
         return True;
      else
         return False;
      end if;
   end Is_Valid_Suit_Short_Image;

   --------------------------------------------------------------------
   --
   function Get_Suit_For_Short_Image (SIST : Short_Image_Suit_Type)
                                      return Definitions.Suits_Valid_Range is
      R : Definitions.Suits_Valid_Range;
   begin
      if SIST = "D" then
         R := Diamond;
      elsif SIST = "H" then
         R := Heart;
      elsif SIST = "C" then
         R := Club;
      elsif SIST = "S" then
         R := Spade;
      else
         raise Deck_Invalid_Short_Image_Suit_Type;
      end if;
      return R;
   end Get_Suit_For_Short_Image;

end Deck;
