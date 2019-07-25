
package body Deck is

   function Is_Red (a_suit : Suit_Type) return Boolean is
   begin
      return a_suit = Diamond or else a_suit = Heart;
   end Is_Red;

   function Is_Black (a_suit : Suit_Type) return Boolean is
   begin
      return not Is_Red (a_suit);
   end Is_Black;

end Deck;
