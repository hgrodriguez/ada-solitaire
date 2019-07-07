package body Card is

   function Ctor (Rank : in Deck.Rank_Type;
                  Suit : in Deck.Suit_Type)
                  return Card_Type is
   begin
      return (Rank, Suit);
   end Ctor;
   
   function Get_Rank (A_Card : Card_Type) return Deck.Rank_Type is
   begin
      return A_Card.Rank;
   end Get_Rank;
   
   function Get_Suit (A_Card : Card_Type) return Deck.Suit_Type is
   begin
      return A_Card.Suit;
   end Get_Suit;
   

end Card;
