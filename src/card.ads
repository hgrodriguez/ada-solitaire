with Deck;

-- Represents one card in a deck of (poker) cards
package Card is
   type Card_Type is tagged private;
   
   -- Create one card
   function Ctor(Rank : in Deck.Rank_Type;
                 Suit : in Deck.Suit_Type)
                 return Card_Type;

   -- Getters for attributes of a card
   function Get_Rank (A_Card : Card_Type) return Deck.Rank_Type;
   function Get_Suit (A_Card : Card_Type) return Deck.Suit_Type;
   
private
   type Card_Type is tagged
      record
         Rank : Deck.Rank_Type;
         Suit : Deck.Suit_Type;
      end record;
   

end Card;
