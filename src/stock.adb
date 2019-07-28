with Ada.Numerics.Discrete_Random;

with Card;
with Deck;

package body Stock is

   package Random_Suit is
     new Ada.Numerics.Discrete_Random (Deck.Suit_Type_Valid_Range);
   package Random_Rank is
     new Ada.Numerics.Discrete_Random (Deck.Rank_Type_Valid_Range);

   function Construct return Stock_Type is
      Card_Is_Available : array (Deck.Rank_Type, Deck.Suit_Type)
                            of Boolean;
      S                 : Stock.Stock_Type;
      G_Suit            : Random_Suit.Generator;
      G_Rank            : Random_Rank.Generator;
      R_Suit            : Deck.Suit_Type; --  random suit
      R_Rank            : Deck.Rank_Type; --  random rank

      procedure Cards_Initialize;
      procedure Cards_Initialize is
      begin
         for rank in Deck.Rank_Type_Valid_Range loop
            for suit in Deck.Suit_Type_Valid_Range loop
               Card_Is_Available (rank, suit) := True;
            end loop;
         end loop;
      end Cards_Initialize;

      function Cards_Left return Integer;
      function Cards_Left return Integer is
         Result : Integer;
      begin
         Result := 0;
         for rank in Deck.Rank_Type_Valid_Range loop
            for suit in Deck.Suit_Type_Valid_Range loop
               if Card_Is_Available (rank, suit) then
                  Result := Result + 1;
               end if;
            end loop;
         end loop;
         return Result;
      end Cards_Left;

   begin
      S.Pile := Pile_Of_Cards_FIFO.Construct;

      Cards_Initialize;

      while Cards_Left > 0 loop
         R_Rank := Random_Rank.Random (G_Rank);
         R_Suit := Random_Suit.Random (G_Suit);
         if Card_Is_Available (R_Rank, R_Suit) then
            declare
               c : Card.Card_Type;
            begin
               c := Card.Construct (Rank => R_Rank,
                                    Suit => R_Suit);
               S.Pile.Put (c);
            end;

            Card_Is_Available (R_Rank, R_Suit) := False;
         end if;

      end loop;

      return S;
   end Construct;

   function Size (S : Stock_Type) return Natural is
   begin
      return S.Pile.Size;
   end Size;

   --------------------------------------------------------------------
   --  PRIVATE Procedures/Functions
   procedure Shuffle (S : in out Stock_Type) is
   begin
      null;
   end Shuffle;

end Stock;
