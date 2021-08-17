with Ada.Numerics.Discrete_Random;

with Definitions;

package body Stock is

   --------------------------------------------------------------------
   --
   function Construct return Stock_Type is

      package Random_Rank is
        new Ada.Numerics.Discrete_Random (Definitions.Ranks_Valid_Range);
      package Random_Suit is
        new Ada.Numerics.Discrete_Random (Definitions.Suits_Valid_Range);

      Card_Is_Available : array (Definitions.Rank,
                                 Definitions.Suit) of Boolean;
      S                 : Stock.Stock_Type;
      G_Suit            : Random_Suit.Generator;
      G_Rank            : Random_Rank.Generator;
      R_Suit            : Definitions.Suit; --  random suit
      R_Rank            : Definitions.Rank; --  random rank

      -----------------------------------------------------------------
      --  resets the availability matrix
      procedure Reset_Is_Available;
      procedure Reset_Is_Available is
      begin
         for rank in Definitions.Ranks_Valid_Range loop
            for suit in Definitions.Suits_Valid_Range loop
               Card_Is_Available (rank, suit) := True;
            end loop;
         end loop;
      end Reset_Is_Available;

      -----------------------------------------------------------------
      --  calculate, how many cards are left to be created
      function Cards_Left return Integer;
      function Cards_Left return Integer is
         Result : Integer;
      begin
         Result := 0;
         for rank in Definitions.Ranks_Valid_Range loop
            for suit in Definitions.Suits_Valid_Range loop
               if Card_Is_Available (rank, suit) then
                  Result := Result + 1;
               end if;
            end loop;
         end loop;
         return Result;
      end Cards_Left;

      -----------------------------------------------------------------
      --  initiliazes the stock with random cards
      procedure Initialize_Stock;
      procedure Initialize_Stock is
      begin
         Random_Rank.Reset (G_Rank);
         Random_Suit.Reset (G_Suit);
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

      end Initialize_Stock;

   begin
      S.Pile := new Pile_Of_Cards.FIFO.Pile_Type_FIFO;
      S.Pile.all := Pile_Of_Cards.FIFO.Construct;

      Reset_Is_Available;

      Initialize_Stock;

      return S;
   end Construct;

   --------------------------------------------------------------------
   --
   function Size (S : Stock_Type) return Natural is
   begin
      return S.Pile.Size;
   end Size;

   --------------------------------------------------------------------
   --
   function Fetch (S : Stock_Type) return Pile_Of_Cards.FIFO.Pile_Type_FIFO is
      Size : Natural := S.Size;
      Pile : Pile_Of_Cards.FIFO.Pile_Type_FIFO := Pile_Of_Cards.FIFO.Construct;
      C    : Card.Card_Type;
   begin
      if Size > 7 then
         Size := 7;
      end if;
      while Size > 0 loop
         C := S.Fetch_One;
         Pile.Put (C);
         Size := Size - 1;
      end loop;
      return Pile;
   end Fetch;

   --------------------------------------------------------------------
   --  fetches exactly one card from stock
   function Fetch_One (S : Stock_Type) return Card.Card_Type is
      C    : Card.Card_Type;
   begin
      if S.Size > 0 then
         C := S.Pile.all.Get;
      else
         raise Stock_Empty_Exception;
      end if;
      return C;
   end Fetch_One;

   --------------------------------------------------------------------
   --
   function To_String (S    : Stock_Type;
                       Peek : Boolean := False)
                       return Card.Short_Image_Type is
   begin
      if S.Size = 0 then
         return Card.Empty_Short_Image;
      else
         if Peek then
            return S.Peek.Short_Image;
         else
            return Card.Obscure_Short_Image;
         end if;
      end if;
   end To_String;

   function Ansi_To_String (S : Stock_Type; Peek : Boolean := False)
                            return Unbounded_String is
   begin
      if S.Size = 0 then
         return To_Unbounded_String (Card.Empty_Short_Image);
      else
         if Peek then
            return S.Peek.Ansi_Image;
         else
            return To_Unbounded_String (Card.Obscure_Short_Image);
         end if;
      end if;
   end Ansi_To_String;

   --------------------------------------------------------------------
   --  PRIVATE PROCEDURES/FUNCTIONS
   --------------------------------------------------------------------
   --------------------------------------------------------------------
   --
   function Peek (S : Stock_Type) return Card.Card_Type is
   begin
      if S.Size = 0 then
         raise Stock_Empty_Exception;
      end if;
      return S.Pile.Peek;
   end Peek;

end Stock;
