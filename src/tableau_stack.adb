with Definitions;
with Deck;

package body Tableau_Stack is

   --------------------------------------------------------------------
   --
   function Construct (Number : Integer) return Tableau_Stack.Stack_Type is
      Tab : Stack_Type;
      Pile : Pile_Of_Cards.LIFO.Pile_Type_LIFO_Access;
   begin
      Pile := new Pile_Of_Cards.LIFO.Pile_Type_LIFO;
      Pile.all := Pile_Of_Cards.LIFO.Construct;
      Tab.Cards := Pile;
      Tab.Number := Number;
      return Tab;
   end Construct;

   --------------------------------------------------------------------
   --
   function Number (T : Stack_Type) return Integer is
   begin
      return T.Number;
   end Number;

   --------------------------------------------------------------------
   --
   function Size (T : Stack_Type) return Natural is
   begin
      return T.Cards.Size;
   end Size;

   --------------------------------------------------------------------
   --
   function Has (T : Stack_Type; C : Card.Card_Type) return Boolean is
   begin
      return T.Cards.Has (C);
   end Has;

   --------------------------------------------------------------------
   --
   function Is_Empty (T : Stack_Type) return Boolean is
   begin
      return T.Cards.Is_Empty;
   end Is_Empty;

   --------------------------------------------------------------------
   --
   procedure Push_Checked (T : Stack_Type; C : Card.Card_Type) is
      can_accept : constant Acceptable_Type := T.Accepts;
   begin
      if can_accept.Has (C) then
         T.Cards.Push (C);
      else
         raise Tableau_Stack_Wrong_Card_Exception;
      end if;
   end Push_Checked;

   --------------------------------------------------------------------
   --
   procedure Push_Unchecked (T : Stack_Type; C : Card.Card_Type) is
   begin
      T.Cards.Push (C);
   end Push_Unchecked;

   --------------------------------------------------------------------
   --
   function Peek (T : Stack_Type) return Card.Card_Type is
   begin
      return T.Cards.all.Peek;
   exception
      when Pile_Of_Cards.Pile_Empty_Exception
         => raise Tableau_Stack_Empty_Exception;
   end Peek;

   --------------------------------------------------------------------
   --
   function Pop (T : Stack_Type) return Card.Card_Type is
   begin
      return T.Cards.all.Pop;
   exception
      when Pile_Of_Cards.Pile_Empty_Exception
           => raise Tableau_Stack_Empty_Exception;
   end Pop;

   --------------------------------------------------------------------
   --
   function Accepts (T : Stack_Type) return Acceptable_Type is
      Result   : Acceptable_Type;
      C        : Card.Card_Type;
      New_Rank : Definitions.Rank;
      use Definitions;
   begin
      if T.Is_Empty then
         for suit in Definitions.Suits_Valid_Range loop
            Result.Put (Card.Construct (Definitions.King, suit));
         end loop;
      else
         --  investigate
         C := T.Cards.Peek;
         if C.Get_Rank = Definitions.Ace then
            return Result;
         end if;
         New_Rank := Definitions.Rank'Pred (C.Get_Rank);
         if Deck.Is_Red (C.Get_Suit) then
            for suit in Definitions.Suits_Black loop
               Result.Put (Card.Construct (New_Rank, suit));
            end loop;
         else
            for suit in Definitions.Suits_Red loop
               Result.Put (Card.Construct (New_Rank, suit));
            end loop;
         end if;
      end if;
      return Result;
   end Accepts;

   --------------------------------------------------------------------
   --
   function Short_Images (T : Stack_Type)
                          return Short_Image_FIFO.Short_Image_FIFO_Type is
      SIF : Short_Image_FIFO.Short_Image_FIFO_Type
        := Short_Image_FIFO.Construct;
      Cards : constant Pile_Of_Cards.LIFO.Pile_Type_LIFO_Access := T.Cards;
   begin
      if T.Is_Empty then
         SIF.Put (Card.Empty_Short_Image);
      end if;
      Cards.Collect (SIF);
      return SIF;
   end Short_Images;

   --------------------------------------------------------------------
   --
   function Has_King_As_Bottom_Card (T : Stack_Type) return Boolean is
      use Definitions;
   begin
      if T.Size = 0 then
         return False;
      else
         return T.Cards.all.Peek_Bottom.Get_Rank = Definitions.King;
      end if;
   end Has_King_As_Bottom_Card;

end Tableau_Stack;
