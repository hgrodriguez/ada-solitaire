package body Pile_Of_Cards.FIFO is

   --------------------------------------------------------------------
   --
   function Construct return Pile_Type_FIFO is
      pile : Pile_Type_FIFO;
   begin
      return pile;
   end Construct;

   --------------------------------------------------------------------
   --
   function Get (Pile : in out Pile_Type_FIFO) return Card.Card_Type is
      r : Card.Card_Type;
   begin
      if Pile.Is_Empty then
         raise Pile_Empty_Exception with "Pile is empty, no Get possible";
      end if;
      r := Pile.Cards.First_Element;
      Pile.Cards.Delete_First;
      return r;
   end Get;

   --------------------------------------------------------------------
   --
   procedure Put (Pile : in out Pile_Type_FIFO; C : Card.Card_Type) is
   begin
      Pile.Cards.Append (C);
   end Put;

   function Peek (Pile : Pile_Type_FIFO) return Card.Card_Type is
   begin
      if Pile.Is_Empty then
         raise Pile_Empty_Exception with "Pile is empty, no Peek possible";
      end if;
      return Pile.Cards.First_Element;
   end Peek;

   --------------------------------------------------------------------
   --
   function Has (Pile : Pile_Type_FIFO;
                 C    : Card.Card_Type) return Boolean is
      use Card_Type_DLL;
   begin
      return Pile.Cards.Find (Item => C) /= Card_Type_DLL.No_Element;
   end Has;

end Pile_Of_Cards.FIFO;
