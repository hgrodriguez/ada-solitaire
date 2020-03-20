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
   begin
      --  re-use base class implementation
      return Pile_Of_Cards.Pile_Type (Pile).Get;
   end Get;

   --------------------------------------------------------------------
   --
   procedure Put (Pile : in out Pile_Type_FIFO; C : Card.Card_Type) is
   begin
      --  re-use base class implementation
      Pile_Of_Cards.Pile_Type (Pile).Put (C);
   end Put;

end Pile_Of_Cards.FIFO;
