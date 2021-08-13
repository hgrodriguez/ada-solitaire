package body Pile_Of_Cards.FIFO is

   --------------------------------------------------------------------
   --
   overriding
   function Construct return Pile_Type_FIFO is
      pile : Pile_Type_FIFO;
   begin
      return pile;
   end Construct;

   --------------------------------------------------------------------
   --
   overriding
   function Get (Pile : in out Pile_Type_FIFO) return Card.Card_Type is
   begin
      --  re-use base class implementation
      return Pile_Of_Cards.Pile_Type (Pile).Get;
   end Get;

   --------------------------------------------------------------------
   --
   overriding
   procedure Put (Pile : in out Pile_Type_FIFO; C : Card.Card_Type) is
   begin
      --  re-use base class implementation
      Pile_Of_Cards.Pile_Type (Pile).Put (C);
   end Put;

end Pile_Of_Cards.FIFO;
