package body Pile_Of_Cards is

   --------------------------------------------------------------------
   --
   function Construct return Pile_Type is
      Pile : Pile_Type;
   begin
      return Pile;
   end Construct;

   --------------------------------------------------------------------
   --
   function Is_Empty (Pile : Pile_Type) return Boolean is
   begin
      return Pile.Cards.Is_Empty;
   end Is_Empty;

   --------------------------------------------------------------------
   --
   function Size (Pile : Pile_Type) return Natural is
   begin
      return Natural (Pile.Cards.Length);
   end Size;

   --------------------------------------------------------------------
   --
   function Has (Pile : Pile_Type;
                 C    : Card.Card_Type) return Boolean is
      use Card_Type_DLL;
   begin
      return Pile.Cards.Find (Item => C) /= Card_Type_DLL.No_Element;
   end Has;

   --------------------------------------------------------------------
   --
   function Peek (Pile : Pile_Type) return Card.Card_Type is
   begin
      if Pile.Is_Empty then
         raise Pile_Empty_Exception with "Pile is empty, no Peek possible";
      end if;
      return Pile.Cards.First_Element;
   end Peek;

   --------------------------------------------------------------------
   --
--     function Duplicate (pile : Pile_Type) return Pile_Type is
--        pile :
--     begin
--
--     end Duplicate;

   --------------------------------------------------------------------
   --  PRIVATE PROCEDURES/FUNCTIONS
   --------------------------------------------------------------------

   --------------------------------------------------------------------
   --
   function Get (Pile : in out Pile_Type) return Card.Card_Type is
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
   procedure Put (Pile : in out Pile_Type; C : Card.Card_Type) is
   begin
      Pile.Cards.Append (C);
   end Put;

   --------------------------------------------------------------------
   --
   procedure Push (pile : in out Pile_Type; c : Card.Card_Type) is
   begin
      pile.Cards.Prepend (c);
   end Push;

   --------------------------------------------------------------------
   --
   function Pop (pile : in out Pile_Type) return Card.Card_Type is
      r       : Card.Card_Type;
   begin
      if pile.Is_Empty then
         raise Pile_Empty_Exception with "Pile is empty, no Pop possible";
      end if;

      r := pile.Cards.First_Element;
      pile.Cards.Delete_First;
      return r;
   end Pop;

   --  with Ada.Unchecked_Deallocation;
--
--  procedure Deallocation_Sample is
--
--     type Vector     is array (Integer range <>) of Float;
--     type Vector_Ref is access Vector;
--
--     procedure Free_Vector is new Ada.Unchecked_Deallocation
--        (Object => Vector, Name => Vector_Ref);
--
--     VA, VB: Vector_Ref;
--     V     : Vector;
--
--  begin
--
--     VA     := new Vector (1 .. 10);
--     VB     := VA;  -- points to the same location as VA
--
--     VA.all := (others => 0.0);
--
--     --  ... Do whatever you need to do with the vector
--
--     Free_Vector (VA); -- The memory is deallocated and VA is now null
--
--     V := VB.all;  -- VB is not null,
--   access to a dangling pointer is erroneous
--
--  end Deallocation_Sample;

end Pile_Of_Cards;
