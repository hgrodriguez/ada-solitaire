package body Pile_Of_Cards.FIFO is

   type List_Element is record
      C        : Card.Card_Type;
      Next     : List_Element_Access := null;
      Previous : List_Element_Access := null;
   end record;

   function Construct return Pile_Type_FIFO is
      pile : Pile_Type_FIFO;
   begin
      return pile;
   end Construct;

   --------------------------------------------------------------------
   --  FIFO operations
   function Get (Pile : in out Pile_Type_FIFO) return Card.Card_Type is
      new_tail : List_Element_Access;
      r        : Card.Card_Type;
   begin

      if Pile.Is_Empty then
         raise Pile_Empty_Exception with "Pile is empty, no Get possible";
      end if;

      r := Pile.Tail.all.C;

      if Pile.Size = 1 then
         Pile.Head := null;
         Pile.Tail := null;
      else
         new_tail := Pile.Tail.all.Previous;
         new_tail.all.Next := null;
         Pile.Tail := new_tail;
      end if;

      Pile.Count := Pile.Count - 1;

      return r;
   end Get;

   procedure Put (Pile : in out Pile_Type_FIFO; C : Card.Card_Type) is
      new_head : constant List_Element_Access := new List_Element;
   begin
      new_head.all.C := C;

      if Pile.Is_Empty then
         Pile.Tail := new_head;
      else
         Pile.Head.all.Previous := new_head;
         new_head.all.Next := Pile.Head;
      end if;
      Pile.Head := new_head;
      Pile.Count := Pile.Count + 1;
   end Put;

   function Peek (Pile : Pile_Type_FIFO) return Card.Card_Type is
      ret : Card.Card_Type;
   begin
      if Pile.Is_Empty then
         raise Pile_Empty_Exception with "Pile is empty, no Peek possible";
      end if;
      ret := Pile.Tail.all.C;
      return ret;
   end Peek;

   function Has (Pile : Pile_Type_FIFO;
                 C    : Card.Card_Type) return Boolean is
      Pointer : List_Element_Access := Pile.Head;
      c_test  : Card.Card_Type;
   begin
      if Pile.Is_Empty then
         return False;
      end if;
      while Pointer /= null loop
         c_test := Pointer.all.C;
         if c_test.Is_Equal_To (C) then
            return True;
         end if;
         Pointer := Pointer.all.Next;
      end loop;
      return False;
   end Has;

end Pile_Of_Cards.FIFO;
