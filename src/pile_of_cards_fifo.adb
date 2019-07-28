package body Pile_Of_Cards_FIFO is

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
   function Get (pile : in out Pile_Type_FIFO) return Card.Card_Type is
      new_tail : List_Element_Access;
      r        : Card.Card_Type;
   begin

      if pile.Is_Empty then
         raise Pile_Empty_Exception with "Pile is empty, no Get possible";
      end if;

      r := pile.Tail.all.C;

      if pile.Size = 1 then
         pile.Head := null;
         pile.Tail := null;
      else
         new_tail := pile.Tail.all.Previous;
         new_tail.all.Next := null;
         pile.Tail := new_tail;
      end if;

      pile.Count := pile.Count - 1;

      return r;
   end Get;

   procedure Put (pile : in out Pile_Type_FIFO; c : Card.Card_Type) is
      new_head : constant List_Element_Access := new List_Element;
   begin
      new_head.all.C := c;

      if pile.Is_Empty then
         pile.Tail := new_head;
      else
         pile.Head.all.Previous := new_head;
         new_head.all.Next := pile.Head;
      end if;
      pile.Head := new_head;
      pile.Count := pile.Count + 1;
   end Put;

end Pile_Of_Cards_FIFO;
