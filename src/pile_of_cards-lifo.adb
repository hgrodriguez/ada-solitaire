package body Pile_Of_Cards.LIFO is

   type List_Element is record
      C        : Card.Card_Type;
      Next     : List_Element_Access := null;
   end record;

   function Construct return Pile_Type_LIFO is
      pile : Pile_Type_LIFO;
   begin
      return pile;
   end Construct;

   function Pop (pile : in out Pile_Type_LIFO) return Card.Card_Type is
      new_tos : List_Element_Access;
      r       : Card.Card_Type;
   begin
      if pile.Is_Empty then
         raise Pile_Empty_Exception with "Pile is empty, no Get possible";
      end if;

      r := pile.TOS.all.C;

      if pile.Size = 1 then
         pile.TOS := null;
      else
         new_tos := pile.TOS.all.Next;
         pile.TOS := new_tos;
      end if;

      pile.Count := pile.Count - 1;

      return r;
   end Pop;

   procedure Push (pile : in out Pile_Type_LIFO; c : Card.Card_Type) is
      new_tos : constant List_Element_Access := new List_Element;
   begin
      new_tos.all.C := c;

      if not pile.Is_Empty then
         new_tos.all.Next := pile.TOS;
      end if;
      pile.TOS := new_tos;
      pile.Count := pile.Count + 1;
   end Push;

end Pile_Of_Cards.LIFO;