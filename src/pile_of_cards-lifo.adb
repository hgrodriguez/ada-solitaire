package body Pile_Of_Cards.LIFO is

   --------------------------------------------------------------------
   --
   function Construct return Pile_Type_LIFO is
      pile : Pile_Type_LIFO;
   begin
      return pile;
   end Construct;

   --------------------------------------------------------------------
   --
   function Pop (pile : in out Pile_Type_LIFO) return Card.Card_Type is
      r       : Card.Card_Type;
   begin
      if pile.Is_Empty then
         raise Pile_Empty_Exception with "Pile is empty, no Pop possible";
      end if;

      r := pile.Cards.First_Element;
      pile.Cards.Delete_First;
      return r;
   end Pop;

   --------------------------------------------------------------------
   --
   procedure Push (pile : in out Pile_Type_LIFO; c : Card.Card_Type) is
   begin
      pile.Cards.Prepend (c);
   end Push;

   function Peek (pile : Pile_Type_LIFO) return Card.Card_Type is
      r       : Card.Card_Type;
   begin
      if pile.Is_Empty then
         raise Pile_Empty_Exception with "Pile is empty, no Peek possible";
      end if;

      r := pile.Cards.First_Element;
      return r;
   end Peek;

   --------------------------------------------------------------------
   --
   function Has (Pile : Pile_Type_LIFO;
                 C    : Card.Card_Type) return Boolean is
      use Card_Type_DLL;
   begin
      return Pile.Cards.Find (Item => C) /= Card_Type_DLL.No_Element;
   end Has;

   --------------------------------------------------------------------
   --
   procedure Collect (Pile : Pile_Type_LIFO;
                      SIF  : in out Short_Image_FIFO.Short_Image_FIFO_Type) is
      Crsr : Card_Type_DLL.Cursor := Pile.Cards.Last;
      C    : Card.Card_Type;
      CSI  : Card.Short_Image_Type;
      use Card_Type_DLL;
   begin
      loop
         exit when Crsr = Card_Type_DLL.No_Element;
         C := Element (Crsr);
         CSI := C.Short_Image;
         SIF.Put (CSI);
         Crsr := Previous (Crsr);
      end loop;
   end Collect;

end Pile_Of_Cards.LIFO;
