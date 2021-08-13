package body Pile_Of_Cards.LIFO is

   --------------------------------------------------------------------
   --
   overriding
   function Construct return Pile_Type_LIFO is
      pile : Pile_Type_LIFO;
   begin
      return pile;
   end Construct;

   --------------------------------------------------------------------
   --
   overriding
   function Pop (Pile : in out Pile_Type_LIFO) return Card.Card_Type is
   begin
      --  re-use base class implementation
      return Pile_Of_Cards.Pile_Type (Pile).Pop;
   end Pop;

   --------------------------------------------------------------------
   --
   overriding
   procedure Push (Pile : in out Pile_Type_LIFO; C : Card.Card_Type) is
   begin
      --  re-use base class implementation
      Pile_Of_Cards.Pile_Type (Pile).Push (C);
   end Push;

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

   --------------------------------------------------------------------
   --
   function Peek_Bottom (Pile : Pile_Type_LIFO) return Card.Card_Type is
      r : Card.Card_Type;
   begin
      if Pile.Is_Empty then
         raise Pile_Empty_Exception
           with "Pile is empty, no Peek_Bottom possible";
      end if;

      r := Pile.Cards.Last_Element;
      return r;
   end Peek_Bottom;

end Pile_Of_Cards.LIFO;
