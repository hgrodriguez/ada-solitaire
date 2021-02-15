package body Foundation is

   Foundation_Has_Been_Created : Boolean := False;
   The_Foundation : Foundation_Type;

   --------------------------------------------------------------------
   --
   function Construct return Foundation_Type is
      ST_Diamond : Foundation_Stack.Stack_Type_Access;
      ST_Club    : Foundation_Stack.Stack_Type_Access;
      ST_Heart   : Foundation_Stack.Stack_Type_Access;
      ST_Spade   : Foundation_Stack.Stack_Type_Access;
   begin
      if not Foundation_Has_Been_Created then
         ST_Diamond := new Foundation_Stack.Stack_Type;
         ST_Diamond.all := Foundation_Stack.Construct (Definitions.Diamond);
         The_Foundation.Stacks (Definitions.Diamond) := ST_Diamond;

         ST_Club    := new Foundation_Stack.Stack_Type;
         ST_Club.all    := Foundation_Stack.Construct (Definitions.Club);
         The_Foundation.Stacks (Definitions.Club)    := ST_Club;

         ST_Heart   := new Foundation_Stack.Stack_Type;
         ST_Heart.all   := Foundation_Stack.Construct (Definitions.Heart);
         The_Foundation.Stacks (Definitions.Heart)   := ST_Heart;

         ST_Spade   := new Foundation_Stack.Stack_Type;
         ST_Spade.all   := Foundation_Stack.Construct (Definitions.Spade);
         The_Foundation.Stacks (Definitions.Spade)   := ST_Spade;

         Foundation_Has_Been_Created := False;
      end if;
      return The_Foundation;
   end Construct;

   --------------------------------------------------------------------
   --
   function Accepts (F : Foundation_Type) return Acceptable_Type is
      ret : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
   begin
      for suit in Definitions.Suits_Valid_Range loop
         ret.Put (F.Stacks (suit).Accepts);
      end loop;
      return ret;
   end Accepts;

   --------------------------------------------------------------------
   --
   function Is_Empty (F : Foundation_Type) return Boolean is
   begin
      return F.Size = 0;
   end Is_Empty;

   --------------------------------------------------------------------
   --
   function Size (F : Foundation_Type) return Natural is
      S : Natural := 0;
   begin
      for suit in Definitions.Suits_Valid_Range loop
         S := S + F.Stacks (suit).Size;
      end loop;
      return S;
   end Size;

   --------------------------------------------------------------------
   --
   procedure Put (F : Foundation_Type; c : Card.Card_Type) is
   begin
      F.Stacks (c.Get_Suit).Push (c);
   exception
      when Foundation_Stack.Wrong_Rank_Exception
         => raise Foundation_Wrong_Card_Exception;
   end Put;

   --------------------------------------------------------------------
   --
   function To_String (F : Foundation_Type) return To_String_Type is
      R     : To_String_Type := "           ";
      J     : Integer := 1;
      T     : Card.Short_Image_Type;
      Stack : Foundation_Stack.Stack_Type_Access;
      use Definitions;
   begin
      for s in Definitions.Suits_Valid_Range loop
         Stack := F.Stacks (s);
         T := Stack.all.To_String;
         R (J .. J + 1) := T;
         if s < Definitions.Suits_Valid_Range'Last then
            --  add separator
            R (J + 2) := ' ';
         end if;
         J := J + 3;
      end loop;
      return R;
   end To_String;

end Foundation;
