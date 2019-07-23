package body Pile_Of_Cards is

   type List_Element is record
      C        : Card.Card_Type;
      Next     : List_Element_Access := null;
      Previous : List_Element_Access := null;
   end record;

   function Construct return Pile_Type is
      pile : Pile_Type;
   begin
      return pile;
   end Construct;

   function Is_Empty (pile : in Pile_Type) return Boolean is
   begin
      return pile.Count = 0;
   end Is_Empty;

   function Size (pile : in Pile_Type) return Natural is
   begin
      return pile.Count;
   end Size;

   --------------------------------------------------------------------
   -- FIFO operations
   procedure Put (pile : in out Pile_Type; c : in Card.Card_Type) is
      new_head : List_Element_Access := new List_Element;
   begin
      new_head.C := c;

      if pile.Is_Empty then
         pile.Tail := new_head;
      else
         pile.Head.Previous := new_head;
         new_head.Next := pile.Head;
      end if;
      pile.Head := new_head;
      pile.Count := pile.Count + 1;
   end Put;




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
--     V := VB.all;  -- VB is not null, access to a dangling pointer is erroneous
--
--  end Deallocation_Sample;


end Pile_Of_Cards;
