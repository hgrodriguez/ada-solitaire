package body Pile_Of_Cards is

   function Is_Empty (pile : Pile_Type) return Boolean is
   begin
      return pile.Cards.Is_Empty;
   end Is_Empty;

   function Size (pile : Pile_Type) return Ada.Containers.Count_Type is
   begin
      return pile.Cards.Length;
   end Size;

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
