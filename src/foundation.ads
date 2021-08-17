with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Definitions;
with Card;
with Foundation_Stack;
with Pile_Of_Cards.FIFO;

package Foundation is

   type Foundation_Type is tagged private;

   --------------------------------------------------------------------
   --  construct a Foundation
   function Construct return Foundation_Type;

   --------------------------------------------------------------------
   --  check, if empty
   function Is_Empty (F : Foundation_Type) return Boolean;

   --------------------------------------------------------------------
   --  determine size/count of cards of Foundation across all suits
   function Size (F : Foundation_Type) return Natural;

   --------------------------------------------------------------------
   --  calculates, which Cards can be accepted by the Foundation
   subtype Acceptable_Type is Pile_Of_Cards.FIFO.Pile_Type_FIFO;
   function Accepts (F : Foundation_Type) return Acceptable_Type;

   --------------------------------------------------------------------
   --  puts a Card onto the Foundation
   --  if wrong card -> not in Accepts set -> exception
   Foundation_Wrong_Card_Exception : exception;
   procedure Put (F : Foundation_Type; c : Card.Card_Type);

   --------------------------------------------------------------------
   --  converts the Foundation into a readable form
   COUNT_OF_STACKS : constant Integer := 4;
   WIDTH_ONE_STACK_IMAGE : constant Integer := 3;
   subtype To_String_Type is String (1 .. COUNT_OF_STACKS
                                     * WIDTH_ONE_STACK_IMAGE - 1);
   function To_String (F : Foundation_Type) return To_String_Type;

   --------------------------------------------------------------------
   --  has color coding in it
   function Ansi_To_String (F : Foundation_Type) return Unbounded_String;

private

   type Stack_Array is array (Definitions.Suit)
     of Foundation_Stack.Stack_Type_Access;

   type Foundation_Type is tagged record
      Stacks : Stack_Array;
   end record;

end Foundation;
