with AUnit.Assertions;
with AUnit.Test_Caller;
with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

with Deck; use Deck;
with Card;

package body Foundation_Stack.Test is


   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   --------------------------------------------------------------------
   -- constructors
   procedure Test_Construct_Diamond(T : in out Test) is
      pragma Unreferenced (T);
      suit  : Deck.Suit_Type := Deck.Diamond;
      stack : Foundation_Stack.Stack_Type;
   begin
      stack := Foundation_Stack.Construct (suit);
      AUNit.Assertions.Assert (stack.Get_Suit = suit,
                               "suit=" & suit'Image &
                               " /= " & stack.Get_Suit'Image);
   end Test_Construct_Diamond;

   procedure Test_Construct_Club(T : in out Test) is
      pragma Unreferenced (T);
      suit  : Deck.Suit_Type := Deck.Club;
      stack : Foundation_Stack.Stack_Type;
   begin
      stack := Foundation_Stack.Construct (suit);
      AUNit.Assertions.Assert (stack.Get_Suit = suit,
                               "suit=" & suit'Image &
                               " /= " & stack.Get_Suit'Image);
   end Test_Construct_Club;

   procedure Test_Construct_Heart(T : in out Test) is
      pragma Unreferenced (T);
      suit  : Deck.Suit_Type := Deck.Heart;
      stack : Foundation_Stack.Stack_Type;
   begin
      stack := Foundation_Stack.Construct (suit);
      AUNit.Assertions.Assert (stack.Get_Suit = suit,
                               "suit=" & suit'Image &
                               " /= " & stack.Get_Suit'Image);
   end Test_Construct_Heart;

   procedure Test_Construct_Spade(T : in out Test) is
      pragma Unreferenced (T);
      suit  : Deck.Suit_Type := Deck.Spade;
      stack : Foundation_Stack.Stack_Type;
   begin
      stack := Foundation_Stack.Construct (suit);
      AUNit.Assertions.Assert (stack.Get_Suit = suit,
                               "suit=" & suit'Image &
                               " /= " & stack.Get_Suit'Image);
   end Test_Construct_Spade;

   procedure Newly_Constucted_Is_Empty(T : in out Test) is
      pragma Unreferenced (T);
      suit  : Deck.Suit_Type := Deck.Spade;
      stack : Foundation_Stack.Stack_Type;
   begin
      stack := Foundation_Stack.Construct (suit);
      AUNit.Assertions.Assert (stack.Is_Empty,
                               "newly created stack should be empty," &
                                 " but is not");
   end Newly_Constucted_Is_Empty;
   
   procedure Newly_Constucted_Accepts_Ace(T : in out Test) is
      pragma Unreferenced (T);
      suit  : Deck.Suit_Type := Deck.Spade;
      stack : Foundation_Stack.Stack_Type := Foundation_Stack.Construct (suit);
      card  : Card.Card_Type;
   begin
      card := Card.Construct(Deck.Ace, suit);
      AUNit.Assertions.Assert (stack.Accepts = card,
                               "should accept: " & card'Image &
                                 "but accepts: " & stack.Accepts'Image);
   end Newly_Constucted_Accepts_Ace;
   
   
   --------------------------------------------------------------------
   -- the test suit construction
   package Caller is new AUnit.Test_Caller (Foundation_Stack.Test.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
             := new AUnit.Test_Suites.Test_Suite;
   begin
      -- ctor tests
      Ret.Add_Test (Caller.
                    Create("Foundation_Stack.Test_Construct_Diamond",
                           Test_Construct_Diamond'Access));
      Ret.Add_Test (Caller.
                    Create("Foundation_Stack.Test_Construct_Club",
                           Test_Construct_Club'Access));
      Ret.Add_Test (Caller.
                    Create("Foundation_Stack.Test_Construct_Heart",
                           Test_Construct_Heart'Access));
      Ret.Add_Test (Caller.
                    Create("Foundation_Stack.Test_Construct_Spade",
                           Test_Construct_Spade'Access));

      -- check states of stack
      Ret.Add_Test (Caller.
                    Create("Foundation_Stack.Newly_Constucted_Is_Empty",
                           Newly_Constucted_Is_Empty'Access));
      
      Ret.Add_Test (Caller.
                    Create("Foundation_Stack.Newly_Constucted_Accepts_Ace",
                           Newly_Constucted_Accepts_Ace'Access));
      return Ret;
   end Suite;


end Foundation_Stack.Test;
