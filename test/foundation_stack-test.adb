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

   -- check newly constructed attributes
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
      suit   : Deck.Suit_Type := Deck.Spade;
      stack  : Foundation_Stack.Stack_Type := Foundation_Stack.Construct (suit);
      a_card  : Card.Card_Type;
   begin
      a_card := Card.Construct(Deck.Ace, suit);
      AUNit.Assertions.Assert (stack.Accepts.Is_Equal_To (a_card),
                               "should accept: " & a_card.Image &
                                 "but accepts: " & stack.Accepts.Image);
   end Newly_Constucted_Accepts_Ace;
   
   procedure Newly_Constucted_Is_Size_0(T : in out Test) is
      pragma Unreferenced (T);
      suit   : Deck.Suit_Type := Deck.Spade;
      stack  : Foundation_Stack.Stack_Type := Foundation_Stack.Construct (suit);
      a_card  : Card.Card_Type;
   begin
      a_card := Card.Construct(Deck.Ace, suit);
      AUNit.Assertions.Assert (stack.Size = 0,
                               "size should = 0, but is: " & stack.Size'Image);
   end Newly_Constucted_Is_Size_0;
   
   -- check for push operations
   procedure Push_Newly_Constructed_OK_Size (T : in out Test) is
      pragma Unreferenced (T);
      suit   : Deck.Suit_Type := Deck.Spade;
      stack  : Foundation_Stack.Stack_Type := Foundation_Stack.Construct (suit);
      a_card  : Card.Card_Type;
   begin
      a_card := Card.Construct(Deck.Ace, suit);
      stack.Push (a_card);
      AUNit.Assertions.Assert (stack.Size = 1,
                               "size should = 1, but is: " & stack.Size'Image);
   end Push_Newly_Constructed_OK_Size;
   
   procedure Push_Newly_Constructed_OK_Accept (T : in out Test) is
      pragma Unreferenced (T);
      suit            : Deck.Suit_Type := Deck.Spade;
      stack           : Foundation_Stack.Stack_Type :=
        Foundation_Stack.Construct (suit);
      a_card          : Card.Card_Type := Card.Construct(Deck.Ace, suit);
      acceptable_card : Card.Card_Type :=
        Card.Construct(Deck.Rank_Type'Succ (a_card.Get_Rank), suit);
   begin
      stack.Push (a_card);
      AUNit.Assertions.Assert (stack.Accepts.Is_Equal_To (acceptable_card),
                               "should accept: " & acceptable_card.Image &
                                 "but accepts: " & stack.Accepts.Image);
   end Push_Newly_Constructed_OK_Accept;

   -- push operation
   procedure Push_All_OK_Size (T : in out Test) is
      pragma Unreferenced (T);
      suit   : Deck.Suit_Type := Deck.Spade;
      stack  : Foundation_Stack.Stack_Type := Foundation_Stack.Construct (suit);
      a_card : Card.Card_Type;
      size   : Integer;     
   begin
      size := 0;
      for rank in Deck.Ace .. Deck.King loop
         a_card := Card.Construct(rank, suit);
         stack.Push (a_card);
         size := size + 1;
         AUNit.Assertions.Assert (stack.Size = size,
                                  "size should= " & size'Image &
                                    ", but is: " & stack.Size'Image);
      end loop;
   end Push_All_OK_Size;
   
   procedure Push_All_OK_Accept (T : in out Test) is
      pragma Unreferenced (T);
      suit   : Deck.Suit_Type := Deck.Spade;
      stack  : Foundation_Stack.Stack_Type := Foundation_Stack.Construct (suit);
      a_card : Card.Card_Type;
      acceptable_card : Card.Card_Type;
   begin
      for rank in Deck.Ace .. Deck.Queen loop
         a_card := Card.Construct(rank, suit);
         stack.Push (a_card);
         acceptable_card := Card.Construct(Deck.Rank_Type'Succ (a_card.Get_Rank),
                                           suit);
         AUNit.Assertions.Assert (stack.Accepts.Is_Equal_To (acceptable_card),
                                  "should accept: " & acceptable_card.Image &
                                    "but accepts: " & stack.Accepts.Image);
      end loop;
   end Push_All_OK_Accept;
   
   
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
      Ret.Add_Test (Caller.
                    Create("Foundation_Stack.Newly_Constucted_Is_Size_0",
                        Newly_Constucted_Is_Size_0'Access));
      -- various operations for the stack
      Ret.Add_Test (Caller.
                    Create("Foundation_Stack.Push_Newly_Constructed_OK_Size",
                           Push_Newly_Constructed_OK_Size'Access));
      Ret.Add_Test (Caller.
                    Create("Foundation_Stack.Push_Newly_Constructed_OK_Accept",
                           Push_Newly_Constructed_OK_Accept'Access));
      Ret.Add_Test (Caller.
                    Create("Foundation_Stack.Push_All_OK_Size",
                           Push_All_OK_Size'Access));
      Ret.Add_Test (Caller.
                    Create("Foundation_Stack.Push_All_OK_Accept",
                           Push_All_OK_Accept'Access));
      return Ret;
   end Suite;


end Foundation_Stack.Test;