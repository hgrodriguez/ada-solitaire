with Ada.Exceptions;

with AUnit.Assertions;
with AUnit.Test_Caller;

with Definitions;
with Card;
with Cards;

package body Foundation_Stack.Test is

   --------------------------------------------------------------------
   --  constructors
   procedure Test_Construct_Diamond (T : in out Test) is
      pragma Unreferenced (T);
      suit  : constant Definitions.Suit := Definitions.Diamond;
      stack : Foundation_Stack.Stack_Type;
      use Definitions;
   begin
      stack := Foundation_Stack.Construct (suit);
      AUnit.Assertions.Assert (stack.Get_Suit = suit,
                               "suit=" & suit'Image &
                                 " /= " & stack.Get_Suit'Image);
   end Test_Construct_Diamond;

   procedure Test_Construct_Club (T : in out Test) is
      pragma Unreferenced (T);
      suit  : constant Definitions.Suit := Definitions.Club;
      stack : Foundation_Stack.Stack_Type;
      use Definitions;
   begin
      stack := Foundation_Stack.Construct (suit);
      AUnit.Assertions.Assert (stack.Get_Suit = suit,
                               "suit=" & suit'Image &
                                 " /= " & stack.Get_Suit'Image);
   end Test_Construct_Club;

   procedure Test_Construct_Heart (T : in out Test) is
      pragma Unreferenced (T);
      suit  : constant Definitions.Suit := Definitions.Heart;
      stack : Foundation_Stack.Stack_Type;
      use Definitions;
   begin
      stack := Foundation_Stack.Construct (suit);
      AUnit.Assertions.Assert (stack.Get_Suit = suit,
                               "suit=" & suit'Image &
                                 " /= " & stack.Get_Suit'Image);
   end Test_Construct_Heart;

   procedure Test_Construct_Spade (T : in out Test) is
      pragma Unreferenced (T);
      suit  : constant Definitions.Suit := Definitions.Spade;
      stack : Foundation_Stack.Stack_Type;
      use Definitions;
   begin
      stack := Foundation_Stack.Construct (suit);
      AUnit.Assertions.Assert (stack.Get_Suit = suit,
                               "suit=" & suit'Image &
                                 " /= " & stack.Get_Suit'Image);
   end Test_Construct_Spade;

   --  check newly constructed attributes
   procedure Newly_Constructed_Is_Empty (T : in out Test) is
      pragma Unreferenced (T);
      suit  : constant Definitions.Suit := Definitions.Spade;
      stack : Foundation_Stack.Stack_Type;
   begin
      stack := Foundation_Stack.Construct (suit);
      AUnit.Assertions.Assert (stack.Is_Empty,
                               "newly created stack should be empty," &
                                 " but is not");
   end Newly_Constructed_Is_Empty;

   procedure Newly_Constructed_Accepts_Ace (T : in out Test) is
      pragma Unreferenced (T);
      suit    : constant Definitions.Suit := Definitions.Spade;
      stack   : constant Foundation_Stack.Stack_Type :=
        Foundation_Stack.Construct (suit);
      a_card  : Card.Card_Type;
   begin
      a_card := Card.Construct (Definitions.Ace, suit);
      AUnit.Assertions.Assert (Cards.Is_Equal_To (stack.Accepts, a_card),
                               "should accept: " & a_card.Image &
                                 "but accepts: " & stack.Accepts.Image);
   end Newly_Constructed_Accepts_Ace;

   procedure Newly_Constructed_Is_Size_0 (T : in out Test) is
      pragma Unreferenced (T);
      suit    : constant Definitions.Suit := Definitions.Spade;
      stack   : constant Foundation_Stack.Stack_Type :=
        Foundation_Stack.Construct (suit);
   begin
      AUnit.Assertions.Assert (stack.Size = 0,
                               "size should = 0, but is: " & stack.Size'Image);
   end Newly_Constructed_Is_Size_0;

   --------------------------------------------------------------------
   --  check for push operations
   procedure Push_Newly_Constructed_OK_Size (T : in out Test) is
      pragma Unreferenced (T);
      suit    : constant Definitions.Suit := Definitions.Spade;
      stack   : Foundation_Stack.Stack_Type
        := Foundation_Stack.Construct (suit);
      a_card  : Card.Card_Type;
   begin
      a_card := Card.Construct (Definitions.Ace, suit);
      stack.Push (a_card);
      AUnit.Assertions.Assert (stack.Size = 1,
                               "size should = 1, but is: " & stack.Size'Image);
   end Push_Newly_Constructed_OK_Size;

   procedure Push_Newly_Constructed_OK_Accept (T : in out Test) is
      pragma Unreferenced (T);
      suit            : constant Definitions.Suit := Definitions.Spade;
      stack           : Foundation_Stack.Stack_Type :=
        Foundation_Stack.Construct (suit);
      a_card          : constant Card.Card_Type :=
        Card.Construct (Definitions.Ace, suit);
      acceptable_card : constant Card.Card_Type
        := Card.Construct (Definitions.Two, suit);
   begin
      stack.Push (a_card);
      AUnit.Assertions.Assert (Cards.Is_Equal_To (stack.Accepts,
                               acceptable_card),
                               "should accept: " & acceptable_card.Image &
                                 "but accepts: " & stack.Accepts.Image);
   end Push_Newly_Constructed_OK_Accept;

   --------------------------------------------------------------------
   --  push operation
   procedure Push_All_OK_Size (T : in out Test) is
      pragma Unreferenced (T);
      suit   : constant Definitions.Suit := Definitions.Spade;
      stack  : Foundation_Stack.Stack_Type
        := Foundation_Stack.Construct (suit);
      a_card : Card.Card_Type;
      size   : Natural;
   begin
      size := 0;
      for rank in Definitions.Ace .. Definitions.King loop
         a_card := Card.Construct (rank, suit);
         stack.Push (a_card);
         size := size + 1;
         AUnit.Assertions.Assert (stack.Size = size,
                                  "size should= " & size'Image &
                                    ", but is: " & stack.Size'Image);
      end loop;
   end Push_All_OK_Size;

   procedure Push_All_OK_Accept (T : in out Test) is
      pragma Unreferenced (T);
      suit            : constant Definitions.Suit := Definitions.Spade;
      stack           : Foundation_Stack.Stack_Type
        := Foundation_Stack.Construct (suit);
      a_card          : Card.Card_Type;
      acceptable_card : Card.Card_Type;
   begin
      for rank in Definitions.Ace .. Definitions.Queen loop
         a_card := Card.Construct (rank, suit);
         stack.Push (a_card);
         acceptable_card :=
           Card.Construct (Definitions.Rank'Succ (a_card.Get_Rank), suit);
         AUnit.Assertions.Assert (Cards.Is_Equal_To (stack.Accepts,
                                  acceptable_card),
                                  "should accept: " & acceptable_card.Image &
                                    "but accepts: " & stack.Accepts.Image);
      end loop;
   end Push_All_OK_Accept;

   procedure Stack_Is_Full  (T : in out Test) is
      pragma Unreferenced (T);
      suit            : constant Definitions.Suit := Definitions.Spade;
      stack           : Foundation_Stack.Stack_Type
        := Foundation_Stack.Construct (suit);
      a_card          : Card.Card_Type;
   begin
      for rank in Definitions.Ranks_Valid_Range loop
         a_card := Card.Construct (rank, suit);
         stack.Push (a_card);
      end loop;
      AUnit.Assertions.Assert (stack.Is_Full, "should be full");
   end Stack_Is_Full;

   procedure Full_Stack_Does_Not_Accept_Anything (T : in out Test) is
      pragma Unreferenced (T);
      suit            : constant Definitions.Suit := Definitions.Spade;
      stack           : Foundation_Stack.Stack_Type
        := Foundation_Stack.Construct (suit);
      a_card          : Card.Card_Type;
      acceptable_card : constant Card.Card_Type
        := Card.Construct_Top_Rank (suit);
   begin
      for rank in Definitions.Ranks_Valid_Range loop
         a_card := Card.Construct (rank, suit);
         stack.Push (a_card);
      end loop;
      AUnit.Assertions.Assert (Cards.Is_Equal_To (stack.Accepts,
                               acceptable_card),
                               "should accept: " & acceptable_card.Image &
                                 "but accepts: " & stack.Accepts.Image);
   end Full_Stack_Does_Not_Accept_Anything;

   --------------------------------------------------------------------
   --  Push wrong
   procedure Push_Wrong_Card_Wrong_Suit_Exception;
   procedure Push_Wrong_Card_Wrong_Suit_Exception is
      suit       : constant Definitions.Suit := Definitions.Spade;
      stack      : Foundation_Stack.Stack_Type
        := Foundation_Stack.Construct (suit);
      push_card  : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
   begin
      stack.Push (push_card);
   exception
      when Wrong_Suit_Exception => raise;
      when Exc : others =>
         AUnit.Assertions.Assert (False,
                                  "Pop_One_Pushed_None_Exception: " &
                                    "wrong exception raised:" &
                                    Ada.Exceptions.Exception_Name (Exc));
   end Push_Wrong_Card_Wrong_Suit_Exception;

   procedure Push_Wrong_Card_Wrong_Suit (T : in out Test) is
      pragma Unreferenced (T);
   begin
      AUnit.
        Assertions.
          Assert_Exception (Push_Wrong_Card_Wrong_Suit_Exception'Access,
                            "Push_Wrong_Card_Wrong_Suit: " &
                              "no exception raised");
   end Push_Wrong_Card_Wrong_Suit;

   procedure Push_Wrong_Card_Wrong_Rank_Exception;
   procedure Push_Wrong_Card_Wrong_Rank_Exception is
      suit       : constant Definitions.Suit := Definitions.Spade;
      stack      : Foundation_Stack.Stack_Type
        := Foundation_Stack.Construct (suit);
      push_card  : constant Card.Card_Type := Card.Construct (Definitions.King,
                                                              suit);
   begin
      stack.Push (push_card);
   exception
      when Wrong_Rank_Exception => raise;
      when Exc : others =>
         AUnit.Assertions.Assert (False,
                                  "Pop_One_Pushed_None_Exception: " &
                                    "wrong exception raised:" &
                                    Ada.Exceptions.Exception_Name (Exc));
   end Push_Wrong_Card_Wrong_Rank_Exception;

   procedure Push_Wrong_Card_Wrong_Rank (T : in out Test) is
      pragma Unreferenced (T);
   begin
      AUnit.
        Assertions.
          Assert_Exception (Push_Wrong_Card_Wrong_Rank_Exception'Access,
                            "Push_Wrong_Card_Wrong_Rank: " &
                              "no exception raised");
   end Push_Wrong_Card_Wrong_Rank;

   procedure To_String_Empty (T : in out Test) is
      pragma Unreferenced (T);
      suit     : constant Definitions.Suit := Definitions.Spade;
      stack    : constant Foundation_Stack.Stack_Type
        := Foundation_Stack.Construct (suit);
      Expected : constant Card.Short_Image_Type := Card.Empty_Short_Image;
      Actual   : Card.Short_Image_Type;
   begin
      Actual := stack.To_String;
      AUnit.Assertions.Assert (Expected = Actual,
                               "Expected=" & Expected &
                                 " /= " & Actual);
   end To_String_Empty;

   procedure To_String_Not_Empty  (T : in out Test) is
      pragma Unreferenced (T);
      suit     : constant Definitions.Suit := Definitions.Spade;
      stack    : Foundation_Stack.Stack_Type
        := Foundation_Stack.Construct (suit);
      a_card   : Card.Card_Type := Card.Construct (Definitions.Ace, suit);
      Expected : constant Card.Short_Image_Type := a_card.Short_Image;
      Actual   : Card.Short_Image_Type;
   begin
      a_card := Card.Construct (Definitions.Ace, suit);
      stack.Push (a_card);
      Actual := stack.To_String;
      AUnit.Assertions.Assert (Expected = Actual,
                               "Expected=" & Expected &
                                 " /= " & Actual);
   end To_String_Not_Empty;

   procedure Ansi_To_String_Empty (T : in out Test) is
      pragma Unreferenced (T);
   begin
      AUnit.Assertions.Assert (False, "NOT IMPLEMENTED");
   end Ansi_To_String_Empty;

   procedure Ansi_To_String_Not_Empty_Red  (T : in out Test) is
      pragma Unreferenced (T);
   begin
      AUnit.Assertions.Assert (False, "NOT IMPLEMENTED");
   end Ansi_To_String_Not_Empty_Red;

   procedure Ansi_To_String_Not_Empty_Black  (T : in out Test) is
      pragma Unreferenced (T);
   begin
      AUnit.Assertions.Assert (False, "NOT IMPLEMENTED");
   end Ansi_To_String_Not_Empty_Black;

   --------------------------------------------------------------------
   --  the test suit construction
   package Caller is new AUnit.Test_Caller (Foundation_Stack.Test.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
      N   : constant String := "Foundation_Stack.";
   begin
      --  ctor tests
      Ret.Add_Test (Caller.
                      Create (N & "Test_Construct_Diamond",
                        Test_Construct_Diamond'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Test_Construct_Club",
                        Test_Construct_Club'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Test_Construct_Heart",
                        Test_Construct_Heart'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Test_Construct_Spade",
                        Test_Construct_Spade'Access));

      --  check states of stack
      Ret.Add_Test (Caller.
                      Create (N & "Newly_Constucted_Is_Empty",
                        Newly_Constructed_Is_Empty'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Newly_Constucted_Accepts_Ace",
                        Newly_Constructed_Accepts_Ace'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Newly_Constucted_Is_Size_0",
                        Newly_Constructed_Is_Size_0'Access));
      --  various operations for the stack
      Ret.Add_Test (Caller.
                      Create (N & "Push_Newly_Constructed_OK_Size",
                        Push_Newly_Constructed_OK_Size'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Push_Newly_Constructed_OK_Accept",
                        Push_Newly_Constructed_OK_Accept'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Push_All_OK_Size",
                        Push_All_OK_Size'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Push_All_OK_Accept",
                        Push_All_OK_Accept'Access));

      Ret.Add_Test (Caller.
                      Create (N & "Stack_Is_Full",
                        Stack_Is_Full'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Full_Stack_Does_Not_Accept_Anything",
                        Full_Stack_Does_Not_Accept_Anything'Access));

      Ret.Add_Test (Caller.
                      Create (N & "Push_Wrong_Card_Wrong_Suit",
                        Push_Wrong_Card_Wrong_Suit'Access));

      Ret.Add_Test (Caller.
                      Create (N & "Push_Wrong_Card_Wrong_Rank",
                        Push_Wrong_Card_Wrong_Rank'Access));

      Ret.Add_Test (Caller.
                      Create (N & "To_String_Empty",
                        To_String_Empty'Access));
      Ret.Add_Test (Caller.
                      Create (N & "To_String_Not_Empty",
                        To_String_Not_Empty'Access));

      Ret.Add_Test (Caller.
                      Create (N & "Ansi_To_String_Empty",
                        Ansi_To_String_Empty'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Ansi_To_String_Not_Empty_Red",
                        Ansi_To_String_Not_Empty_Red'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Ansi_To_String_Not_Empty_Black",
                        Ansi_To_String_Not_Empty_Black'Access));

      return Ret;
   end Suite;

end Foundation_Stack.Test;
