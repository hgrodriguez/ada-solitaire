with Ada.Exceptions;

with AUnit.Assertions;
with AUnit.Test_Caller;

with Card;
with Cards;

package body Foundation.Test is

   --------------------------------------------------------------------
   --  all test procedures

   procedure Construct (T : in out Test) is
      pragma Unreferenced (T);
      F : Foundation.Foundation_Type := Foundation.Construct;
      pragma Warnings (Off, F);
   begin
      null;
   end Construct;

   procedure Construct_Check_Size (T : in out Test) is
      pragma Unreferenced (T);
      F : constant Foundation.Foundation_Type := Foundation.Construct;
   begin
      AUnit.Assertions.Assert (F.Size = 0,
                               "f.Size=" & F.Size'Image &
                                 " /= 0");
   end Construct_Check_Size;

   procedure Construct_Check_Is_Empty (T : in out Test) is
      pragma Unreferenced (T);
      F : constant Foundation.Foundation_Type := Foundation.Construct;
   begin
      AUnit.Assertions.Assert (F.Is_Empty,
                               "True" &
                                 " /= False");
   end Construct_Check_Is_Empty;

   procedure Check_Accepted_Empty_Foundation (T : in out Test) is
      pragma Unreferenced (T);
      F           : constant Foundation.Foundation_Type
        := Foundation.Construct;
      Acc_Diamond : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      Acc_Club    : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Club);
      Acc_Heart   : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Heart);
      Acc_Spade   : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Spade);
      Acceptable  : Foundation.Acceptable_Type := F.Accepts;
      Acc_Card    : Card.Card_Type;
   begin
      AUnit.Assertions.Assert (Acceptable.Size = 4,
                               "Acceptable.Size=" & Acceptable.Size'Image &
                                 " /= 4");

      Acc_Card := Acceptable.Get;
      AUnit.Assertions.Assert (Cards.Is_Equal_To (Acc_Card, Acc_Diamond),
                               "card=" & Acc_Card.Image &
                                 " /= " & Acc_Diamond.Image);
      Acc_Card := Acceptable.Get;
      AUnit.Assertions.Assert (Cards.Is_Equal_To (Acc_Card, Acc_Heart),
                               "card=" & Acc_Card.Image &
                                 " /= " & Acc_Heart.Image);
      Acc_Card := Acceptable.Get;
      AUnit.Assertions.Assert (Cards.Is_Equal_To (Acc_Card, Acc_Club),
                               "card=" & Acc_Card.Image &
                                 " /= " & Acc_Club.Image);
      Acc_Card := Acceptable.Get;
      AUnit.Assertions.Assert (Cards.Is_Equal_To (Acc_Card, Acc_Spade),
                               "card=" & Acc_Card.Image &
                                 " /= " & Acc_Spade.Image);
   end Check_Accepted_Empty_Foundation;

   procedure Accepts_Suit_All_Cards (Suit : Definitions.Suit);
   procedure Accepts_Suit_All_Cards (Suit : Definitions.Suit) is
      F            : constant Foundation.Foundation_Type
        := Foundation.Construct;
      Acceptable   : Foundation.Acceptable_Type;
      Card_To_Put  : Card.Card_Type;
      Card_Accepts : Card.Card_Type;
      use Definitions;
   begin
      for rank in Definitions.Ranks_Valid_Range loop
         Card_To_Put := Card.Construct (rank, Suit);
         Acceptable := F.Accepts;
         if Suit = Definitions.Diamond then
            Card_Accepts := Acceptable.Get;
         elsif Suit = Definitions.Heart then
            Card_Accepts := Acceptable.Get;
            Card_Accepts := Acceptable.Get;
         elsif Suit = Definitions.Club then
            Card_Accepts := Acceptable.Get;
            Card_Accepts := Acceptable.Get;
            Card_Accepts := Acceptable.Get;
         elsif Suit = Definitions.Spade then
            Card_Accepts := Acceptable.Get;
            Card_Accepts := Acceptable.Get;
            Card_Accepts := Acceptable.Get;
            Card_Accepts := Acceptable.Get;
         end if;
         AUnit.
           Assertions.
             Assert (Cards.Is_Equal_To (Card_Accepts, Card_To_Put),
                     "accepts:" & Card_Accepts.Image &
                       " /= to_put:" & Card_To_Put.Image);
         F.Put (Card_To_Put);
      end loop;
   end Accepts_Suit_All_Cards;

   procedure Accepts_Suit_Diamond_All_Cards (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Accepts_Suit_All_Cards (Definitions.Diamond);
   end Accepts_Suit_Diamond_All_Cards;

   procedure Accepts_Suit_Club_All_Cards (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Accepts_Suit_All_Cards (Definitions.Club);
   end Accepts_Suit_Club_All_Cards;

   procedure Accepts_Suit_Heart_All_Cards (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Accepts_Suit_All_Cards (Definitions.Heart);
   end Accepts_Suit_Heart_All_Cards;

   procedure Accepts_Suit_Spade_All_Cards (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Accepts_Suit_All_Cards (Definitions.Spade);
   end Accepts_Suit_Spade_All_Cards;

   procedure Accepts_Only_Other_Suits_Ace_For (Suit : Definitions.Suit);
   procedure Accepts_Only_Other_Suits_Ace_For (Suit : Definitions.Suit) is
      F             : constant Foundation.Foundation_Type
        := Foundation.Construct;
      C             : Card.Card_Type;
      Expected_Card : Card.Card_Type;
      Acceptable    : Foundation.Acceptable_Type;
      use Definitions;
   begin
      for r in Definitions.Ranks_Valid_Range loop
         for s in Definitions.Suits_Valid_Range loop
            if s = Suit then
               C := Card.Construct (r, s);
               F.Put (C);
            end if;
         end loop;
      end loop;
      Acceptable := F.Accepts;

      for s in Definitions.Suits_Valid_Range loop
         if s = Suit then
            Expected_Card := Card.Construct_Top_Rank (Suit);
         else
            Expected_Card := Card.Construct (Definitions.Ace, s);
         end if;
         C := Acceptable.Get;
         AUnit.Assertions.Assert (Cards.Is_Equal_To (C, Expected_Card),
                                  "exp: " & Expected_Card.Image &
                                    " /= act: " & C.Image);
      end loop;
   end Accepts_Only_Other_Suits_Ace_For;

   procedure Accepts_Only_Other_Suits_Ace_Diamond (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Accepts_Only_Other_Suits_Ace_For (Definitions.Diamond);
   end Accepts_Only_Other_Suits_Ace_Diamond;

   procedure Accepts_Only_Other_Suits_Ace_Club (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Accepts_Only_Other_Suits_Ace_For (Definitions.Club);
   end Accepts_Only_Other_Suits_Ace_Club;

   procedure Accepts_Only_Other_Suits_Ace_Heart (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Accepts_Only_Other_Suits_Ace_For (Definitions.Heart);
   end Accepts_Only_Other_Suits_Ace_Heart;

   procedure Accepts_Only_Other_Suits_Ace_Spade (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Accepts_Only_Other_Suits_Ace_For (Definitions.Spade);
   end Accepts_Only_Other_Suits_Ace_Spade;

   procedure Full_Foundation_Accepts_Is_Empty (T : in out Test) is
      pragma Unreferenced (T);
      F             : constant Foundation.Foundation_Type
        := Foundation.Construct;
      C             : Card.Card_Type;
      Acceptable    : Foundation.Acceptable_Type;
      Expected_Rank : constant Definitions.Rank := Definitions.Top;
      Actual_Rank   : Definitions.Rank;
      use Definitions;
   begin
      for rank in Definitions.Ranks_Valid_Range loop
         for suit in Definitions.Suits_Valid_Range loop
            C := Card.Construct (rank, suit);
            F.Put (C);
         end loop;
      end loop;
      Acceptable := F.Accepts;
      Actual_Rank := Acceptable.Get.Get_Rank;
      AUnit.Assertions.Assert (Expected_Rank = Actual_Rank,
                               "Rank exp: " & Expected_Rank'Image &
                                 " /= Rank act: " & Actual_Rank'Image);
      Actual_Rank := Acceptable.Get.Get_Rank;
      AUnit.Assertions.Assert (Expected_Rank = Actual_Rank,
                               "Rank exp: " & Expected_Rank'Image &
                                 " /= Rank act: " & Actual_Rank'Image);
      Actual_Rank := Acceptable.Get.Get_Rank;
      AUnit.Assertions.Assert (Expected_Rank = Actual_Rank,
                               "Rank exp: " & Expected_Rank'Image &
                                 " /= Rank act: " & Actual_Rank'Image);
      Actual_Rank := Acceptable.Get.Get_Rank;
      AUnit.Assertions.Assert (Expected_Rank = Actual_Rank,
                               "Rank exp: " & Expected_Rank'Image &
                                 " /= Rank act: " & Actual_Rank'Image);
   end Full_Foundation_Accepts_Is_Empty;

   procedure Does_Not_Accept_Card_Out_Of_Order_Exception;
   procedure Does_Not_Accept_Card_Out_Of_Order_Exception is
      F : constant Foundation.Foundation_Type := Foundation.Construct;
      C : constant Card.Card_Type
        := Card.Construct (Definitions.Three, Definitions.Diamond);
   begin
      F.Put (C);
   exception
      when Foundation_Wrong_Card_Exception => raise;
      when Exc : others =>
         AUnit.
           Assertions.
             Assert (False,
                     "Does_Not_Accept_Card_Out_Of_Order_Exception: " &
                       "wrong exception raised:" &
                       Ada.Exceptions.Exception_Name (Exc));
   end Does_Not_Accept_Card_Out_Of_Order_Exception;

   procedure Does_Not_Accept_Card_Out_Of_Order (T : in out Test) is
      pragma Unreferenced (T);
   begin
      AUnit.
        Assertions.
          Assert_Exception (Does_Not_Accept_Card_Out_Of_Order_Exception'Access,
                            "Does_Not_Accept_Card_Out_Of_Order: " &
                              "no exception raised");
   end Does_Not_Accept_Card_Out_Of_Order;

   procedure To_String_Empty_Foundation (T : in out Test) is
      pragma Unreferenced (T);
      F        : constant Foundation.Foundation_Type := Foundation.Construct;
      Expected : constant Foundation.To_String_Type := "           ";
      Actual   : Foundation.To_String_Type;
   begin
      Actual := F.To_String;
      AUnit.
        Assertions.Assert (Expected = Actual,
                           "Expected= " & Expected &
                             " /= " & Actual);
   end To_String_Empty_Foundation;

   procedure To_String_1_Non_Empty_Stack (T : in out Test) is
      pragma Unreferenced (T);
      F        : constant Foundation.Foundation_Type := Foundation.Construct;
      Card_1   : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      Expected : Foundation.To_String_Type := "           ";
      Actual   : Foundation.To_String_Type;
   begin
      Expected (1 .. 2) := Card_1.Short_Image;
      F.Put (Card_1);
      Actual := F.To_String;
      AUnit.Assertions.Assert (Expected = Actual,
                               "Expected= " & Expected &
                                 " /= " & Actual);
   end To_String_1_Non_Empty_Stack;

   procedure To_String_2_Non_Empty_Stack (T : in out Test) is
      pragma Unreferenced (T);
      F        : constant Foundation.Foundation_Type := Foundation.Construct;
      Card_1   : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      Card_2   : constant Card.Card_Type := Card.Construct (Definitions.Ace,
                                                            Definitions.Heart);
      Expected : Foundation.To_String_Type := "           ";
      Actual   : Foundation.To_String_Type;
   begin
      Expected (1 .. 2) := Card_1.Short_Image;
      Expected (4 .. 5) := Card_2.Short_Image;
      F.Put (Card_1);
      F.Put (Card_2);
      Actual := F.To_String;
      AUnit.Assertions.Assert (Expected = Actual,
                               "Expected= " & Expected &
                                 " /= " & Actual);
   end To_String_2_Non_Empty_Stack;

   procedure To_String_3_Non_Empty_Stack (T : in out Test) is
      pragma Unreferenced (T);
      F        : constant Foundation.Foundation_Type := Foundation.Construct;
      Card_1   : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      Card_2   : constant Card.Card_Type := Card.Construct (Definitions.Ace,
                                                            Definitions.Heart);
      Card_3   : constant Card.Card_Type := Card.Construct (Definitions.Ace,
                                                            Definitions.Club);
      Expected : Foundation.To_String_Type := "           ";
      Actual   : Foundation.To_String_Type;
   begin
      Expected (1 .. 2) := Card_1.Short_Image;
      Expected (4 .. 5) := Card_2.Short_Image;
      Expected (7 .. 8) := Card_3.Short_Image;
      F.Put (Card_1);
      F.Put (Card_2);
      F.Put (Card_3);
      Actual := F.To_String;
      AUnit.Assertions.Assert (Expected = Actual,
                               "Expected= " & Expected &
                                 " /= " & Actual);
   end To_String_3_Non_Empty_Stack;

   procedure To_String_4_Non_Empty_Stack (T : in out Test) is
      pragma Unreferenced (T);
      F        : constant Foundation.Foundation_Type := Foundation.Construct;
      Card_1   : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      Card_2   : constant Card.Card_Type := Card.Construct (Definitions.Ace,
                                                            Definitions.Heart);
      Card_3   : constant Card.Card_Type := Card.Construct (Definitions.Ace,
                                                            Definitions.Club);
      Card_4   : constant Card.Card_Type := Card.Construct (Definitions.Ace,
                                                            Definitions.Spade);
      Expected : Foundation.To_String_Type := "           ";
      Actual   : Foundation.To_String_Type;
   begin
      Expected (1 .. 2) := Card_1.Short_Image;
      Expected (4 .. 5) := Card_2.Short_Image;
      Expected (7 .. 8) := Card_3.Short_Image;
      Expected (10 .. 11) := Card_4.Short_Image;
      F.Put (Card_1);
      F.Put (Card_2);
      F.Put (Card_3);
      F.Put (Card_4);
      Actual := F.To_String;
      AUnit.Assertions.Assert (Expected = Actual,
                               "Expected= " & Expected &
                                 " /= " & Actual);
   end To_String_4_Non_Empty_Stack;

   procedure Ansi_To_String_Empty_Foundation (T : in out Test) is
      pragma Unreferenced (T);
      F        : constant Foundation.Foundation_Type := Foundation.Construct;
      Expected : constant Unbounded_String
        := To_Unbounded_String ("           ");
      Actual   : Unbounded_String;
   begin
      Actual := F.Ansi_To_String;
      AUnit.
        Assertions.
          Assert (Expected = Actual,
                  "Expected=" & To_String (Expected) &
                      " /= " & To_String (Actual));
   end Ansi_To_String_Empty_Foundation;

   procedure Ansi_To_String_1_Non_Empty_Stack (T : in out Test) is
      pragma Unreferenced (T);
      F        : constant Foundation.Foundation_Type := Foundation.Construct;
      Card_1   : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      Expected : constant Unbounded_String := Card_1.Ansi_Image &
        To_Unbounded_String ("         ");
      Actual   : Unbounded_String;
   begin
      F.Put (Card_1);
      Actual := F.Ansi_To_String;
      AUnit.
        Assertions.
          Assert (Expected = Actual,
                  "Expected=" & To_String (Expected) &
                      " /= " & To_String (Actual));
   end Ansi_To_String_1_Non_Empty_Stack;

   procedure Ansi_To_String_2_Non_Empty_Stack (T : in out Test) is
      pragma Unreferenced (T);
      F        : constant Foundation.Foundation_Type := Foundation.Construct;
      Card_1   : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      Card_2   : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Heart);
      Expected : constant Unbounded_String := Card_1.Ansi_Image &
        To_Unbounded_String (" ") &
        Card_2.Ansi_Image &
        To_Unbounded_String ("      ");
      Actual   : Unbounded_String;
   begin
      F.Put (Card_1);
      F.Put (Card_2);
      Actual := F.Ansi_To_String;
      AUnit.
        Assertions.
          Assert (Expected = Actual,
                  "Expected=" & To_String (Expected) &
                      " /= " & To_String (Actual));
   end Ansi_To_String_2_Non_Empty_Stack;

   procedure Ansi_To_String_3_Non_Empty_Stack (T : in out Test) is
      pragma Unreferenced (T);
      F        : constant Foundation.Foundation_Type := Foundation.Construct;
      Card_1   : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      Card_2   : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Heart);
      Card_3   : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Club);
      Expected : constant Unbounded_String := Card_1.Ansi_Image &
        To_Unbounded_String (" ") &
        Card_2.Ansi_Image &
        To_Unbounded_String (" ") &
        Card_3.Ansi_Image &
        To_Unbounded_String ("   ");
      Actual   : Unbounded_String;
   begin
      F.Put (Card_1);
      F.Put (Card_2);
      F.Put (Card_3);
      Actual := F.Ansi_To_String;
      AUnit.
        Assertions.
          Assert (Expected = Actual,
                  "Expected=" & To_String (Expected) &
                      " /= " & To_String (Actual));
   end Ansi_To_String_3_Non_Empty_Stack;

   procedure Ansi_To_String_4_Non_Empty_Stack (T : in out Test) is
      pragma Unreferenced (T);
      F        : constant Foundation.Foundation_Type := Foundation.Construct;
      Card_1   : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      Card_2   : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Heart);
      Card_3   : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Club);
      Card_4   : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Spade);
      Expected : constant Unbounded_String := Card_1.Ansi_Image &
        To_Unbounded_String (" ") &
        Card_2.Ansi_Image &
        To_Unbounded_String (" ") &
        Card_3.Ansi_Image &
        To_Unbounded_String (" ") &
        Card_4.Ansi_Image;
      Actual   : Unbounded_String;
   begin
      F.Put (Card_1);
      F.Put (Card_2);
      F.Put (Card_3);
      F.Put (Card_4);
      Actual := F.Ansi_To_String;
      AUnit.
        Assertions.
          Assert (Expected = Actual,
                  "Expected=" & To_String (Expected) &
                      " /= " & To_String (Actual));
   end Ansi_To_String_4_Non_Empty_Stack;

   --------------------------------------------------------------------
   --  the test suit construction
   package Caller is new AUnit.Test_Caller (Foundation.Test.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
      N   : constant String := "Foundation.Test.";
   begin
      --  ctor tests
      Ret.Add_Test (Caller.
                      Create (N & "Construct",
                        Construct'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Construct_Check_Size",
                        Construct_Check_Size'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Construct_Check_Is_Empty",
                        Construct_Check_Is_Empty'Access));

      Ret.Add_Test (Caller.
                      Create (N & "Check_Accepted_Empty_Foundation",
                        Check_Accepted_Empty_Foundation'Access));

      Ret.Add_Test (Caller.
                      Create (N & "Accepts_Suit_Diamond_All_Cards",
                        Accepts_Suit_Diamond_All_Cards'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Accepts_Suit_Club_All_Cards",
                        Accepts_Suit_Club_All_Cards'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Accepts_Suit_Heart_All_Cards",
                        Accepts_Suit_Heart_All_Cards'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Accepts_Suit_Spade_All_Cards",
                        Accepts_Suit_Spade_All_Cards'Access));

      Ret.Add_Test (Caller.
                      Create (N & "Accepts_Only_Other_Suits_Ace_Diamond",
                        Accepts_Only_Other_Suits_Ace_Diamond'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Accepts_Only_Other_Suits_Ace_Club",
                        Accepts_Only_Other_Suits_Ace_Club'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Accepts_Only_Other_Suits_Ace_Heart",
                        Accepts_Only_Other_Suits_Ace_Heart'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Accepts_Only_Other_Suits_Ace_Spade",
                        Accepts_Only_Other_Suits_Ace_Spade'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Full_Foundation_Accepts_Is_Empty",
                        Full_Foundation_Accepts_Is_Empty'Access));

      Ret.Add_Test (Caller.
                      Create (N & "Does_Not_Accept_Card_Out_Of_Order",
                        Does_Not_Accept_Card_Out_Of_Order'Access));

      Ret.Add_Test (Caller.
                      Create (N & "To_String_Empty_Foundation",
                        To_String_Empty_Foundation'Access));
      Ret.Add_Test (Caller.
                      Create (N & "To_String_1_Non_Empty_Stack",
                        To_String_1_Non_Empty_Stack'Access));
      Ret.Add_Test (Caller.
                      Create (N & "To_String_2_Non_Empty_Stack",
                        To_String_2_Non_Empty_Stack'Access));
      Ret.Add_Test (Caller.
                      Create (N & "To_String_3_Non_Empty_Stack",
                        To_String_3_Non_Empty_Stack'Access));
      Ret.Add_Test (Caller.
                      Create (N & "To_String_4_Non_Empty_Stack",
                        To_String_4_Non_Empty_Stack'Access));

      Ret.Add_Test (Caller.
                      Create (N & "Ansi_To_String_Empty_Foundation",
                        Ansi_To_String_Empty_Foundation'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Ansi_To_String_1_Non_Empty_Stack",
                        Ansi_To_String_1_Non_Empty_Stack'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Ansi_To_String_2_Non_Empty_Stack",
                        Ansi_To_String_2_Non_Empty_Stack'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Ansi_To_String_3_Non_Empty_Stack",
                        Ansi_To_String_3_Non_Empty_Stack'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Ansi_To_String_4_Non_Empty_Stack",
                        Ansi_To_String_4_Non_Empty_Stack'Access));

      return Ret;
   end Suite;

end Foundation.Test;
