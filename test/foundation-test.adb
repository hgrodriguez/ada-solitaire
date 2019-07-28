with AUnit.Assertions;
with AUnit.Test_Caller;

with Card;
with Deck; use Deck;

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

   procedure Check_Accepted_Empty_Foundation (T : in out Test) is
      pragma Unreferenced (T);
      F           : constant Foundation.Foundation_Type := Foundation.Construct;
      Acc_Diamond : constant Card.Card_Type := Card.Construct (Deck.Ace,
                                                               Deck.Diamond);
      Acc_Club    : constant Card.Card_Type := Card.Construct (Deck.Ace,
                                                               Deck.Club);
      Acc_Heart   : constant Card.Card_Type := Card.Construct (Deck.Ace,
                                                               Deck.Heart);
      Acc_Spade   : constant Card.Card_Type := Card.Construct (Deck.Ace,
                                                               Deck.Spade);
      Acceptable  : Foundation.Acceptable_Type := F.Accepts;
      Acc_Card    : Card.Card_Type;
   begin
      AUnit.Assertions.Assert (Acceptable.Size = 4,
                               "Acceptable.Size=" & Acceptable.Size'Image &
                                 " /= 4");

      Acc_Card := Acceptable.Get;
      AUnit.Assertions.Assert (Acc_Card.Is_Equal_To (Acc_Diamond),
                               "card=" & Acc_Card.Image &
                                 " /= " & Acc_Diamond.Image);
      Acc_Card := Acceptable.Get;
      AUnit.Assertions.Assert (Acc_Card.Is_Equal_To (Acc_Club),
                               "card=" & Acc_Card.Image &
                                 " /= " & Acc_Diamond.Image);
      Acc_Card := Acceptable.Get;
      AUnit.Assertions.Assert (Acc_Card.Is_Equal_To (Acc_Heart),
                               "card=" & Acc_Card.Image &
                                 " /= " & Acc_Diamond.Image);
      Acc_Card := Acceptable.Get;
      AUnit.Assertions.Assert (Acc_Card.Is_Equal_To (Acc_Spade),
                               "card=" & Acc_Card.Image &
                                 " /= " & Acc_Diamond.Image);
   end Check_Accepted_Empty_Foundation;

   procedure Accepts_Suit_All_Cards (Suit : Deck.Suit_Type);
   procedure Accepts_Suit_All_Cards (Suit : Deck.Suit_Type) is
      F            : constant Foundation.Foundation_Type
        := Foundation.Construct;
      Acceptable   : Foundation.Acceptable_Type;
      Card_To_Put  : Card.Card_Type;
      Card_Accepts : Card.Card_Type;
   begin
      for rank in Deck.Rank_Type_Valid_Range loop
         Card_To_Put := Card.Construct (rank, Suit);
         Acceptable := F.Accepts;
         if Suit = Deck.Diamond then
            Card_Accepts := Acceptable.Get;
         elsif Suit = Deck.Club then
            Card_Accepts := Acceptable.Get;
            Card_Accepts := Acceptable.Get;
         elsif Suit = Deck.Heart then
            Card_Accepts := Acceptable.Get;
            Card_Accepts := Acceptable.Get;
            Card_Accepts := Acceptable.Get;
         elsif Suit = Deck.Spade then
            Card_Accepts := Acceptable.Get;
            Card_Accepts := Acceptable.Get;
            Card_Accepts := Acceptable.Get;
            Card_Accepts := Acceptable.Get;
         end if;
         AUnit.Assertions.Assert (Card_Accepts.Is_Equal_To (Card_To_Put),
                                  "accepts:" & Card_Accepts.Image &
                                    " /= to_put:" & Card_To_Put.Image);
         F.Put (Card_To_Put);
      end loop;
   end Accepts_Suit_All_Cards;

   procedure Accepts_Suit_Diamond_All_Cards (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Accepts_Suit_All_Cards (Deck.Diamond);
   end Accepts_Suit_Diamond_All_Cards;

   procedure Accepts_Suit_Club_All_Cards (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Accepts_Suit_All_Cards (Deck.Club);
   end Accepts_Suit_Club_All_Cards;

   procedure Accepts_Suit_Heart_All_Cards (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Accepts_Suit_All_Cards (Deck.Heart);
   end Accepts_Suit_Heart_All_Cards;

   procedure Accepts_Suit_Spade_All_Cards (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Accepts_Suit_All_Cards (Deck.Spade);
   end Accepts_Suit_Spade_All_Cards;

   procedure Accepts_Only_Other_Suits_Ace_For (Suit : Deck.Suit_Type);
   procedure Accepts_Only_Other_Suits_Ace_For (Suit : Deck.Suit_Type) is
      F             : constant Foundation.Foundation_Type
        := Foundation.Construct;
      C             : Card.Card_Type;
      Expected_Card : Card.Card_Type;
      Acceptable    : Foundation.Acceptable_Type;
   begin
      for r in Deck.Rank_Type_Valid_Range loop
         for s in Deck.Suit_Type_Valid_Range loop
            if s = Suit then
               C := Card.Construct (r, s);
               F.Put (C);
            end if;
         end loop;
      end loop;
      Acceptable := F.Accepts;

      for s in Deck.Suit_Type_Valid_Range loop
         if s = Suit then
            Expected_Card := Card.Construct_Top_Rank (Suit);
         else
            Expected_Card := Card.Construct (Deck.Ace, s);
         end if;
         C := Acceptable.Get;
         AUnit.Assertions.Assert (C.Is_Equal_To (Expected_Card),
                                  "exp: " & Expected_Card.Image &
                                    " /= act: " & C.Image);
      end loop;
   end Accepts_Only_Other_Suits_Ace_For;

   procedure Accepts_Only_Other_Suits_Ace_Diamond (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Accepts_Only_Other_Suits_Ace_For (Deck.Diamond);
   end Accepts_Only_Other_Suits_Ace_Diamond;

   procedure Accepts_Only_Other_Suits_Ace_Club (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Accepts_Only_Other_Suits_Ace_For (Deck.Club);
   end Accepts_Only_Other_Suits_Ace_Club;

   procedure Accepts_Only_Other_Suits_Ace_Heart (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Accepts_Only_Other_Suits_Ace_For (Deck.Heart);
   end Accepts_Only_Other_Suits_Ace_Heart;

   procedure Accepts_Only_Other_Suits_Ace_Spade (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Accepts_Only_Other_Suits_Ace_For (Deck.Spade);
   end Accepts_Only_Other_Suits_Ace_Spade;

   procedure Full_Foundation_Accepts_Is_Empty (T : in out Test) is
      pragma Unreferenced (T);
      F             : constant Foundation.Foundation_Type
                        := Foundation.Construct;
      C             : Card.Card_Type;
      Acceptable    : Foundation.Acceptable_Type;
      Expected_Rank : constant Deck.Rank_Type := Deck.Top;
      Actual_Rank   : Deck.Rank_Type;
   begin
      for rank in Deck.Rank_Type_Valid_Range loop
         for suit in Deck.Suit_Type_Valid_Range loop
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

   --------------------------------------------------------------------
   --  the test suit construction
   package Caller is new AUnit.Test_Caller (Foundation.Test.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
             := new AUnit.Test_Suites.Test_Suite;
   begin
      --  ctor tests
      Ret.Add_Test (Caller.
                      Create ("Foundation.Construct",
                        Construct'Access));
      Ret.Add_Test (Caller.
                      Create ("Foundation.Construct_Check_Size",
                        Construct_Check_Size'Access));

      Ret.Add_Test (Caller.
                      Create ("Foundation.Check_Accepted_Empty_Foundation",
                        Check_Accepted_Empty_Foundation'Access));

      Ret.Add_Test (Caller.
                      Create ("Foundation.Accepts_Suit_Diamond_All_Cards",
                        Accepts_Suit_Diamond_All_Cards'Access));
      Ret.Add_Test (Caller.
                      Create ("Foundation.Accepts_Suit_Club_All_Cards",
                        Accepts_Suit_Club_All_Cards'Access));
      Ret.Add_Test (Caller.
                      Create ("Foundation.Accepts_Suit_Heart_All_Cards",
                        Accepts_Suit_Heart_All_Cards'Access));
      Ret.Add_Test (Caller.
                      Create ("Foundation.Accepts_Suit_Spade_All_Cards",
                        Accepts_Suit_Spade_All_Cards'Access));

      Ret.Add_Test (Caller.
                      Create ("Foundation.Accepts_Only_Other_Suits_Ace_Diamond",
                        Accepts_Only_Other_Suits_Ace_Diamond'Access));
      Ret.Add_Test (Caller.
                      Create ("Foundation.Accepts_Only_Other_Suits_Ace_Club",
                        Accepts_Only_Other_Suits_Ace_Club'Access));
      Ret.Add_Test (Caller.
                      Create ("Foundation.Accepts_Only_Other_Suits_Ace_Heart",
                        Accepts_Only_Other_Suits_Ace_Heart'Access));
      Ret.Add_Test (Caller.
                      Create ("Foundation.Accepts_Only_Other_Suits_Ace_Spade",
                        Accepts_Only_Other_Suits_Ace_Spade'Access));
      Ret.Add_Test (Caller.
                      Create ("Foundation.Full_Foundation_Accepts_Is_Empty",
                        Full_Foundation_Accepts_Is_Empty'Access));
      return Ret;
   end Suite;

end Foundation.Test;
