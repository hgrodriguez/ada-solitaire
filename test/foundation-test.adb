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
      return Ret;
   end Suite;

end Foundation.Test;
