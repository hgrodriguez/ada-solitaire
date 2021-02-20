with Definitions;
with Card;
with Cards;

with AUnit.Assertions;
with AUnit.Test_Caller;

package body Cards.Test is

   --------------------------------------------------------------------
   --  test comparisons of two cards
   --------------------------------------------------------------------

   --------------------------------------------------------------------
   --  rank
   procedure Rank_Is_Equal_To_Using_Identical_Rank (T : in out Test) is
      pragma Unreferenced (T);
      rank : constant Definitions.Rank := Definitions.Ace;
      suit : constant Definitions.Suit := Definitions.Diamond;
      c1   : constant Card.Card_Type := Card.Construct (rank, suit);
      c2   : constant Card.Card_Type := Card.Construct (rank, suit);

   begin
      AUnit.Assertions.Assert (Rank_Is_Equal_To (c1, c2),
                               "c1.Rank:" & c1.Get_Rank'Image &
                                 " should be equal to " &
                                 "c2.Rank:" & c2.Get_Rank'Image);
   end Rank_Is_Equal_To_Using_Identical_Rank;

   procedure Rank_Is_Equal_To_Using_Equal_Rank (T : in out Test) is
      pragma Unreferenced (T);
      rank1 : constant Definitions.Rank := Definitions.Ace;
      rank2 : constant Definitions.Rank := Definitions.Ace;
      suit  : constant Definitions.Suit := Definitions.Diamond;
      c1    : constant Card.Card_Type := Card.Construct (rank1, suit);
      c2    : constant Card.Card_Type := Card.Construct (rank2, suit);

   begin
      AUnit.Assertions.Assert (Rank_Is_Equal_To (c1, c2),
                               "c1.Rank:" & c1.Get_Rank'Image &
                                 " should be equal to " &
                                 "c2.Rank:" & c2.Get_Rank'Image);
   end Rank_Is_Equal_To_Using_Equal_Rank;

   procedure Rank_IsEqualTo_Using_Identical_Rank (T : in out Test) is
      pragma Unreferenced (T);
      rank : constant Definitions.Rank := Definitions.Ace;
      suit : constant Definitions.Suit := Definitions.Diamond;
      c1   : constant Card.Card_Type := Card.Construct (rank, suit);
      c2   : constant Card.Card_Type := Card.Construct (rank, suit);

   begin
      AUnit.Assertions.Assert (c1 = c2,
                               "c1.Rank:" & c1.Get_Rank'Image &
                                 " should be equal to " &
                                 "c2.Rank:" & c2.Get_Rank'Image);
   end Rank_IsEqualTo_Using_Identical_Rank;

   procedure Rank_IsEqualTo_Using_Equal_Rank (T : in out Test) is
      pragma Unreferenced (T);
      rank1 : constant Definitions.Rank := Definitions.Ace;
      rank2 : constant Definitions.Rank := Definitions.Ace;
      suit  : constant Definitions.Suit := Definitions.Diamond;
      c1    : constant Card.Card_Type := Card.Construct (rank1, suit);
      c2    : constant Card.Card_Type := Card.Construct (rank2, suit);

   begin
      AUnit.Assertions.Assert (Rank_Is_Equal_To (c1, c2),
                               "c1.Rank:" & c1.Get_Rank'Image &
                                 " should be equal to " &
                                 "c2.Rank:" & c2.Get_Rank'Image);
   end Rank_IsEqualTo_Using_Equal_Rank;

   procedure Rank_Is_Higher_Than_True (T : in out Test) is
      pragma Unreferenced (T);
      rank_low  : constant Definitions.Rank := Definitions.Ace;
      rank_high : constant Definitions.Rank := Definitions.King;
      suit      : constant Definitions.Suit := Definitions.Diamond;
      c1        : constant Card.Card_Type := Card.Construct (rank_high, suit);
      c2        : constant Card.Card_Type := Card.Construct (rank_low, suit);

   begin
      AUnit.Assertions.Assert (Rank_Is_Higher_Than (c1, c2),
                               "c1.Rank:" & c1.Get_Rank'Image &
                                 " should be higher than " &
                                 "c2.Rank:" & c2.Get_Rank'Image);
   end Rank_Is_Higher_Than_True;

   procedure Rank_Is_Higher_Than_False (T : in out Test) is
      pragma Unreferenced (T);
      rank_low  : constant Definitions.Rank := Definitions.Ace;
      rank_high : constant Definitions.Rank := Definitions.King;
      suit      : constant Definitions.Suit := Definitions.Diamond;
      c1        : constant Card.Card_Type := Card.Construct (rank_low, suit);
      c2        : constant Card.Card_Type := Card.Construct (rank_high, suit);

   begin
      AUnit.Assertions.Assert (not Rank_Is_Higher_Than (c1, c2),
                               "c1.Rank:" & c1.Get_Rank'Image &
                                 " should be higher than " &
                                 "c2.Rank:" & c2.Get_Rank'Image);
   end Rank_Is_Higher_Than_False;

   procedure Rank_Is_Lower_Than_True (T : in out Test) is
      pragma Unreferenced (T);
      rank_low  : constant Definitions.Rank := Definitions.Ace;
      rank_high : constant Definitions.Rank := Definitions.King;
      suit      : constant Definitions.Suit := Definitions.Diamond;
      c1        : constant Card.Card_Type := Card.Construct (rank_low, suit);
      c2        : constant Card.Card_Type := Card.Construct (rank_high, suit);

   begin
      AUnit.Assertions.Assert (Rank_Is_Lower_Than (c1, c2),
                               "c1.Rank:" & c1.Get_Rank'Image &
                                 " should be lower than " &
                                 "c2.Rank:" & c2.Get_Rank'Image);
   end Rank_Is_Lower_Than_True;

   procedure Rank_Is_Lower_Than_False (T : in out Test) is
      pragma Unreferenced (T);
      rank_low  : constant Definitions.Rank := Definitions.Ace;
      rank_high : constant Definitions.Rank := Definitions.King;
      suit      : constant Definitions.Suit := Definitions.Diamond;
      c1        : constant Card.Card_Type := Card.Construct (rank_high, suit);
      c2        : constant Card.Card_Type := Card.Construct (rank_low, suit);

   begin
      AUnit.Assertions.Assert (not Rank_Is_Lower_Than (c1, c2),
                               "c1.Rank:" & c1.Get_Rank'Image &
                                 " should be higher than " &
                                 "c2.Rank:" & c2.Get_Rank'Image);
   end Rank_Is_Lower_Than_False;

   --------------------------------------------------------------------
   --  suit
   procedure Suit_Is_Equal_To_Using_Identical_Suit (T : in out Test) is
      pragma Unreferenced (T);
      rank : constant Definitions.Rank := Definitions.Ace;
      suit : constant Definitions.Suit := Definitions.Diamond;
      c1   : constant Card.Card_Type := Card.Construct (rank, suit);
      c2   : constant Card.Card_Type := Card.Construct (rank, suit);

   begin
      AUnit.Assertions.Assert (Suit_Is_Equal_To (c1, c2),
                               "c1.Suit:" & c1.Get_Suit'Image &
                                 " should be equal to " &
                                 "c2.Suit:" & c2.Get_Suit'Image);
   end Suit_Is_Equal_To_Using_Identical_Suit;

   procedure Suit_Is_Equal_To_Using_Equal_Suit (T : in out Test) is
      pragma Unreferenced (T);
      rank  : constant Definitions.Rank := Definitions.Ace;
      suit1 : constant Definitions.Suit := Definitions.Diamond;
      suit2 : constant Definitions.Suit := Definitions.Diamond;
      c1    : constant Card.Card_Type := Card.Construct (rank, suit1);
      c2    : constant Card.Card_Type := Card.Construct (rank, suit2);

   begin
      AUnit.Assertions.Assert (Suit_Is_Equal_To (c1, c2),
                               "c1.Suit:" & c1.Get_Suit'Image &
                                 " should be equal to " &
                                 "c2.Suit:" & c2.Get_Suit'Image);
   end Suit_Is_Equal_To_Using_Equal_Suit;

   procedure Suit_IsEqualTo_Using_Identical_Suit (T : in out Test) is
      pragma Unreferenced (T);
      rank : constant Definitions.Rank := Definitions.Ace;
      suit : constant Definitions.Suit := Definitions.Diamond;
      c1   : constant Card.Card_Type := Card.Construct (rank, suit);
      c2   : constant Card.Card_Type := Card.Construct (rank, suit);

   begin
      AUnit.Assertions.Assert (c1 = c2,
                               "c1.Suit:" & c1.Get_Suit'Image &
                                 " should be equal to " &
                                 "c2.Suit:" & c2.Get_Suit'Image);
   end Suit_IsEqualTo_Using_Identical_Suit;

   procedure Suit_IsEqualTo_Using_Equal_Suit (T : in out Test) is
      pragma Unreferenced (T);
      rank  : constant Definitions.Rank := Definitions.Ace;
      suit1 : constant Definitions.Suit := Definitions.Diamond;
      suit2 : constant Definitions.Suit := Definitions.Diamond;
      c1    : constant Card.Card_Type := Card.Construct (rank, suit1);
      c2    : constant Card.Card_Type := Card.Construct (rank, suit2);

   begin
      AUnit.Assertions.Assert (Suit_Is_Equal_To (c1, c2),
                               "c1.Suit:" & c1.Get_Suit'Image &
                                 " should be equal to " &
                                 "c2.Suit:" & c2.Get_Suit'Image);
   end Suit_IsEqualTo_Using_Equal_Suit;

   --------------------------------------------------------------------
   --  rank and suit
   procedure Is_Equal_To_Identical_Rank_Identical_Suit (T : in out Test) is
      pragma Unreferenced (T);
      rank : constant Definitions.Rank := Definitions.Ace;
      suit : constant Definitions.Suit := Definitions.Diamond;
      c1   : constant Card.Card_Type := Card.Construct (rank, suit);
      c2   : constant Card.Card_Type := Card.Construct (rank, suit);

   begin
      AUnit.Assertions.Assert (Is_Equal_To (c1, c2),
                               "c1.Rank:" & c1.Get_Rank'Image &
                                 " should be equal to " &
                                 "c2.Rank:" & c2.Get_Rank'Image);
   end Is_Equal_To_Identical_Rank_Identical_Suit;

   procedure Is_Equal_To_Equal_Rank_Identical_Suit (T : in out Test) is
      pragma Unreferenced (T);
      rank1 : constant Definitions.Rank := Definitions.Ace;
      rank2 : constant Definitions.Rank := Definitions.Ace;
      suit  : constant Definitions.Suit := Definitions.Diamond;
      c1    : constant Card.Card_Type := Card.Construct (rank1, suit);
      c2    : constant Card.Card_Type := Card.Construct (rank2, suit);

   begin
      AUnit.Assertions.Assert (Is_Equal_To (c1, c2),
                               "c1.Rank:" & c1.Get_Rank'Image &
                                 " should be equal to " &
                                 "c2.Rank:" & c2.Get_Rank'Image);
   end Is_Equal_To_Equal_Rank_Identical_Suit;

   procedure Is_Equal_To_Identical_Rank_Equal_Suit (T : in out Test) is
      pragma Unreferenced (T);
      rank  : constant Definitions.Rank := Definitions.Ace;
      suit1 : constant Definitions.Suit := Definitions.Diamond;
      suit2 : constant Definitions.Suit := Definitions.Diamond;
      c1    : constant Card.Card_Type := Card.Construct (rank, suit1);
      c2    : constant Card.Card_Type := Card.Construct (rank, suit2);

   begin
      AUnit.Assertions.Assert (Is_Equal_To (c1, c2),
                               "c1.Rank:" & c1.Get_Rank'Image &
                                 " should be equal to " &
                                 "c2.Rank:" & c2.Get_Rank'Image);
   end Is_Equal_To_Identical_Rank_Equal_Suit;

   procedure Is_Equal_To_Equal_Rank_Equal_Suit (T : in out Test) is
      pragma Unreferenced (T);
      rank1 : constant Definitions.Rank := Definitions.Ace;
      rank2 : constant Definitions.Rank := Definitions.Ace;
      suit1 : constant Definitions.Suit := Definitions.Diamond;
      suit2 : constant Definitions.Suit := Definitions.Diamond;
      c1    : constant Card.Card_Type := Card.Construct (rank1, suit1);
      c2    : constant Card.Card_Type := Card.Construct (rank2, suit2);

   begin
      AUnit.Assertions.Assert (Is_Equal_To (c1, c2),
                               "c1.Rank:" & c1.Get_Rank'Image &
                                 " should be equal to " &
                                 "c2.Rank:" & c2.Get_Rank'Image);
   end Is_Equal_To_Equal_Rank_Equal_Suit;

   procedure IsEqualTo_Identical_Rank_Identical_Suit (T : in out Test) is
      pragma Unreferenced (T);
      rank : constant Definitions.Rank := Definitions.Ace;
      suit : constant Definitions.Suit := Definitions.Diamond;
      c1   : constant Card.Card_Type := Card.Construct (rank, suit);
      c2   : constant Card.Card_Type := Card.Construct (rank, suit);

   begin
      AUnit.Assertions.Assert (c1 = c2,
                               "c1.Rank:" & c1.Get_Rank'Image &
                                 " should be equal to " &
                                 "c2.Rank:" & c2.Get_Rank'Image);
   end IsEqualTo_Identical_Rank_Identical_Suit;

   procedure IsEqualTo_Equal_Rank_Identical_Suit (T : in out Test) is
      pragma Unreferenced (T);
      rank1 : constant Definitions.Rank := Definitions.Ace;
      rank2 : constant Definitions.Rank := Definitions.Ace;
      suit  : constant Definitions.Suit := Definitions.Diamond;
      c1    : constant Card.Card_Type := Card.Construct (rank1, suit);
      c2    : constant Card.Card_Type := Card.Construct (rank2, suit);

   begin
      AUnit.Assertions.Assert (c1 = c2,
                               "c1.Rank:" & c1.Get_Rank'Image &
                                 " should be equal to " &
                                 "c2.Rank:" & c2.Get_Rank'Image);
   end IsEqualTo_Equal_Rank_Identical_Suit;

   procedure IsEqualTo_Identical_Rank_Equal_Suit (T : in out Test) is
      pragma Unreferenced (T);
      rank  : constant Definitions.Rank := Definitions.Ace;
      suit1 : constant Definitions.Suit := Definitions.Diamond;
      suit2 : constant Definitions.Suit := Definitions.Diamond;
      c1    : constant Card.Card_Type := Card.Construct (rank, suit1);
      c2    : constant Card.Card_Type := Card.Construct (rank, suit2);

   begin
      AUnit.Assertions.Assert (Is_Equal_To (c1, c2),
                               "c1.Rank:" & c1.Get_Rank'Image &
                                 " should be equal to " &
                                 "c2.Rank:" & c2.Get_Rank'Image);
   end IsEqualTo_Identical_Rank_Equal_Suit;

   procedure IsEqualTo_Equal_Rank_Equal_Suit (T : in out Test) is
      pragma Unreferenced (T);
      rank1 : constant Definitions.Rank := Definitions.Ace;
      rank2 : constant Definitions.Rank := Definitions.Ace;
      suit1 : constant Definitions.Suit := Definitions.Diamond;
      suit2 : constant Definitions.Suit := Definitions.Diamond;
      c1    : constant Card.Card_Type := Card.Construct (rank1, suit1);
      c2    : constant Card.Card_Type := Card.Construct (rank2, suit2);

   begin
      AUnit.Assertions.Assert (Is_Equal_To (c1, c2),
                               "c1.Rank:" & c1.Get_Rank'Image &
                                 " should be equal to " &
                                 "c2.Rank:" & c2.Get_Rank'Image);
   end IsEqualTo_Equal_Rank_Equal_Suit;

   --------------------------------------------------------------------
   --  the test suit construction
   package Caller is new AUnit.Test_Caller (Cards.Test.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
      N   : constant String := "Cards.Test.";
   begin
      --  rank tests
      Ret.Add_Test (Caller.
                      Create (N & "Rank_Is_Equal_To_Using_Identical_Rank",
                        Rank_Is_Equal_To_Using_Identical_Rank'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Rank_Is_Equal_To_Using_Equal_Rank",
                        Rank_Is_Equal_To_Using_Equal_Rank'Access));

      Ret.Add_Test (Caller.
                      Create (N & "Rank_IsEqualTo_Using_Identical_Rank",
                        Rank_IsEqualTo_Using_Identical_Rank'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Rank_IsEqualTo_Using_Equal_Rank",
                        Rank_IsEqualTo_Using_Equal_Rank'Access));

      Ret.Add_Test (Caller.
                      Create (N & "Rank_Is_Higher_Than_True",
                        Rank_Is_Higher_Than_True'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Rank_Is_Higher_Than_False",
                        Rank_Is_Higher_Than_False'Access));

      Ret.Add_Test (Caller.
                      Create (N & "Rank_Is_Lower_Than_True",
                        Rank_Is_Lower_Than_True'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Rank_Is_Lower_Than_False",
                        Rank_Is_Lower_Than_False'Access));
      --  equal tests
      Ret.Add_Test (Caller.
                      Create (
                        N & "Is_Equal_To_Identical_Rank_And_Identical_Suit",
                        Is_Equal_To_Identical_Rank_Identical_Suit'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Is_Equal_To_Equal_Rank_And_Identical_Suit",
                        Is_Equal_To_Equal_Rank_Identical_Suit'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Is_Equal_To_Identical_Rank_And_Equal_Suit",
                        Is_Equal_To_Identical_Rank_Equal_Suit'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Is_Equal_To_Equal_Rank_And_Equal_Suit",
                        Is_Equal_To_Equal_Rank_Equal_Suit'Access));

      Ret.Add_Test (Caller.
                      Create (
                        N & "IsEqualTo_Identical_Rank_Identical_Suit",
                        IsEqualTo_Identical_Rank_Identical_Suit'Access));
      Ret.Add_Test (Caller.
                      Create (N & "IsEqualTo_Equal_Rank_Identical_Suit",
                        IsEqualTo_Equal_Rank_Identical_Suit'Access));
      Ret.Add_Test (Caller.
                      Create (N & "IsEqualTo_Identical_Rank_Equal_Suit",
                        IsEqualTo_Identical_Rank_Equal_Suit'Access));
      Ret.Add_Test (Caller.
                      Create (N & "IsEqualTo_Equal_Rank_Equal_Suit",
                        IsEqualTo_Equal_Rank_Equal_Suit'Access));

      Ret.Add_Test (Caller.
                      Create (N & "Suit_IsEqualTo_Using_Identical_Suit",
                        Suit_IsEqualTo_Using_Identical_Suit'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Suit_IsEqualTo_Using_Equal_Suit",
                        Suit_IsEqualTo_Using_Equal_Suit'Access));

      --  suit tests
      Ret.Add_Test (Caller.
                      Create (N & "Suit_Is_Equal_To_Using_Identical_Suit",
                        Suit_Is_Equal_To_Using_Identical_Suit'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Suit_Is_Equal_To_Using_Equal_Suit",
                        Suit_Is_Equal_To_Using_Equal_Suit'Access));

      Ret.Add_Test (Caller.
                      Create (N & "Suit_IsEqualTo_Using_Identical_Suit",
                        Suit_IsEqualTo_Using_Identical_Suit'Access));
      Ret.Add_Test (Caller.
                      Create (N & "SuitIsEqual_To_Using_Equal_Suit",
                        Suit_IsEqualTo_Using_Equal_Suit'Access));

      return Ret;
   end Suite;

end Cards.Test;
