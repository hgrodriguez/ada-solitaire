with Definitions;
with Card;
with Deck; use Deck;

with AUnit.Assertions;
with AUnit.Test_Caller;

package body Card.Test is

   --------------------------------------------------------------------
   --  all test procedures

   --  test constructing a card
   procedure Ctor_Rank (T : in out Test) is
      pragma Unreferenced (T);
      rank : constant Definitions.Rank := Definitions.Ace;
      suit : constant Definitions.Suit := Definitions.Diamond;
      c    : Card.Card_Type;
      use Definitions;
   begin
      c := Card.Construct (rank, suit);
      AUnit.Assertions.Assert (c.Get_Rank = rank,
                               "Rank should be: " & rank'Image &
                                 ", but is:" & c.Get_Rank'Image);
   end Ctor_Rank;

   procedure Ctor_Top_Rank (T : in out Test) is
      pragma Unreferenced (T);
      rank : constant Definitions.Rank := Definitions.Top;
      suit : constant Definitions.Suit := Definitions.Diamond;
      c    : Card.Card_Type;
      use Definitions;
   begin
      c := Card.Construct_Top_Rank (suit);
      AUnit.Assertions.Assert (c.Get_Rank = rank,
                               "Rank should be: " & rank'Image &
                                 ", but is:" & c.Get_Rank'Image);
   end Ctor_Top_Rank;

   procedure Ctor_Suit (T : in out Test) is
      pragma Unreferenced (T);
      rank : constant Definitions.Rank := Definitions.Ace;
      suit : constant Definitions.Suit := Definitions.Diamond;
      c    : Card.Card_Type;
      use Definitions;
   begin
      c := Card.Construct (rank, suit);
      AUnit.Assertions.Assert (c.Get_Suit = suit,
                               "Suit should be: " & suit'Image &
                                 ", but is:" & c.Get_Suit'Image);
   end Ctor_Suit;

   --------------------------------------------------------------------
   --  red suits
   procedure Suit_Is_Red_Diamond (T : in out Test) is
      pragma Unreferenced (T);
      rank : constant Definitions.Rank := Definitions.Ace;
      suit : constant Definitions.Suit := Definitions.Diamond;
      c    : constant Card.Card_Type := Card.Construct (rank, suit);

   begin
      AUnit.Assertions.Assert (c.Suit_Is_Red,
                               "c.Suit:" & c.Get_Suit'Image &
                                 " should be red");
   end Suit_Is_Red_Diamond;

   procedure Suit_Is_Red_Heart (T : in out Test) is
      pragma Unreferenced (T);
      rank : constant Definitions.Rank := Definitions.Ace;
      suit : constant Definitions.Suit := Definitions.Heart;
      c    : constant Card.Card_Type := Card.Construct (rank, suit);

   begin
      AUnit.Assertions.Assert (c.Suit_Is_Red,
                               "c.Suit:" & c.Get_Suit'Image &
                                 " should be red");
   end Suit_Is_Red_Heart;

   --  black suits
   procedure Suit_Is_Black_Club (T : in out Test) is
      pragma Unreferenced (T);
      rank : constant Definitions.Rank := Definitions.Ace;
      suit : constant Definitions.Suit := Definitions.Club;
      c    : constant Card.Card_Type := Card.Construct (rank, suit);

   begin
      AUnit.Assertions.Assert (c.Suit_Is_Black,
                               "c.Suit:" & c.Get_Suit'Image &
                                 " should be black");
   end Suit_Is_Black_Club;

   procedure Suit_Is_Black_Spade (T : in out Test) is
      pragma Unreferenced (T);
      rank : constant Definitions.Rank := Definitions.Ace;
      suit : constant Definitions.Suit := Definitions.Spade;
      c    : constant Card.Card_Type := Card.Construct (rank, suit);

   begin
      AUnit.Assertions.Assert (c.Suit_Is_Black,
                               "c.Suit:" & c.Get_Suit'Image &
                                 " should be black");
   end Suit_Is_Black_Spade;

   --------------------------------------------------------------------
   --  image tests
   procedure Image_1 (T : in out Test) is
      pragma Unreferenced (T);
      rank       : constant Definitions.Rank := Definitions.Ace;
      suit       : constant Definitions.Suit := Definitions.Diamond;
      c          : constant Card.Card_Type := Card.Construct (rank, suit);
      s_expected : constant String
        := "(" & rank'Image & "," & suit'Image & ")";
      s_result   : String (1 .. s_expected'Length);

   begin
      s_result := c.Image;
      AUnit.Assertions.Assert (s_expected = s_result,
                               "expected: " & s_expected &
                                 " /= " & s_result);
   end Image_1;

   procedure Short_Image (T : in out Test) is
      pragma Unreferenced (T);
      C        : Card.Card_Type;
      Expected : Card.Short_Image_Type;
      Actual   : Card.Short_Image_Type;
   begin
      for Rank in Definitions.Ranks_Valid_Range loop
         for Suit in Definitions.Suits_Valid_Range loop
            Expected (1) := Deck.Short_Image (Rank) (1);
            Expected (2) := Deck.Short_Image (Suit) (1);
            C := Card.Construct (Rank, Suit);
            Actual := C.Short_Image;
            AUnit.Assertions.Assert (Expected = Actual,
                                     "expected: " & Expected &
                                       " /= " & Actual);
         end loop;
      end loop;
   end Short_Image;

   procedure Ansi_Image_Red (T : in out Test) is
      pragma Unreferenced (T);
      C        : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Five,
                           Suit => Definitions.Heart);
      Expected : constant Unbounded_String
        := To_Unbounded_String (Definitions.Ansi_Red_Head &
                                  "5H" &
                                  Definitions.Ansi_Red_Tail);
      Actual   : Unbounded_String;
   begin
      Actual := C.Ansi_Image;
      AUnit.
        Assertions.
          Assert (Expected = Actual,
                  "Expected=" & To_String (Expected) &
                      "/=" & To_String (Actual));
   end Ansi_Image_Red;

   procedure Ansi_Image_Black (T : in out Test) is
      pragma Unreferenced (T);
      C        : constant Card.Card_Type
        := Card.Construct (Rank => Definitions.Five,
                           Suit => Definitions.Club);
      Expected : constant Unbounded_String := To_Unbounded_String ("5C");
      Actual   : Unbounded_String;
   begin
      Actual := C.Ansi_Image;
      AUnit.
        Assertions.
          Assert (Expected = Actual,
                  "Expected=" & To_String (Expected) &
                      "/=" & To_String (Actual));
   end Ansi_Image_Black;

   --------------------------------------------------------------------
   --  the test suit construction
   package Caller is new AUnit.Test_Caller (Card.Test.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
      N   : constant String := "Card.Test.";
   begin
      --  c tor tests
      Ret.Add_Test (Caller.
                      Create (N & "Ctor_Rank",
                        Ctor_Rank'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Ctor_Top_Rank",
                        Ctor_Top_Rank'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Ctor_Suit",
                        Ctor_Suit'Access));

      --  suit tests
      Ret.Add_Test (Caller.
                      Create (N & "Suit_Is_Red_Diamond",
                        Suit_Is_Red_Diamond'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Suit_Is_Red_Heart",
                        Suit_Is_Red_Heart'Access));

      Ret.Add_Test (Caller.
                      Create (N & "Suit_Is_Black_Club",
                        Suit_Is_Black_Club'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Suit_Is_Black_Spade",
                        Suit_Is_Black_Spade'Access));

      --  image tests
      Ret.Add_Test (Caller.
                      Create (N & "Image_1",
                        Image_1'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Short_Image",
                        Short_Image'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Ansi_Image_Red",
                        Ansi_Image_Red'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Ansi_Image_Black",
                        Ansi_Image_Black'Access));

      return Ret;
   end Suite;

end Card.Test;
