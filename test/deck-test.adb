with Deck;

with AUnit.Assertions;
with AUnit.Test_Caller;

package body Deck.Test is

   procedure Test_Is_Red_Diamond (T : in out Test) is
      pragma Unreferenced (T);
   begin
      AUnit.Assertions.Assert (Deck.Is_Red (Diamond),
                               "Diamond should be red!");
   end Test_Is_Red_Diamond;

   procedure Test_Is_Red_Heart (T : in out Test) is
      pragma Unreferenced (T);
   begin
      AUnit.Assertions.Assert (Deck.Is_Red (Heart), "Heart should be red!");
   end Test_Is_Red_Heart;

   procedure Test_Is_Black_Clubs (T : in out Test) is
      pragma Unreferenced (T);
   begin
      AUnit.Assertions.Assert (Deck.Is_Black (Club), "Club should be black!");
   end Test_Is_Black_Clubs;

   procedure Test_Is_Black_Spade (T : in out Test) is
      pragma Unreferenced (T);
   begin
      AUnit.Assertions.Assert (Deck.Is_Black (Spade), "Spade should be red!");
   end Test_Is_Black_Spade;

   procedure Test_Short_Image_Ranks (T : in out Test) is
      pragma Unreferenced (T);
      R : Deck.Short_Image_Rank_Type;

   begin
      R := Deck.Short_Image (Ace);
      AUnit.Assertions.Assert (String (R) = "A", "A /= " & String (R));
      R := Deck.Short_Image (Two);
      AUnit.Assertions.Assert (String (R) = "2", "2 /= " & String (R));
      R := Deck.Short_Image (Three);
      AUnit.Assertions.Assert (String (R) = "3", "3 /= " & String (R));
      R := Deck.Short_Image (Four);
      AUnit.Assertions.Assert (String (R) = "4", "4 /= " & String (R));
      R := Deck.Short_Image (Five);
      AUnit.Assertions.Assert (String (R) = "5", "5 /= " & String (R));
      R := Deck.Short_Image (Six);
      AUnit.Assertions.Assert (String (R) = "6", "6 /= " & String (R));
      R := Deck.Short_Image (Seven);
      AUnit.Assertions.Assert (String (R) = "7", "7 /= " & String (R));
      R := Deck.Short_Image (Eight);
      AUnit.Assertions.Assert (String (R) = "8", "8 /= " & String (R));
      R := Deck.Short_Image (Nine);
      AUnit.Assertions.Assert (String (R) = "9", "9 /= " & String (R));
      R := Deck.Short_Image (Ten);
      AUnit.Assertions.Assert (String (R) = "T", "T /= " & String (R));
      R := Deck.Short_Image (Jack);
      AUnit.Assertions.Assert (String (R) = "J", "J /= " & String (R));
      R := Deck.Short_Image (Queen);
      AUnit.Assertions.Assert (String (R) = "Q", "Q /= " & String (R));
      R := Deck.Short_Image (King);
      AUnit.Assertions.Assert (String (R) = "K", "K /= " & String (R));
   end Test_Short_Image_Ranks;

   procedure Test_Short_Image_Suits (T : in out Test) is
      pragma Unreferenced (T);
      S : Deck.Short_Image_Suit_Type;
   begin
      S := Deck.Short_Image (Diamond);
      AUnit.Assertions.Assert (String (S) = "D", "D /= " & String (S));
      S := Deck.Short_Image (Heart);
      AUnit.Assertions.Assert (String (S) = "H", "H /= " & String (S));
      S := Deck.Short_Image (Club);
      AUnit.Assertions.Assert (String (S) = "C", "C /= " & String (S));
      S := Deck.Short_Image (Spade);
      AUnit.Assertions.Assert (String (S) = "S", "S /= " & String (S));
   end Test_Short_Image_Suits;

   package Caller is new AUnit.Test_Caller (Deck.Test.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
      N   : constant String := "Deck.Test.";
   begin
      Ret.Add_Test (Caller.Create (N & "Test_Is_Red:Diamond",
                                 Test_Is_Red_Diamond'Access));
      Ret.Add_Test (Caller.Create (N & "Test_Is_Red:Heart",
                                 Test_Is_Red_Heart'Access));
      Ret.Add_Test (Caller.Create (N & "Test_Is_Black:Club",
                                 Test_Is_Black_Clubs'Access));
      Ret.Add_Test (Caller.Create (N & "Test_Is_Black Spade",
                                 Test_Is_Black_Spade'Access));

      Ret.Add_Test (Caller.Create (N & "Test_Short_Image_Ranks",
                    Test_Short_Image_Ranks'Access));
      Ret.Add_Test (Caller.Create (N & "Test_Short_Image_Suits",
                    Test_Short_Image_Suits'Access));
      return Ret;
   end Suite;

end Deck.Test;
