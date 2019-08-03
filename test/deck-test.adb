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
      R : String (1 .. 1);

   begin
      R := Deck.Short_Image (Ace);
      AUnit.Assertions.Assert (R = "A", "A /= " & R);
      R := Deck.Short_Image (Two);
      AUnit.Assertions.Assert (R = "2", "2 /= " & R);
      R := Deck.Short_Image (Three);
      AUnit.Assertions.Assert (R = "3", "3 /= " & R);
      R := Deck.Short_Image (Four);
      AUnit.Assertions.Assert (R = "4", "4 /= " & R);
      R := Deck.Short_Image (Five);
      AUnit.Assertions.Assert (R = "5", "5 /= " & R);
      R := Deck.Short_Image (Six);
      AUnit.Assertions.Assert (R = "6", "6 /= " & R);
      R := Deck.Short_Image (Seven);
      AUnit.Assertions.Assert (R = "7", "7 /= " & R);
      R := Deck.Short_Image (Eight);
      AUnit.Assertions.Assert (R = "8", "8 /= " & R);
      R := Deck.Short_Image (Nine);
      AUnit.Assertions.Assert (R = "9", "9 /= " & R);
      R := Deck.Short_Image (Ten);
      AUnit.Assertions.Assert (R = "T", "T /= " & R);
      R := Deck.Short_Image (Jack);
      AUnit.Assertions.Assert (R = "J", "J /= " & R);
      R := Deck.Short_Image (Queen);
      AUnit.Assertions.Assert (R = "Q", "Q /= " & R);
      R := Deck.Short_Image (King);
      AUnit.Assertions.Assert (R = "K", "K /= " & R);
   end Test_Short_Image_Ranks;

   procedure Test_Short_Image_Suits (T : in out Test) is
      pragma Unreferenced (T);
      S : String (1 .. 1);
   begin
      S := Deck.Short_Image (Diamond);
      AUnit.Assertions.Assert (S = "D", "D /= " & S);
      S := Deck.Short_Image (Heart);
      AUnit.Assertions.Assert (S = "H", "H /= " & S);
      S := Deck.Short_Image (Club);
      AUnit.Assertions.Assert (S = "C", "C /= " & S);
      S := Deck.Short_Image (Spade);
      AUnit.Assertions.Assert (S = "S", "S /= " & S);
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
