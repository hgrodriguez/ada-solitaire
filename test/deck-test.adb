with Deck;

with AUnit.Assertions;
with Ada.Exceptions;
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

   procedure Get_Rank_From_Short_Image (T : in out Test) is
      pragma Unreferenced (T);
      SI                            : Deck.Short_Image_Rank_Type;
      Expected_Ranks                : constant
        array (Deck.Rank_Type_Valid_Range)
        of Deck.Rank_Type_Valid_Range := (Ace,
                                          Two,
                                          Three,
                                          Four,
                                          Five,
                                          Six,
                                          Seven,
                                          Eight,
                                          Nine,
                                          Ten,
                                          Jack,
                                          Queen,
                                          King);
      Actual_Rank                   : Deck.Rank_Type_Valid_Range;
   begin
      for R in Deck.Rank_Type_Valid_Range loop
         SI := Deck.Short_Image (R);
         Actual_Rank := Deck.Get_Rank_For_Short_Image (SI);
         AUnit.Assertions.Assert (Expected_Ranks (R) = Actual_Rank,
                                  "Expected_Rank= " & Expected_Ranks (R)'Image &
                                    " /= Actual_Suit" & Actual_Rank'Image);
      end loop;
   end Get_Rank_From_Short_Image;

   procedure Get_Rank_From_Wrong_Short_Image_Exception;
   procedure Get_Rank_From_Wrong_Short_Image_Exception is
      Actual_Rank    : Deck.Rank_Type_Valid_Range;
      pragma Warnings (Off, Actual_Rank);
   begin
      Actual_Rank := Deck.Get_Rank_For_Short_Image ("X");
   exception
      when Deck_Invalid_Short_Image_Rank_Type => raise;
      when Exc : others =>
         AUnit.
           Assertions.
             Assert (False,
                     "Get_Rank_From_Wrong_Short_Image_Exception: " &
                       "wrong exception raised:" &
                       Ada.Exceptions.Exception_Name (Exc));
   end Get_Rank_From_Wrong_Short_Image_Exception;

   procedure Get_Rank_From_Wrong_Short_Image (T : in out Test) is
      pragma Unreferenced (T);

   begin
      AUnit.
        Assertions.
          Assert_Exception (Get_Rank_From_Wrong_Short_Image_Exception'Access,
                            "Pop_One_Pushed_None: " &
                              "no exception raised");
   end Get_Rank_From_Wrong_Short_Image;

   procedure Get_Suit_From_Short_Image (T : in out Test) is
      pragma Unreferenced (T);
      SI             : Deck.Short_Image_Suit_Type;
      Expected_Suits : constant
        array (Deck.Suit_Type_Valid_Range)
        of Deck.Suit_Type_Valid_Range := (Deck.Diamond,
                                          Deck.Heart,
                                          Deck.Club,
                                          Deck.Spade);
      Actual_Suit    : Deck.Suit_Type_Valid_Range;
   begin
      for S in Deck.Suit_Type_Valid_Range loop
         SI := Deck.Short_Image (S);
         Actual_Suit := Deck.Get_Suit_For_Short_Image (SI);
         AUnit.Assertions.Assert (Expected_Suits (S) = Actual_Suit,
                                  "Expected_Suit= " & Expected_Suits (S)'Image &
                                    " /= Actual_Suit" & Actual_Suit'Image);
      end loop;
   end Get_Suit_From_Short_Image;

   procedure Get_Suit_From_Wrong_Short_Image_Exception;
   procedure Get_Suit_From_Wrong_Short_Image_Exception is
      Actual_Suit    : Deck.Suit_Type_Valid_Range;
      pragma Warnings (Off, Actual_Suit);
   begin
      Actual_Suit := Deck.Get_Suit_For_Short_Image ("X");
   exception
      when Deck_Invalid_Short_Image_Suit_Type => raise;
      when Exc : others =>
         AUnit.
           Assertions.
             Assert (False,
                     "Get_Suit_From_Wrong_Short_Image_Exception: " &
                       "wrong exception raised:" &
                       Ada.Exceptions.Exception_Name (Exc));
   end Get_Suit_From_Wrong_Short_Image_Exception;

   procedure Get_Suit_From_Wrong_Short_Image (T : in out Test) is
      pragma Unreferenced (T);

   begin
      AUnit.
        Assertions.
          Assert_Exception (Get_Suit_From_Wrong_Short_Image_Exception'Access,
                            "Pop_One_Pushed_None: " &
                              "no exception raised");
   end Get_Suit_From_Wrong_Short_Image;

   --------------------------------------------------------------------
   --  test suite construction
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

      Ret.Add_Test (Caller.Create (N & "Get_Rank_From_Short_Image",
                    Get_Rank_From_Short_Image'Access));
      Ret.Add_Test (Caller.Create (N & "Get_Rank_From_Wrong_Short_Image",
                    Get_Rank_From_Wrong_Short_Image'Access));

      Ret.Add_Test (Caller.Create (N & "Get_Suit_From_Short_Image",
                    Get_Suit_From_Short_Image'Access));
      Ret.Add_Test (Caller.Create (N & "Get_Suit_From_Wrong_Short_Image",
                    Get_Suit_From_Wrong_Short_Image'Access));

      return Ret;
   end Suite;

end Deck.Test;
