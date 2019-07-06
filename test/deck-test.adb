with Deck;

with AUnit.Assertions;
with AUnit.Test_Caller;
with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package body Deck.Test is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Is_Red_Diamond(T : in out Test) is
      pragma Unreferenced (T);
      is_red : Boolean;
      
   begin
      is_red := Deck.Is_Red(Diamond);
      AUNit.Assertions.Assert (is_red, "Diamond should be red!");
   end Test_Is_Red_Diamond;

   procedure Test_Is_Red_Heart(T : in out Test) is
      pragma Unreferenced (T);
      is_red : Boolean;
      
   begin
      is_red := Deck.Is_Red(Heart);
      AUNit.Assertions.Assert (is_red, "Heart should be red!");
   end Test_Is_Red_Heart;

   procedure Test_Is_Black_Clubs(T : in out Test) is
      pragma Unreferenced (T);
      is_black : Boolean;
      
   begin
      is_black := Deck.Is_Black(Club);
      AUNit.Assertions.Assert (is_black, "Club should be black!");
   end Test_Is_Black_Clubs;

   procedure Test_Is_Black_Spade(T : in out Test) is
      pragma Unreferenced (T);
      is_black : Boolean;
      
   begin
      is_black := Deck.Is_Black(Spade);
      AUNit.Assertions.Assert (is_black, "Spade should be red!");
   end Test_Is_Black_Spade;

   package Caller is new AUnit.Test_Caller (Deck.Test.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
             := new AUnit.Test_Suites.Test_Suite;
   begin
      Ret.Add_Test
        (Caller.Create ("Deck.Test_Is_Red:Diamond", Test_Is_Red_Diamond'Access));
      Ret.Add_Test
        (Caller.Create ("Deck.Test_Is_Red:Heart", Test_Is_Red_Heart'Access));
      Ret.Add_Test
        (Caller.Create ("Deck.Test_Is_Black:Club", Test_Is_Black_Clubs'Access));
      Ret.Add_Test
        (Caller.Create ("Deck.Test_Is_Black Spade", Test_Is_Black_Spade'Access));
      return Ret;
   end Suite;

end Deck.Test;
