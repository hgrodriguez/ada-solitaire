with Deck;

with AUnit.Assertions;
with AUnit.Test_Caller;
with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package body Deck.Test is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;


   
   procedure Test_Ctor (T : in out Test) is
      pragma Unreferenced (T);
      DT : Deck_Type;
      
   begin
      DT := Deck.Ctor;
      
      -- AUNit.Assertions.Assert ( DT /= null, "Wrong image for 0");
   end Test_Ctor;

      package Caller is new AUnit.Test_Caller (Deck.Test.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
   begin
      Ret.Add_Test
        (Caller.Create ("TEst.Deck.Test_Ctor", Test_Ctor'Access));
      return Ret;
   end Suite;

end Deck.Test;
