with Pile_Of_Cards;

with AUnit.Assertions;
with AUnit.Test_Caller;
with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package body Pile_Of_Cards.Test is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   --------------------------------------------------------------------
   -- all test procedures
   
   -- test constructing a pile
  procedure Construct(T : in out Test) is
     pragma Unreferenced (T);
     pile : Pile_Of_Cards.Pile_Type;
     
  begin
     pile := Pile_Of_Cards.Construct;
     AUNit.Assertions.Assert (pile.Is_Empty, "should be empty!");
  end Construct;
   
   
   
   --------------------------------------------------------------------
   -- the test suit construction
   package Caller is new AUnit.Test_Caller (Pile_Of_Cards.Test.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
             := new AUnit.Test_Suites.Test_Suite;
   begin
      -- ctor tests
      Ret.Add_Test (Caller.
                      Create("Pile_Of_Cards.Construct",
                        Construct'Access));
      
      return Ret;
   end Suite;

end Pile_Of_Cards.Test;
