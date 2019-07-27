package body Pile_Of_Cards.Test is

   --------------------------------------------------------------------
   --  all test procedures

   --------------------------------------------------------------------
   --  the test suit construction
   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
             := new AUnit.Test_Suites.Test_Suite;
   begin
      return Ret;
   end Suite;

end Pile_Of_Cards.Test;
