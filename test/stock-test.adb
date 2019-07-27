package body Stock.Test is

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
             := new AUnit.Test_Suites.Test_Suite;
   begin
--        Ret.Add_Test (Caller.Create ("Deck.Test_Is_Red:Diamond",
--                                   Test_Is_Red_Diamond'Access));
--        Ret.Add_Test (Caller.Create ("Deck.Test_Is_Red:Heart",
--                                   Test_Is_Red_Heart'Access));
--        Ret.Add_Test (Caller.Create ("Deck.Test_Is_Black:Club",
--                                   Test_Is_Black_Clubs'Access));
--        Ret.Add_Test (Caller.Create ("Deck.Test_Is_Black Spade",
--                                   Test_Is_Black_Spade'Access));
      return Ret;
   end Suite;

end Stock.Test;
