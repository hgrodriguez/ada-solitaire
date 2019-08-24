with AUnit.Reporter;
with AUnit.Reporter.Text;
with AUnit.Run;
with AUnit.Test_Suites;

with Card.Test;
with Deck.Test;
with Foundation.Test;
with Foundation_Stack.Test;
with Pile_Of_Cards.FIFO.Test;
with Pile_Of_Cards.LIFO.Test;
with Pile_Of_Cards.Test;
with Short_Image_FIFO.Test;
with Stock.Test;
with Tableau.All_Tests;
with Tableau_Stack.Test;

procedure All_Tests is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
              := new AUnit.Test_Suites.Test_Suite;
   begin
      Ret.Add_Test (Card.Test.Suite);
      Ret.Add_Test (Deck.Test.Suite);
      Ret.Add_Test (Foundation.Test.Suite);
      Ret.Add_Test (Foundation_Stack.Test.Suite);
      Ret.Add_Test (Pile_Of_Cards.FIFO.Test.Suite);
      Ret.Add_Test (Pile_Of_Cards.LIFO.Test.Suite);
      Ret.Add_Test (Pile_Of_Cards.Test.Suite);
      Ret.Add_Test (Short_Image_FIFO.Test.Suite);
      Ret.Add_Test (Stock.Test.Suite);
      Ret.Add_Test (Tableau.All_Tests.Suite);
      Ret.Add_Test (Tableau_Stack.Test.Suite);
      return Ret;
   end Suite;

   procedure Runner is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Runner (Reporter);

end All_Tests;
