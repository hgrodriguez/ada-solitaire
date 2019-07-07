with AUnit.Test_Suites;
with AUnit.Reporter;
with AUnit.Reporter.Text;
with AUnit.Run;

With Card.Test;

procedure Card_Test_Main is
   
   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
            := new AUnit.Test_Suites.Test_Suite;
   begin
      Ret.Add_Test (Card.Test.Suite);
      return Ret;
   end Suite;
   
   procedure Runner is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;

begin
   Runner (Reporter);
end Card_Test_Main;
