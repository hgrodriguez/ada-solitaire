with AUnit.Test_Suites;
with AUnit.Reporter;
with AUnit.Reporter.Text;
with AUnit.Run;

with Tableau_Stack.Test;

procedure Tableau_Stack_Test_Main is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      Ret.Add_Test (Tableau_Stack.Test.Suite);
      return Ret;
   end Suite;

   procedure Runner is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;

begin
   Runner (Reporter);

end Tableau_Stack_Test_Main;