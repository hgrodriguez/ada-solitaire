with AUnit.Test_Suites;
with AUnit.Reporter;
with AUnit.Reporter.Text;
with AUnit.Run;

with Tableau.Test;
with Tableau.Tests4_Remove_Mandatory_Cards;
with Tableau.Tests4_To_String;
with Tableau.Tests4_To_String_One_Line;

procedure Tableau_Test_Main is
   function Suite return AUnit.Test_Suites.Access_Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      Ret.Add_Test (Tableau.Test.Suite);
      Ret.Add_Test (Tableau.Tests4_Remove_Mandatory_Cards.Suite);
      Ret.Add_Test (Tableau.Tests4_To_String.Suite);
      Ret.Add_Test (Tableau.Tests4_To_String_One_Line.Suite);
      return Ret;
   end Suite;

   procedure Runner is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;

begin
   Runner (Reporter);

end Tableau_Test_Main;
