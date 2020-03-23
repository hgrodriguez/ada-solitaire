with AUnit.Test_Suites;
with AUnit.Reporter;
with AUnit.Reporter.Text;
with AUnit.Run;

with Move_Candidate.Test;

procedure Move_Candidate_Test_Main is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      Ret.Add_Test (Move_Candidate.Test.Suite);
      return Ret;
   end Suite;

   procedure Runner is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;

begin
   Runner (Reporter);

end Move_Candidate_Test_Main;
