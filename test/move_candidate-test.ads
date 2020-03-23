with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Move_Candidate.Test is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Construct (T : in out Test);

end Move_Candidate.Test;
