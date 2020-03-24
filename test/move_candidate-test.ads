with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Move_Candidate.Test is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Construct (T : in out Test);

   procedure Construct_Check_Card_To_Include (T : in out Test);
   procedure Construct_Check_Stack_Number (T : in out Test);
   procedure Construct_Check_Rank_Delta (T : in out Test);

end Move_Candidate.Test;
