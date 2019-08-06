with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Tableau.Tests4_Remove_Mandatory_Cards is
   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   --  pattern of procedure naming:
   --  RMC is short for Remove_Mandatory_Cards
   --  0/1/2/3/4_C: number of candidates
   --  0/1/2/3/4_R: number of cards removed from Tableau
   --  Overview of test cases/possible combinations
   --  The empty case must be tested, too.
   --  Candidates -> |0|1|2|3|4|
   --  Removals      | | | | | |
   --      |         | | | | | |
   --      v         | | | | | |
   --      E         |X|x|x|x|x|
   --      0         |X|x|x|x|x|
   --      1         | |x|x|x|x|
   --      2         | | |x|x|x|
   --      3         | | | |x|x|
   --      4         | | | | |x|
   --
   procedure RMC_0_C_Tab_Empty (T : in out Test);
   procedure RMC_1_C_0_R_Tab_Empty (T : in out Test);
   procedure RMC_2_C_0_R_Tab_Empty (T : in out Test);
   procedure RMC_3_C_0_R_Tab_Empty (T : in out Test);
   procedure RMC_4_C_0_R_Tab_Empty (T : in out Test);

   procedure RMC_0_C_Tab_Not_Empty (T : in out Test);

   procedure RMC_1_C_0_R_Tab_Not_Empty (T : in out Test);
   procedure RMC_1_C_1_R_Tab_Not_Empty (T : in out Test);

   procedure RMC_2_C_0_R_Tab_Not_Empty (T : in out Test);
   procedure RMC_2_C_1_R_Tab_Not_Empty (T : in out Test);
   procedure RMC_2_C_2_R_Tab_Not_Empty (T : in out Test);

   procedure RMC_3_C_0_R_Tab_Not_Empty (T : in out Test);
   procedure RMC_3_C_1_R_Tab_Not_Empty (T : in out Test);
   procedure RMC_3_C_2_R_Tab_Not_Empty (T : in out Test);
   procedure RMC_3_C_3_R_Tab_Not_Empty (T : in out Test);

   procedure RMC_4_C_0_R_Tab_Not_Empty (T : in out Test);
   procedure RMC_4_C_1_R_Tab_Not_Empty (T : in out Test);
   procedure RMC_4_C_2_R_Tab_Not_Empty (T : in out Test);
   procedure RMC_4_C_3_R_Tab_Not_Empty (T : in out Test);
   procedure RMC_4_C_4_R_Tab_Not_Empty (T : in out Test);
end Tableau.Tests4_Remove_Mandatory_Cards;
