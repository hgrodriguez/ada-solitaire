with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Tableau.Tests4_Check_Move_To is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Check_Move_To_Src_Stack_Equals_Trgt_Stack (T : in out Test);
   procedure Check_Move_To_Source_Selection_Does_Not_Exist (T : in out Test);
   procedure Check_Move_To_1_Trgt_Does_Not_Accept (T : in out Test);
   procedure Check_Move_To_1_Src_Not_E_Trgt_E (T : in out Test);
   procedure Check_Move_To_1_Src_Not_E_Trgt_Not_E (T : in out Test);
   procedure Check_Move_To_1_Src_E_Trgt_E (T : in out Test);
   procedure Check_Move_To_1_Src_E_Trgt_Not_E (T : in out Test);

   procedure Check_Move_To_X_Trgt_Does_Not_Accept (T : in out Test);
   procedure Check_Move_To_X_Src_Not_E_Trgt_E (T : in out Test);
   procedure Check_Move_To_X_Src_Not_E_Trgt_Not_E (T : in out Test);
   procedure Check_Move_To_X_Src_E_Trgt_Not_E (T : in out Test);

end Tableau.Tests4_Check_Move_To;
