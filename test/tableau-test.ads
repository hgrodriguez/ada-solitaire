with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Tableau.Test is
   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Construct (T : in out Test);
   procedure Construct_Check_Size (T : in out Test);
   procedure Construct_Check_Pop_Fails (T : in out Test);

   procedure Init_Check_Overall_Size (T : in out Test);
   procedure Init_Check_Individual_Sizes (T : in out Test);

   procedure Add_1_Card_Check_Size (T : in out Test);
   procedure Add_3_Cards_Check_Size (T : in out Test);
   procedure Add_7_Cards_Check_Size (T : in out Test);

   procedure Add_1_Card_Check_Stack (T : in out Test);
   procedure Add_3_Cards_Check_Stack (T : in out Test);
   procedure Add_7_Cards_Check_Stack (T : in out Test);

   procedure Add_1_Card_Check_Pop (T : in out Test);
   procedure Add_3_Cards_Check_Pop (T : in out Test);
   procedure Add_7_Cards_Check_Pop (T : in out Test);

   procedure Move_To_Src_Stack_Equals_Trgt_Stack (T : in out Test);
   procedure Move_To_Source_Selection_Does_Not_Exist (T : in out Test);
   procedure Move_To_1_Trgt_Does_Not_Accept (T : in out Test);
   procedure Move_To_1_Src_Not_E_Trgt_E (T : in out Test);
   procedure Move_To_1_Src_Not_E_Trgt_Not_E (T : in out Test);
   procedure Move_To_1_Src_E_Trgt_E (T : in out Test);
   procedure Move_To_1_Src_E_Trgt_Not_E (T : in out Test);

   procedure Move_To_X_Trgt_Does_Not_Accept (T : in out Test);
   procedure Move_To_X_Src_Not_E_Trgt_E (T : in out Test);
   procedure Move_To_X_Src_Not_E_Trgt_Not_E (T : in out Test);
   procedure Move_To_X_Src_E_Trgt_Not_E (T : in out Test);

end Tableau.Test;
