with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Tableau.Tests4_To_String_One_Line is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   --------------------------------------------------------------------
   --  Test for the To_String_One_Line
   procedure To_String_1_Line_Empty (T : in out Test);

   --------------------------------------------------------------------
   --  Test for the To_String_One_Line: only first line tests
   procedure To_String_1_Line_1st_Line_Only_1st_Has_Contents (T : in out Test);
   procedure To_String_1_Line_1st_Line_Only_2nd_Has_Contents (T : in out Test);
   procedure To_String_1_Line_1st_Line_Only_3rd_Has_Contents (T : in out Test);
   procedure To_String_1_Line_1st_Line_Only_4th_Has_Contents (T : in out Test);
   procedure To_String_1_Line_1st_Line_Only_5th_Has_Contents (T : in out Test);
   procedure To_String_1_Line_1st_Line_Only_6th_Has_Contents (T : in out Test);
   procedure To_String_1_Line_1st_Line_Only_7th_Has_Contents (T : in out Test);

end Tableau.Tests4_To_String_One_Line;
