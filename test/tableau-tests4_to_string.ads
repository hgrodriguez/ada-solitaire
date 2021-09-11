with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Tableau.Tests4_To_String is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   overriding
   procedure Set_Up (T : in out Test);

   procedure To_String_Empty_Tableau (T : in out Test);

   procedure To_String_Pattern_1 (T : in out Test);
   procedure To_String_Pattern_2 (T : in out Test);

   procedure Ansi_To_String_Empty_Tableau (T : in out Test);
   procedure Ansi_To_String_Pattern_1 (T : in out Test);
   procedure Ansi_To_String_Pattern_2 (T : in out Test);

end Tableau.Tests4_To_String;
