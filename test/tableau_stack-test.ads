with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Tableau_Stack.Test is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;
private
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Construct (T : in out Test);
   procedure Construct_Check_Size (T : in out Test);
   procedure Construct_Check_Is_Empty (T : in out Test);
   procedure Construct_Check_Pop_Fails (T : in out Test);

   procedure Empty_Stack_Push_King_Check_Size (T : in out Test);
   procedure Empty_Stack_Push_King_Check_Pop (T : in out Test);
   procedure Empty_Stack_Push_Other_Than_King (T : in out Test);
   procedure Push_Multiple_Cards_Check_Size (T : in out Test);

   procedure Empty_Stack_Accepts_All_Kings (T : in out Test);
   procedure Bottom_Ace_Does_Not_Accept_AnyCard (T : in out Test);
   procedure Bottom_Ten_Accepts_Two_Nines (T : in out Test);

end Tableau_Stack.Test;
