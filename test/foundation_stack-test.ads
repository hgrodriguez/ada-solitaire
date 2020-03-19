with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Foundation_Stack.Test is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;
private
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Construct_Diamond (T : in out Test);
   procedure Test_Construct_Club (T : in out Test);
   procedure Test_Construct_Heart (T : in out Test);
   procedure Test_Construct_Spade (T : in out Test);
   procedure Newly_Constructed_Is_Empty (T : in out Test);
   procedure Newly_Constructed_Accepts_Ace (T : in out Test);
   procedure Newly_Constructed_Is_Size_0 (T : in out Test);

   procedure Push_Newly_Constructed_OK_Size (T : in out Test);
   procedure Push_Newly_Constructed_OK_Accept (T : in out Test);
   procedure Push_All_OK_Size (T : in out Test);
   procedure Push_All_OK_Accept (T : in out Test);

   procedure Stack_Is_Full  (T : in out Test);

   procedure Full_Stack_Does_Not_Accept_Anything (T : in out Test);

   procedure Push_Wrong_Card_Wrong_Suit (T : in out Test);
   procedure Push_Wrong_Card_Wrong_Rank (T : in out Test);

   procedure To_String_Empty (T : in out Test);
   procedure To_String_Not_Empty  (T : in out Test);

end Foundation_Stack.Test;
