with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Stock.Test is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Construct (T : in out Test);

   procedure Fetch_Once (T : in out Test);
   procedure Fetch_Twice (T : in out Test);
   procedure Fetch_Thrice (T : in out Test);
   procedure Fetch_4_Times (T : in out Test);
   procedure Fetch_5_Times (T : in out Test);
   procedure Fetch_6_Times (T : in out Test);
   procedure Fetch_7_Times (T : in out Test);
   procedure Fetch_8_Times (T : in out Test);
   procedure Fetch_9_Times (T : in out Test);

   procedure To_String (T : in out Test);
end Stock.Test;
