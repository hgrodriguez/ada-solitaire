with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Stock.Test is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Construct (T : in out Test);

   procedure Fetch_One_Card (T : in out Test);
   procedure Fetch_Two_Cards (T : in out Test);
   procedure Fetch_52_Cards (T : in out Test);
   procedure Fetch_53_Cards (T : in out Test);

end Stock.Test;
