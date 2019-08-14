with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Pile_Of_Cards.FIFO.Test is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;
private
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Construct (T : in out Test);
   procedure Put_One_Card (T : in out Test);
   procedure Put_Two_Cards (T : in out Test);
   procedure Get_One_Card (T : in out Test);
   procedure Get_One_Card_Put_Two (T : in out Test);
   procedure Get_Two_Cards_Put_Two (T : in out Test);
   procedure Get_One_Card_No_Put (T : in out Test);

   procedure Peek_No_Cards (T : in out Test);
   procedure Peek_One_Card (T : in out Test);
   procedure Peek_Two_Cards (T : in out Test);

   procedure Has_Not_No_Cards (T : in out Test);
   procedure Has_Not_1_Card (T : in out Test);
   procedure Has_Not_2_Cards (T : in out Test);
   procedure Has_1_Card (T : in out Test);
   procedure Has_2_Cards_1st_Put (T : in out Test);
   procedure Has_2_Cards_2nd_Put (T : in out Test);
end Pile_Of_Cards.FIFO.Test;
