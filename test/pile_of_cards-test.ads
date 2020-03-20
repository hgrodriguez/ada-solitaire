with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Pile_Of_Cards.Test is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;
private
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Construct (T : in out Test);

   procedure Put_One_Card_FIFO (T : in out Test);
   procedure Put_Two_Cards_FIFO (T : in out Test);
   procedure Get_One_Card_FIFO (T : in out Test);
   procedure Get_One_Card_Put_Two_FIFO (T : in out Test);
   procedure Get_Two_Cards_Put_Two_FIFO (T : in out Test);
   procedure Get_One_Card_No_Put_FIFO (T : in out Test);

   procedure Push_One_Card_LIFO (T : in out Test);
   procedure Push_Two_Cards_LIFO (T : in out Test);
   procedure Pop_One_Pushed_One_LIFO (T : in out Test);
   procedure Pop_One_Pushed_Two_LIFO (T : in out Test);
   procedure Pop_Two_Pushed_Two_LIFO (T : in out Test);
   procedure Pop_One_Pushed_None_LIFO (T : in out Test);
   procedure Pop_Two_Pushed_One_LIFO (T : in out Test);

   procedure Peek_No_Cards (T : in out Test);

   procedure Peek_One_Card_FIFO (T : in out Test);
   procedure Peek_Two_Cards_FIFO (T : in out Test);

   procedure Peek_One_Card_LIFO (T : in out Test);
   procedure Peek_Two_Cards_LIFO (T : in out Test);

   procedure Has_Not_No_Cards (T : in out Test);

   procedure Has_Not_1_Card_FIFO (T : in out Test);
   procedure Has_Not_2_Cards_FIFO (T : in out Test);
   procedure Has_1_Card_FIFO (T : in out Test);
   procedure Has_2_Cards_1st_Put_FIFO (T : in out Test);
   procedure Has_2_Cards_2nd_Put_FIFO (T : in out Test);

   procedure Has_Not_1_Card_LIFO (T : in out Test);
   procedure Has_Not_2_Cards_LIFO (T : in out Test);
   procedure Has_1_Card_LIFO (T : in out Test);
   procedure Has_2_Cards_1st_Push_LIFO (T : in out Test);
   procedure Has_2_Cards_2nd_Push_LIFO (T : in out Test);
--
end Pile_Of_Cards.Test;
