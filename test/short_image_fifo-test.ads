with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Short_Image_FIFO.Test is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Construct (T : in out Test);

   procedure Construct_Is_Empty (T : in out Test);
   procedure Construct_Size (T : in out Test);
   procedure Construct_Check_Get_Fails (T : in out Test);

   procedure Put_1_Card_Is_Empty (T : in out Test);
   procedure Put_1_Card_Size (T : in out Test);
   procedure Put_2_Cards_Is_Empty (T : in out Test);
   procedure Put_2_Cards_Size (T : in out Test);

   procedure Get_1_Card_Put_0 (T : in out Test);
   procedure Get_1_Card_Put_1 (T : in out Test);
   procedure Get_1_Card_Put_2 (T : in out Test);
   procedure Get_2_Cards_Put_2 (T : in out Test);

   procedure Ansi_Get_1_Card_Put_1_Red (T : in out Test);
   procedure Ansi_Get_1_Card_Put_1_Black (T : in out Test);
   procedure Ansi_Get_1_Card_Put_2_Red (T : in out Test);
   procedure Ansi_Get_1_Card_Put_2_Black (T : in out Test);
   procedure Ansi_Get_2_Cards_Put_2_Red_Red (T : in out Test);
   procedure Ansi_Get_2_Cards_Put_2_Red_Black (T : in out Test);
   procedure Ansi_Get_2_Cards_Put_2_Black_Red (T : in out Test);
   procedure Ansi_Get_2_Cards_Put_2_Black_Black (T : in out Test);

end Short_Image_FIFO.Test;
