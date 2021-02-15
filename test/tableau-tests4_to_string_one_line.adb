with AUnit.Assertions;
with AUnit.Test_Caller;

with Definitions;

package body Tableau.Tests4_To_String_One_Line is

   function Create_Empty_Stack_Images return Stack_Images;

   procedure To_String_1_Line_Empty (T : in out Test) is
      pragma Unreferenced (T);
      SI       : constant Stack_Images := Create_Empty_Stack_Images;
      Expected : constant One_Line_String := EMPTY_ONE_LINE;
      Actual   : One_Line_String;
   begin
      Actual := To_String_One_Line (SI);
      AUnit.Assertions.Assert (Expected = Actual,
                               "Expected= " & String (Expected) &
                                 " /= Actual=" & String (Actual));
   end To_String_1_Line_Empty;

   function Create_Stack_Images_1stLine_For (Sel : Valid_Stacks_Range)
                                             return Stack_Images;

   First_Line_Card       : constant Card.Card_Type
     := Card.Construct (Definitions.Ace,
                        Definitions.Diamond);
   First_Line_Card_Image : constant Card.Short_Image_Type
     := Card.Short_Image (First_Line_Card);

   procedure To_String_1_Line_1st_Line_Only_X_Has_Contents
     (X : Valid_Stacks_Range);
   procedure To_String_1_Line_1st_Line_Only_X_Has_Contents
     (X : Valid_Stacks_Range) is
      Idx      : constant Integer := (Integer (X) - 1) * 3 + 1;
      SI       : constant Stack_Images
        := Create_Stack_Images_1stLine_For (X);
      Expected : One_Line_String := EMPTY_ONE_LINE;
      Actual   : One_Line_String;
   begin
      Expected (Idx) := First_Line_Card_Image (1);
      Expected (Idx + 1) := First_Line_Card_Image (2);
      Actual := To_String_One_Line (SI);
      AUnit.Assertions.Assert (Expected = Actual,
                               "Expected= " & String (Expected) &
                                 " /= Actual=" & String (Actual));
   end To_String_1_Line_1st_Line_Only_X_Has_Contents;

   procedure To_String_1_Line_1st_Line_Only_One_Has_Contents
     (T : in out Test) is
      pragma Unreferenced (T);
   begin
      for J in Valid_Stacks_Range loop
         To_String_1_Line_1st_Line_Only_X_Has_Contents (J);
      end loop;
   end To_String_1_Line_1st_Line_Only_One_Has_Contents;

   Second_Line_Card       : constant Card.Card_Type
     := Card.Construct (Definitions.King, Definitions.Spade);
   Second_Line_Card_Image : constant Card.Short_Image_Type
     := Card.Short_Image (Second_Line_Card);

   function Create_Stack_Images_1st2nd_Line_For (Sel : Valid_Stacks_Range)
                                                 return Stack_Images;
   function Create_Stack_Images_1st2nd_Line_For (Sel : Valid_Stacks_Range)
                                                 return Stack_Images is
      Ret_Val : Stack_Images;
   begin
      for J in Valid_Stacks_Range loop
         Ret_Val (J) := new Short_Image_FIFO.Short_Image_FIFO_Type;
         Ret_Val (J).all := Short_Image_FIFO.Construct;
      end loop;
      Ret_Val (Sel).all.Put (First_Line_Card_Image);
      Ret_Val (Sel).all.Put (Second_Line_Card_Image);
      return Ret_Val;
   end Create_Stack_Images_1st2nd_Line_For;

   procedure
   To_String_1_Line_1st2nd_Line_Only_X_Has_Contents (X : Valid_Stacks_Range);
   procedure
   To_String_1_Line_1st2nd_Line_Only_X_Has_Contents (X : Valid_Stacks_Range) is
      Idx      : constant Integer := (Integer (X) - 1) * 3 + 1;
      SI       : constant Stack_Images
        := Create_Stack_Images_1st2nd_Line_For (X);
      Expected : One_Line_String := EMPTY_ONE_LINE;
      Actual   : One_Line_String;
      pragma Warnings (Off, Actual);
   begin
      Actual := To_String_One_Line (SI);
      Expected (Idx) := Second_Line_Card_Image (1);
      Expected (Idx + 1) := Second_Line_Card_Image (2);
      Actual := To_String_One_Line (SI);
      AUnit.Assertions.Assert (Expected = Actual,
                               "Expected= " & String (Expected) &
                                 " /= Actual=" & String (Actual));
   end To_String_1_Line_1st2nd_Line_Only_X_Has_Contents;

   procedure
   To_String_1_Line_1st2nd_Line_Only_One_Has_Contents (T : in out Test) is
      pragma Unreferenced (T);
   begin
      for J in Valid_Stacks_Range loop
         To_String_1_Line_1st2nd_Line_Only_X_Has_Contents (J);
      end loop;
   end To_String_1_Line_1st2nd_Line_Only_One_Has_Contents;

   procedure
   To_String_1_Line_1st2nd_Line_Arbitrary_Contents (T : in out Test) is
      pragma Unreferenced (T);
      Idx       : Valid_Stacks_Range;
      SI        : constant Stack_Images := Create_Stack_Images;
      Expected1 : One_Line_String := EMPTY_ONE_LINE;
      Expected2 : One_Line_String := EMPTY_ONE_LINE;
      Actual    : One_Line_String;
      pragma Warnings (Off, Actual);
   begin
      --  Construct first line
      Idx := 1;
      SI (Idx).all.Put (First_Line_Card_Image);
      Expected1 ((Integer (Idx) - 1) * 3 + 1) := First_Line_Card_Image (1);
      Expected1 ((Integer (Idx) - 1) * 3 + 2) := First_Line_Card_Image (2);

      Idx := 3;
      SI (Idx).all.Put (First_Line_Card_Image);
      Expected1 ((Integer (Idx) - 1) * 3 + 1) := First_Line_Card_Image (1);
      Expected1 ((Integer (Idx) - 1) * 3 + 2) := First_Line_Card_Image (2);

      Idx := 6;
      SI (Idx).all.Put (First_Line_Card_Image);
      Expected1 ((Integer (Idx) - 1) * 3 + 1) := First_Line_Card_Image (1);
      Expected1 ((Integer (Idx) - 1) * 3 + 2) := First_Line_Card_Image (2);

      --  Construct second line
      Idx := 3;
      SI (Idx).all.Put (Second_Line_Card_Image);
      Expected2 ((Integer (Idx) - 1) * 3 + 1) := Second_Line_Card_Image (1);
      Expected2 ((Integer (Idx) - 1) * 3 + 2) := Second_Line_Card_Image (2);

      Idx := 6;
      SI (Idx).all.Put (Second_Line_Card_Image);
      Expected2 ((Integer (Idx) - 1) * 3 + 1) := Second_Line_Card_Image (1);
      Expected2 ((Integer (Idx) - 1) * 3 + 2) := Second_Line_Card_Image (2);

      Actual := To_String_One_Line (SI);
      AUnit.Assertions.Assert (Expected1 = Actual,
                               "Expected1= " & String (Expected1) &
                                 " /= Actual=" & String (Actual));

      Actual := To_String_One_Line (SI);
      AUnit.Assertions.Assert (Expected2 = Actual,
                               "Expected2= " & String (Expected2) &
                                 " /= Actual=" & String (Actual));
   end To_String_1_Line_1st2nd_Line_Arbitrary_Contents;

   --------------------------------------------------------------------
   --  the test suit construction
   package Caller is new AUnit.Test_Caller
     (Tableau.Tests4_To_String_One_Line.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
      N   : constant String := "Tableau.Tests4_To_String_One_Line.";
   begin
      --  One_Liners
      Ret.
        Add_Test (Caller.
                    Create (N &
                        "To_String_1_Line_Empty",
                      To_String_1_Line_Empty'Access));

      Ret.
        Add_Test (Caller.
                    Create (N &
                        "To_String_1_Line_1st_Line_Only_One_Has_Contents",
                      To_String_1_Line_1st_Line_Only_One_Has_Contents'Access));

      Ret.
        Add_Test (Caller.
                    Create (N &
                        "To_String_1_Line_1st2nd_Line_Only_One_Has_Contents",
                      To_String_1_Line_1st2nd_Line_Only_One_Has_Contents'Access)
                 );
      Ret.
        Add_Test (Caller.
                    Create (N &
                        "To_String_1_Line_1st2nd_Line_Arbitrary_Contents",
                      To_String_1_Line_1st2nd_Line_Arbitrary_Contents'Access)
                 );

      return Ret;
   end Suite;

   --------------------------------------------------------------------
   --  Helper functions/procedures

   function Create_Empty_Stack_Images return Stack_Images is
   begin
      return Create_Stack_Images;
   end Create_Empty_Stack_Images;

   function Create_Stack_Images_1stLine_For (Sel : Valid_Stacks_Range)
                                             return Stack_Images is
      Ret_Val : Stack_Images;
   begin
      for J in Valid_Stacks_Range loop
         Ret_Val (J) := new Short_Image_FIFO.Short_Image_FIFO_Type;
         Ret_Val (J).all := Short_Image_FIFO.Construct;
      end loop;
      Ret_Val (Sel).all.Put (First_Line_Card_Image);
      return Ret_Val;
   end Create_Stack_Images_1stLine_For;

end Tableau.Tests4_To_String_One_Line;
