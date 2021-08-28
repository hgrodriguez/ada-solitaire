with Ada.Exceptions;

with AUnit.Assertions;
with AUnit.Test_Caller;

with Definitions;

with Short_Image_FIFO;

package body Short_Image_FIFO.Test is

   procedure Construct (T : in out Test) is
      pragma Unreferenced (T);
      SIF : Short_Image_FIFO.Short_Image_FIFO_Type
        := Short_Image_FIFO.Construct;
      pragma Warnings (Off, SIF);
   begin
      null;
   end Construct;

   procedure Construct_Is_Empty (T : in out Test) is
      pragma Unreferenced (T);
      SIF      : constant Short_Image_FIFO.Short_Image_FIFO_Type
        := Short_Image_FIFO.Construct;
      Expected : constant Boolean := True;
      Actual   : Boolean;
   begin
      Actual := SIF.Is_Empty;
      AUnit.Assertions.Assert (Expected = Actual,
                               "expected=" & Expected'Image &
                                 " /= actual:" & Actual'Image);
   end Construct_Is_Empty;

   procedure Construct_Size (T : in out Test) is
      pragma Unreferenced (T);
      SIF      : constant Short_Image_FIFO.Short_Image_FIFO_Type
        := Short_Image_FIFO.Construct;
      Expected : constant Natural := 0;
      Actual   : Natural;
   begin
      Actual := SIF.Size;
      AUnit.Assertions.Assert (Expected = Actual,
                               "expected=" & Expected'Image &
                                 " /= actual:" & Actual'Image);
   end Construct_Size;

   procedure Construct_Check_Get_Fails_Exception;
   procedure Construct_Check_Get_Fails_Exception is
      SIF      : Short_Image_FIFO.Short_Image_FIFO_Type
        := Short_Image_FIFO.Construct;
      SI       : Card.Short_Image_Type;
      pragma Warnings (Off, SI);
   begin
      SI := SIF.Get;
   exception
      when Short_Image_FIFO_Empty_Exception => raise;
      when Exc : others =>
         AUnit.
           Assertions.
             Assert (False,
                     "Construct_Check_Pop_Fails_Exception: " &
                       "wrong exception raised:" &
                       Ada.Exceptions.Exception_Name (Exc));
   end Construct_Check_Get_Fails_Exception;

   procedure Construct_Check_Get_Fails (T : in out Test) is
      pragma Unreferenced (T);
   begin
      AUnit.
        Assertions.
          Assert_Exception (Construct_Check_Get_Fails_Exception'Access,
                            "Construct_Check_Get_Fails_Exception: " &
                              "no exception raised");
   end Construct_Check_Get_Fails;

   procedure Put_1_Card_Is_Empty (T : in out Test) is
      pragma Unreferenced (T);
      SIF      : Short_Image_FIFO.Short_Image_FIFO_Type
        := Short_Image_FIFO.Construct;
      C        : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      SI       : constant Card.Short_Image_Type := C.Short_Image;
      Expected : constant Boolean := False;
      Actual   : Boolean;
   begin
      SIF.Put (SI);
      Actual := SIF.Is_Empty;
      AUnit.Assertions.Assert (Expected = Actual,
                               "expected=" & Expected'Image &
                                 " /= actual:" & Actual'Image);
   end Put_1_Card_Is_Empty;

   procedure Put_1_Card_Size (T : in out Test) is
      pragma Unreferenced (T);
      SIF      : Short_Image_FIFO.Short_Image_FIFO_Type
        := Short_Image_FIFO.Construct;
      C        : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      SI       : constant Card.Short_Image_Type := C.Short_Image;
      Expected : constant Natural := 1;
      Actual   : Natural;
   begin
      SIF.Put (SI);
      Actual := SIF.Size;
      AUnit.Assertions.Assert (Expected = Actual,
                               "expected=" & Expected'Image &
                                 " /= actual:" & Actual'Image);
   end Put_1_Card_Size;

   procedure Put_2_Cards_Is_Empty (T : in out Test) is
      pragma Unreferenced (T);
      SIF      : Short_Image_FIFO.Short_Image_FIFO_Type
        := Short_Image_FIFO.Construct;
      C        : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      SI       : constant Card.Short_Image_Type := C.Short_Image;
      Expected : constant Boolean := False;
      Actual   : Boolean;
   begin
      SIF.Put (SI);
      SIF.Put (SI);
      Actual := SIF.Is_Empty;
      AUnit.Assertions.Assert (Expected = Actual,
                               "expected=" & Expected'Image &
                                 " /= actual:" & Actual'Image);
   end Put_2_Cards_Is_Empty;

   procedure Put_2_Cards_Size (T : in out Test) is
      pragma Unreferenced (T);
      SIF      : Short_Image_FIFO.Short_Image_FIFO_Type
        := Short_Image_FIFO.Construct;
      C        : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      SI       : constant Card.Short_Image_Type := C.Short_Image;
      Expected : constant Natural := 2;
      Actual   : Natural;
   begin
      SIF.Put (SI);
      SIF.Put (SI);
      Actual := SIF.Size;
      AUnit.Assertions.Assert (Expected = Actual,
                               "expected=" & Expected'Image &
                                 " /= actual:" & Actual'Image);
   end Put_2_Cards_Size;

   procedure Get_1_Card_Put_0_Exception;
   procedure Get_1_Card_Put_0_Exception is
      SIF      : Short_Image_FIFO.Short_Image_FIFO_Type
        := Short_Image_FIFO.Construct;
      SI       : Card.Short_Image_Type;
      pragma Warnings (Off, SI);
   begin
      SI := SIF.Get;
   exception
      when Short_Image_FIFO_Empty_Exception => raise;
      when Exc : others =>
         AUnit.
           Assertions.
             Assert (False,
                     "Construct_Check_Pop_Fails_Exception: " &
                       "wrong exception raised:" &
                       Ada.Exceptions.Exception_Name (Exc));
   end Get_1_Card_Put_0_Exception;

   procedure Get_1_Card_Put_0 (T : in out Test) is
      pragma Unreferenced (T);
   begin
      AUnit.
        Assertions.
          Assert_Exception (Get_1_Card_Put_0_Exception'Access,
                            "Get_1_Card_Put_0_Exception: " &
                              "no exception raised");
   end Get_1_Card_Put_0;

   procedure Get_1_Card_Put_1 (T : in out Test) is
      pragma Unreferenced (T);
      SIF      : Short_Image_FIFO.Short_Image_FIFO_Type
        := Short_Image_FIFO.Construct;
      C        : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      SI       : constant Card.Short_Image_Type := C.Short_Image;
      Expected : constant Card.Short_Image_Type := SI;
      Actual   : Card.Short_Image_Type;
   begin
      SIF.Put (SI);
      Actual := SIF.Get;
      AUnit.Assertions.Assert (Expected = Actual,
                               "expected=" & Expected &
                                 " /= actual:" & Actual);
   end Get_1_Card_Put_1;

   procedure Get_1_Card_Put_2 (T : in out Test) is
      pragma Unreferenced (T);
      SIF      : Short_Image_FIFO.Short_Image_FIFO_Type
        := Short_Image_FIFO.Construct;
      C1       : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      SI1      : constant Card.Short_Image_Type := C1.Short_Image;
      C2       : constant Card.Card_Type
        := Card.Construct (Definitions.King,
                           Definitions.Spade);
      SI2      : constant Card.Short_Image_Type := C2.Short_Image;
      Expected : constant Card.Short_Image_Type := SI1;
      Actual   : Card.Short_Image_Type;
   begin
      SIF.Put (SI1);
      SIF.Put (SI2);
      Actual := SIF.Get;
      AUnit.Assertions.Assert (Expected = Actual,
                               "expected=" & Expected &
                                 " /= actual:" & Actual);
   end Get_1_Card_Put_2;

   procedure Get_2_Cards_Put_2 (T : in out Test) is
      pragma Unreferenced (T);
      SIF       : Short_Image_FIFO.Short_Image_FIFO_Type
        := Short_Image_FIFO.Construct;
      C1        : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      SI1       : constant Card.Short_Image_Type := C1.Short_Image;
      C2        : constant Card.Card_Type
        := Card.Construct (Definitions.King,
                           Definitions.Spade);
      SI2       : constant Card.Short_Image_Type := C2.Short_Image;
      Expected1 : constant Card.Short_Image_Type := SI1;
      Actual1   : Card.Short_Image_Type;
      Expected2 : constant Card.Short_Image_Type := SI2;
      Actual2   : Card.Short_Image_Type;
   begin
      SIF.Put (SI1);
      SIF.Put (SI2);
      Actual1 := SIF.Get;
      AUnit.Assertions.Assert (Expected1 = Actual1,
                               "expected=" & Expected1 &
                                 " /= actual:" & Actual1);
      Actual2 := SIF.Get;
      AUnit.Assertions.Assert (Expected2 = Actual2,
                               "expected=" & Expected2 &
                                 " /= actual:" & Actual2);
   end Get_2_Cards_Put_2;

   procedure Ansi_Get_1_Card_Put_1_Red (T : in out Test) is
      pragma Unreferenced (T);
      SIF      : Short_Image_FIFO.Short_Image_FIFO_Type
        := Short_Image_FIFO.Construct;
      C        : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      SI       : constant Card.Short_Image_Type := C.Short_Image;
      Expected : constant Unbounded_String := C.Ansi_Image;
      Actual   : Unbounded_String;
   begin
      SIF.Put (SI);
      Actual := SIF.Ansi_Get;
      AUnit.Assertions.Assert (Expected = Actual,
                               "Expected=" & To_String (Expected) &
                                 " /= " & To_String (Actual));
   end Ansi_Get_1_Card_Put_1_Red;

   procedure Ansi_Get_1_Card_Put_1_Black (T : in out Test) is
      pragma Unreferenced (T);
      SIF      : Short_Image_FIFO.Short_Image_FIFO_Type
        := Short_Image_FIFO.Construct;
      C        : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Club);
      SI       : constant Card.Short_Image_Type := C.Short_Image;
      Expected : constant Unbounded_String := C.Ansi_Image;
      Actual   : Unbounded_String;
   begin
      SIF.Put (SI);
      Actual := SIF.Ansi_Get;
      AUnit.Assertions.Assert (Expected = Actual,
                               "Expected=" & To_String (Expected) &
                                 " /= " & To_String (Actual));
   end Ansi_Get_1_Card_Put_1_Black;

   procedure Ansi_Get_1_Card_Put_2_Red (T : in out Test) is
      pragma Unreferenced (T);
      SIF      : Short_Image_FIFO.Short_Image_FIFO_Type
        := Short_Image_FIFO.Construct;
      C1       : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Heart);
      SI1      : constant Card.Short_Image_Type := C1.Short_Image;
      C2       : constant Card.Card_Type
        := Card.Construct (Definitions.King,
                           Definitions.Diamond);
      SI2      : constant Card.Short_Image_Type := C2.Short_Image;
      Expected : constant Unbounded_String := C1.Ansi_Image;
      Actual   : Unbounded_String;
   begin
      SIF.Put (SI1);
      SIF.Put (SI2);
      Actual := SIF.Ansi_Get;
      AUnit.Assertions.Assert (Expected = Actual,
                               "Expected=" & To_String (Expected) &
                                 " /= " & To_String (Actual));
   end Ansi_Get_1_Card_Put_2_Red;

   procedure Ansi_Get_1_Card_Put_2_Black (T : in out Test) is
      pragma Unreferenced (T);
      SIF      : Short_Image_FIFO.Short_Image_FIFO_Type
        := Short_Image_FIFO.Construct;
      C1       : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Club);
      SI1      : constant Card.Short_Image_Type := C1.Short_Image;
      C2       : constant Card.Card_Type
        := Card.Construct (Definitions.King,
                           Definitions.Spade);
      SI2      : constant Card.Short_Image_Type := C2.Short_Image;
      Expected : constant Unbounded_String := C1.Ansi_Image;
      Actual   : Unbounded_String;
   begin
      SIF.Put (SI1);
      SIF.Put (SI2);
      Actual := SIF.Ansi_Get;
      AUnit.Assertions.Assert (Expected = Actual,
                               "Expected=" & To_String (Expected) &
                                 " /= " & To_String (Actual));
   end Ansi_Get_1_Card_Put_2_Black;

   procedure Ansi_Get_2_Cards_Put_2_Red_Red (T : in out Test) is
      pragma Unreferenced (T);
      SIF       : Short_Image_FIFO.Short_Image_FIFO_Type
        := Short_Image_FIFO.Construct;

      C1        : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Diamond);
      SI1       : constant Card.Short_Image_Type := C1.Short_Image;
      Expected1 : constant Unbounded_String := C1.Ansi_Image;
      Actual1   : Unbounded_String;

      C2        : constant Card.Card_Type
        := Card.Construct (Definitions.King,
                           Definitions.Heart);
      SI2       : constant Card.Short_Image_Type := C2.Short_Image;
      Expected2 : constant Unbounded_String := C2.Ansi_Image;
      Actual2   : Unbounded_String;
   begin
      SIF.Put (SI1);
      SIF.Put (SI2);

      Actual1 := SIF.Ansi_Get;
      AUnit.Assertions.Assert (Expected1 = Actual1,
                               "Expected=" & To_String (Expected1) &
                                 " /= " & To_String (Actual1));

      Actual2 := SIF.Ansi_Get;
      AUnit.Assertions.Assert (Expected2 = Actual2,
                               "Expected=" & To_String (Expected2) &
                                 " /= " & To_String (Actual2));
   end Ansi_Get_2_Cards_Put_2_Red_Red;

   procedure Ansi_Get_2_Cards_Put_2_Red_Black (T : in out Test) is
      pragma Unreferenced (T);
      SIF       : Short_Image_FIFO.Short_Image_FIFO_Type
        := Short_Image_FIFO.Construct;

      C1        : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Heart);
      SI1       : constant Card.Short_Image_Type := C1.Short_Image;
      Expected1 : constant Unbounded_String := C1.Ansi_Image;
      Actual1   : Unbounded_String;

      C2        : constant Card.Card_Type
        := Card.Construct (Definitions.King,
                           Definitions.Spade);
      SI2       : constant Card.Short_Image_Type := C2.Short_Image;
      Expected2 : constant Unbounded_String := C2.Ansi_Image;
      Actual2   : Unbounded_String;
   begin
      SIF.Put (SI1);
      SIF.Put (SI2);

      Actual1 := SIF.Ansi_Get;
      AUnit.Assertions.Assert (Expected1 = Actual1,
                               "Expected=" & To_String (Expected1) &
                                 " /= " & To_String (Actual1));

      Actual2 := SIF.Ansi_Get;
      AUnit.Assertions.Assert (Expected2 = Actual2,
                               "Expected=" & To_String (Expected2) &
                                 " /= " & To_String (Actual2));
   end Ansi_Get_2_Cards_Put_2_Red_Black;

   procedure Ansi_Get_2_Cards_Put_2_Black_Red (T : in out Test) is
      pragma Unreferenced (T);
      SIF       : Short_Image_FIFO.Short_Image_FIFO_Type
        := Short_Image_FIFO.Construct;

      C1        : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Club);
      SI1       : constant Card.Short_Image_Type := C1.Short_Image;
      Expected1 : constant Unbounded_String := C1.Ansi_Image;
      Actual1   : Unbounded_String;

      C2        : constant Card.Card_Type
        := Card.Construct (Definitions.King,
                           Definitions.Heart);
      SI2       : constant Card.Short_Image_Type := C2.Short_Image;
      Expected2 : constant Unbounded_String := C2.Ansi_Image;
      Actual2   : Unbounded_String;
   begin
      SIF.Put (SI1);
      SIF.Put (SI2);

      Actual1 := SIF.Ansi_Get;
      AUnit.Assertions.Assert (Expected1 = Actual1,
                               "Expected=" & To_String (Expected1) &
                                 " /= " & To_String (Actual1));

      Actual2 := SIF.Ansi_Get;
      AUnit.Assertions.Assert (Expected2 = Actual2,
                               "Expected=" & To_String (Expected2) &
                                 " /= " & To_String (Actual2));
   end Ansi_Get_2_Cards_Put_2_Black_Red;

   procedure Ansi_Get_2_Cards_Put_2_Black_Black (T : in out Test) is
      pragma Unreferenced (T);
      SIF       : Short_Image_FIFO.Short_Image_FIFO_Type
        := Short_Image_FIFO.Construct;

      C1        : constant Card.Card_Type
        := Card.Construct (Definitions.Ace,
                           Definitions.Club);
      SI1       : constant Card.Short_Image_Type := C1.Short_Image;
      Expected1 : constant Unbounded_String := C1.Ansi_Image;
      Actual1   : Unbounded_String;

      C2        : constant Card.Card_Type
        := Card.Construct (Definitions.King,
                           Definitions.Spade);
      SI2       : constant Card.Short_Image_Type := C2.Short_Image;
      Expected2 : constant Unbounded_String := C2.Ansi_Image;
      Actual2   : Unbounded_String;
   begin
      SIF.Put (SI1);
      SIF.Put (SI2);

      Actual1 := SIF.Ansi_Get;
      AUnit.Assertions.Assert (Expected1 = Actual1,
                               "Expected=" & To_String (Expected1) &
                                 " /= " & To_String (Actual1));

      Actual2 := SIF.Ansi_Get;
      AUnit.Assertions.Assert (Expected2 = Actual2,
                               "Expected=" & To_String (Expected2) &
                                 " /= " & To_String (Actual2));
   end Ansi_Get_2_Cards_Put_2_Black_Black;

   --------------------------------------------------------------------
   --  the test suit construction
   package Caller is new AUnit.Test_Caller
     (Short_Image_FIFO.Test.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
      N   : constant String := "Short_Image_FIFO.Test.";
   begin
      --  Construct tests
      Ret.Add_Test (Caller.
                      Create (N & "Construct",
                        Construct'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Construct_Is_Empty",
                        Construct_Is_Empty'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Construct_Size",
                        Construct_Size'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Construct_Check_Get_Fails",
                        Construct_Check_Get_Fails'Access));

      --  Put tests
      Ret.Add_Test (Caller.
                      Create (N & "Put_1_Card_Is_Empty",
                        Put_1_Card_Is_Empty'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Put_1_Card_Size",
                        Put_1_Card_Size'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Put_2_Cards_Is_Empty",
                        Put_2_Cards_Is_Empty'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Put_2_Cards_Size",
                        Put_2_Cards_Size'Access));
      --  Get tests
      Ret.Add_Test (Caller.
                      Create (N & "Get_1_Card_Put_0",
                        Get_1_Card_Put_0'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Get_1_Card_Put_1",
                        Get_1_Card_Put_1'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Get_1_Card_Put_2",
                        Get_1_Card_Put_2'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Get_2_Cards_Put_2",
                        Get_2_Cards_Put_2'Access));

      Ret.Add_Test (Caller.
                      Create (N & "Ansi_Get_1_Card_Put_1_Red",
                        Ansi_Get_1_Card_Put_1_Red'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Ansi_Get_1_Card_Put_1_Black",
                        Ansi_Get_1_Card_Put_1_Black'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Ansi_Get_1_Card_Put_2_Red",
                        Ansi_Get_1_Card_Put_2_Red'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Ansi_Get_1_Card_Put_2_Black",
                        Ansi_Get_1_Card_Put_2_Black'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Ansi_Get_2_Cards_Put_2_Red_Red",
                        Ansi_Get_2_Cards_Put_2_Red_Red'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Ansi_Get_2_Cards_Put_2_Red_Black",
                        Ansi_Get_2_Cards_Put_2_Red_Black'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Ansi_Get_2_Cards_Put_2_Black_Red",
                        Ansi_Get_2_Cards_Put_2_Black_Red'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Ansi_Get_2_Cards_Put_2_Black_Black",
                        Ansi_Get_2_Cards_Put_2_Black_Black'Access));

      return Ret;
   end Suite;

end Short_Image_FIFO.Test;
