with Ada.Characters.Latin_1;

with AUnit.Assertions;
with AUnit.Test_Caller;

with Definitions;

package body Tableau.Tests4_To_String is

   CR_LF : constant String
     := Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.LF;

   procedure To_String_Empty_Tableau (T : in out Test) is
      pragma Unreferenced (T);
      Tab      : constant Tableau.Tableau_Type := Tableau.Construct;
      Expected : constant String
        := String (Tableau.HEADER_LINE) & CR_LF;
      Actual   : String (1 .. Tableau.HEADER_LINE'Length + 2);
   begin
      Actual := Tab.To_String;
      AUnit.Assertions.Assert (Expected'Length = Actual'Length,
                               "Expected'Length= " & Expected'Length'Image &
                                 " /= Actual'Length= " & Actual'Length'Image);
      AUnit.Assertions.Assert (Expected = Actual,
                               "Expected= " & Expected &
                                 " /= Actual= " & Actual);
   end To_String_Empty_Tableau;

   procedure To_String_Pattern_1 (T : in out Test) is
      pragma Unreferenced (T);
      Tab      : constant Tableau.Tableau_Type := Tableau.Construct;
      S        : Tableau_Stack.Stack_Type_Access;
      C        : Card.Card_Type;
      Expected : constant String
        :=
        String (Tableau.HEADER_LINE) & CR_LF &
        "KD    AH JC    TH    " & CR_LF &
        "QS       2C          " & CR_LF &
        "         9D          " & CR_LF;
      Actual   : String
        :=
        String (Tableau.HEADER_LINE) & CR_LF &
        "                     " & CR_LF &
        "                     " & CR_LF &
        "                     " & CR_LF;
   begin
      --  "                     ";
      --  Fill stack one
      S := Tab.Get_Stack (1);
      C := Card.Construct (Definitions.King, Definitions.Diamond);
      S.all.Push_Unchecked (C);
      C := Card.Construct (Definitions.Queen, Definitions.Spade);
      S.all.Push_Unchecked (C);

      --  Fill stack two: EMPTY

      --  Fill stack three
      S := Tab.Get_Stack (3);
      C := Card.Construct (Definitions.Ace, Definitions.Heart);
      S.all.Push_Unchecked (C);

      --  Fill stack four
      S := Tab.Get_Stack (4);
      C := Card.Construct (Definitions.Jack, Definitions.Club);
      S.all.Push_Unchecked (C);
      C := Card.Construct (Definitions.Two, Definitions.Club);
      S.all.Push_Unchecked (C);
      C := Card.Construct (Definitions.Nine, Definitions.Diamond);
      S.all.Push_Unchecked (C);

      --  Fill stack five: EMPTY

      --  Fill stack six
      S := Tab.Get_Stack (6);
      C := Card.Construct (Definitions.Ten, Definitions.Heart);
      S.all.Push_Unchecked (C);

      --  Fill stack seven: EMPTY

      Actual := Tab.To_String;
      AUnit.Assertions.Assert (Expected'Length = Actual'Length,
                               "Expected'Length= " & Expected'Length'Image &
                                 " /= Actual'Length= " & Actual'Length'Image);
      AUnit.Assertions.Assert (Expected = Actual,
                               "Expected= " & Expected &
                                 " /= Actual= " & Actual);
   end To_String_Pattern_1;

   procedure To_String_Pattern_2 (T : in out Test) is
      pragma Unreferenced (T);
      Tab      : constant Tableau.Tableau_Type := Tableau.Construct;
      S        : Tableau_Stack.Stack_Type_Access;
      C        : Card.Card_Type;
      Expected : constant String
        :=
        String (Tableau.HEADER_LINE) & CR_LF &
        "   7C AS    KD    TD " & CR_LF &
        "      KC    QS    9C " & CR_LF &
        "      2H    JH    8H " & CR_LF &
        "      5S             " & CR_LF &
        "      KH             " & CR_LF;
      Actual   : String
        :=
        String (Tableau.HEADER_LINE) & CR_LF &
        "                     " & CR_LF &
        "                     " & CR_LF &
        "                     " & CR_LF &
        "                     " & CR_LF &
        "                     " & CR_LF;
   begin
      --  "                     ";
      --  Fill stack one: EMPTY

      --  Fill stack two
      S := Tab.Get_Stack (2);
      C := Card.Construct (Definitions.Seven, Definitions.Club);
      S.all.Push_Unchecked (C);

      --  Fill stack three
      S := Tab.Get_Stack (3);
      C := Card.Construct (Definitions.Ace, Definitions.Spade);
      S.all.Push_Unchecked (C);
      C := Card.Construct (Definitions.King, Definitions.Club);
      S.all.Push_Unchecked (C);
      C := Card.Construct (Definitions.Two, Definitions.Heart);
      S.all.Push_Unchecked (C);
      C := Card.Construct (Definitions.Five, Definitions.Spade);
      S.all.Push_Unchecked (C);
      C := Card.Construct (Definitions.King, Definitions.Heart);
      S.all.Push_Unchecked (C);

      --  Fill stack four : EMPTY

      --  Fill stack five
      S := Tab.Get_Stack (5);
      C := Card.Construct (Definitions.King, Definitions.Diamond);
      S.all.Push_Unchecked (C);
      C := Card.Construct (Definitions.Queen, Definitions.Spade);
      S.all.Push_Unchecked (C);
      C := Card.Construct (Definitions.Jack, Definitions.Heart);
      S.all.Push_Unchecked (C);

      --  Fill stack six : EMPTY

      --  Fill stack seven
      S := Tab.Get_Stack (7);
      C := Card.Construct (Definitions.Ten, Definitions.Diamond);
      S.all.Push_Unchecked (C);
      C := Card.Construct (Definitions.Nine, Definitions.Club);
      S.all.Push_Unchecked (C);
      C := Card.Construct (Definitions.Eight, Definitions.Heart);
      S.all.Push_Unchecked (C);

      Actual := Tab.To_String;
      AUnit.Assertions.Assert (Expected'Length = Actual'Length,
                               "Expected'Length= " & Expected'Length'Image &
                                 " /= Actual'Length= " & Actual'Length'Image);
      AUnit.Assertions.Assert (Expected = Actual,
                               "Expected= " & Expected &
                                 " /= Actual= " & Actual);
   end To_String_Pattern_2;

   --------------------------------------------------------------------
   --  the test suit construction
   package Caller is new AUnit.Test_Caller
     (Tableau.Tests4_To_String.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
      N   : constant String := "Tableau.Tests4_To_String.";
   begin
      --  One_Liners
      Ret.Add_Test (Caller.
                      Create (N & "To_String_Empty_Tableau",
                        To_String_Empty_Tableau'Access));

      Ret.Add_Test (Caller.
                      Create (N & "To_String_Pattern_1",
                        To_String_Pattern_1'Access));
      Ret.Add_Test (Caller.
                      Create (N & "To_String_Pattern_2",
                        To_String_Pattern_2'Access));
      return Ret;
   end Suite;

end Tableau.Tests4_To_String;
