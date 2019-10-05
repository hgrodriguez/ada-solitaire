with Ada.Characters.Latin_1;

with AUnit.Assertions;
with AUnit.Test_Caller;

with Deck;

package body Tableau.Tests4_To_String is

   procedure To_String_Empty_Tableau (T : in out Test) is
      pragma Unreferenced (T);
      Tab      : constant Tableau.Tableau_Type := Tableau.Construct;
      Expected : constant String
        := String (Tableau.HEADER_LINE) & Ada.Characters.Latin_1.CR;
      Actual   : String (1 .. Tableau.HEADER_LINE'Length + 1);
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
        String (Tableau.HEADER_LINE) & Ada.Characters.Latin_1.CR &
        "KD    AH JC    TH    " & Ada.Characters.Latin_1.CR &
        "QS       2C          " & Ada.Characters.Latin_1.CR &
        "         9D          " & Ada.Characters.Latin_1.CR;
      Actual   : String
        :=
        String (Tableau.HEADER_LINE) & Ada.Characters.Latin_1.CR &
        "                     " & Ada.Characters.Latin_1.CR &
        "                     " & Ada.Characters.Latin_1.CR &
        "                     " & Ada.Characters.Latin_1.CR;
   begin
      --  "                     ";
      --  Fill stack one
      S := Tab.Get_Stack (1);
      C := Card.Construct (Deck.King, Deck.Diamond);
      S.all.Push_Unchecked (C);
      C := Card.Construct (Deck.Queen, Deck.Spade);
      S.all.Push_Unchecked (C);

      --  Fill stack two: EMPTY

      --  Fill stack three
      S := Tab.Get_Stack (3);
      C := Card.Construct (Deck.Ace, Deck.Heart);
      S.all.Push_Unchecked (C);

      --  Fill stack four
      S := Tab.Get_Stack (4);
      C := Card.Construct (Deck.Jack, Deck.Club);
      S.all.Push_Unchecked (C);
      C := Card.Construct (Deck.Two, Deck.Club);
      S.all.Push_Unchecked (C);
      C := Card.Construct (Deck.Nine, Deck.Diamond);
      S.all.Push_Unchecked (C);

      --  Fill stack five: EMPTY

      --  Fill stack six
      S := Tab.Get_Stack (6);
      C := Card.Construct (Deck.Ten, Deck.Heart);
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
        String (Tableau.HEADER_LINE) & Ada.Characters.Latin_1.CR &
        "   7C AS    KD    TD " & Ada.Characters.Latin_1.CR &
        "      KC    QS    9C " & Ada.Characters.Latin_1.CR &
        "      2H    JH    8H " & Ada.Characters.Latin_1.CR &
        "      5S             " & Ada.Characters.Latin_1.CR &
        "      KH             " & Ada.Characters.Latin_1.CR;
      Actual   : String
        :=
        String (Tableau.HEADER_LINE) & Ada.Characters.Latin_1.CR &
        "                     " & Ada.Characters.Latin_1.CR &
        "                     " & Ada.Characters.Latin_1.CR &
        "                     " & Ada.Characters.Latin_1.CR &
        "                     " & Ada.Characters.Latin_1.CR &
        "                     " & Ada.Characters.Latin_1.CR;
   begin
      --  "                     ";
      --  Fill stack one: EMPTY

      --  Fill stack two
      S := Tab.Get_Stack (2);
      C := Card.Construct (Deck.Seven, Deck.Club);
      S.all.Push_Unchecked (C);

      --  Fill stack three
      S := Tab.Get_Stack (3);
      C := Card.Construct (Deck.Ace, Deck.Spade);
      S.all.Push_Unchecked (C);
      C := Card.Construct (Deck.King, Deck.Club);
      S.all.Push_Unchecked (C);
      C := Card.Construct (Deck.Two, Deck.Heart);
      S.all.Push_Unchecked (C);
      C := Card.Construct (Deck.Five, Deck.Spade);
      S.all.Push_Unchecked (C);
      C := Card.Construct (Deck.King, Deck.Heart);
      S.all.Push_Unchecked (C);

      --  Fill stack four : EMPTY

      --  Fill stack five
      S := Tab.Get_Stack (5);
      C := Card.Construct (Deck.King, Deck.Diamond);
      S.all.Push_Unchecked (C);
      C := Card.Construct (Deck.Queen, Deck.Spade);
      S.all.Push_Unchecked (C);
      C := Card.Construct (Deck.Jack, Deck.Heart);
      S.all.Push_Unchecked (C);

      --  Fill stack six : EMPTY

      --  Fill stack seven
      S := Tab.Get_Stack (7);
      C := Card.Construct (Deck.Ten, Deck.Diamond);
      S.all.Push_Unchecked (C);
      C := Card.Construct (Deck.Nine, Deck.Club);
      S.all.Push_Unchecked (C);
      C := Card.Construct (Deck.Eight, Deck.Heart);
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
