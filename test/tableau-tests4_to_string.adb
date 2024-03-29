with Ada.Characters.Latin_1;

with AUnit.Assertions;
with AUnit.Test_Caller;

with Definitions;

package body Tableau.Tests4_To_String is

   CR_LF : constant String
     := Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.LF;

   Tab : Tableau.Tableau_Type;

   overriding
   procedure Set_Up (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Tab  := Tableau.Construct;
   end Set_Up;

   procedure To_String_Empty_Tableau (T : in out Test) is
      pragma Unreferenced (T);
--      Tab      : constant Tableau.Tableau_Type := Tableau.Construct;
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

   procedure Ansi_To_String_Empty_Tableau (T : in out Test) is
      pragma Unreferenced (T);
      Expected : constant Unbounded_String
        := To_Unbounded_String (String (Tableau.HEADER_LINE)) & CR_LF;
      Actual   : Unbounded_String;
   begin
      Actual := Tab.Ansi_To_String;
      AUnit.
        Assertions.
          Assert (Length (Expected) = Length (Actual),
                  "Expected'Length= " & Length (Expected)'Image &
                      " /= Actual'Length= " & Length (Actual)'Image);
      AUnit.
        Assertions.
          Assert (Expected = Actual,
                  "Expected= " & To_String (Expected) &
                    " /= Actual= " & To_String (Actual));
   end Ansi_To_String_Empty_Tableau;

   procedure Ansi_To_String_Pattern_1 (T : in out Test) is
      pragma Unreferenced (T);
      S        : Tableau_Stack.Stack_Type_Access;
      C        : Card.Card_Type;
      KD       : constant Card.Card_Type
        := Card.Construct (Definitions.King, Definitions.Diamond);
      KD_Ansi  : constant Unbounded_String := KD.Ansi_Image;
      AH       : constant Card.Card_Type
        := Card.Construct (Definitions.Ace, Definitions.Heart);
      AH_Ansi  : constant Unbounded_String := AH.Ansi_Image;
      TH       : constant Card.Card_Type
        := Card.Construct (Definitions.Ten, Definitions.Heart);
      TH_Ansi  : constant Unbounded_String := TH.Ansi_Image;
      ND       : constant Card.Card_Type
        := Card.Construct (Definitions.Nine, Definitions.Diamond);
      ND_Ansi  : constant Unbounded_String := ND.Ansi_Image;
      Expected : constant Unbounded_String
        := To_Unbounded_String (
          String (Tableau.HEADER_LINE) & CR_LF) &
        KD_Ansi & To_Unbounded_String ("    ") &
        AH_Ansi & To_Unbounded_String (" JC    ") &
        TH_Ansi & To_Unbounded_String ("    ") & CR_LF &
        To_Unbounded_String ("QS       2C          ") & CR_LF &
        To_Unbounded_String ("         ") & ND_Ansi &
        To_Unbounded_String ("          ") & CR_LF;
      Actual   : Unbounded_String;
   begin
      --  "                     ";
      --  Fill stack one
      S := Tab.Get_Stack (1);
      S.all.Push_Unchecked (KD);
      C := Card.Construct (Definitions.Queen, Definitions.Spade);
      S.all.Push_Unchecked (C);

      --  Fill stack two: EMPTY

      --  Fill stack three
      S := Tab.Get_Stack (3);
      S.all.Push_Unchecked (AH);

      --  Fill stack four
      S := Tab.Get_Stack (4);
      C := Card.Construct (Definitions.Jack, Definitions.Club);
      S.all.Push_Unchecked (C);
      C := Card.Construct (Definitions.Two, Definitions.Club);
      S.all.Push_Unchecked (C);
      S.all.Push_Unchecked (ND);

      --  Fill stack five: EMPTY

      --  Fill stack six
      S := Tab.Get_Stack (6);
      S.all.Push_Unchecked (TH);

      --  Fill stack seven: EMPTY

      Actual := Tab.Ansi_To_String;
      AUnit.
        Assertions.
          Assert (Length (Expected) = Length (Actual),
                  "Expected'Length= " & Length (Expected)'Image &
                      " /= Actual'Length= " & Length (Actual)'Image);
      AUnit.
        Assertions.
          Assert (Expected = Actual,
                  "Expected= " & To_String (Expected) &
                      " /= Actual= " & To_String (Actual));
   end Ansi_To_String_Pattern_1;

   procedure Ansi_To_String_Pattern_2 (T : in out Test) is
      pragma Unreferenced (T);
      S        : Tableau_Stack.Stack_Type_Access;
      C        : Card.Card_Type;
      KD       : constant Card.Card_Type
        := Card.Construct (Definitions.King, Definitions.Diamond);
      KD_Ansi  : constant Unbounded_String := KD.Ansi_Image;
      KH       : constant Card.Card_Type
        := Card.Construct (Definitions.King, Definitions.Heart);
      KH_Ansi  : constant Unbounded_String := KH.Ansi_Image;
      TD       : constant Card.Card_Type
        := Card.Construct (Definitions.Ten, Definitions.Diamond);
      TD_Ansi  : constant Unbounded_String := TD.Ansi_Image;
      TH       : constant Card.Card_Type
        := Card.Construct (Definitions.Two, Definitions.Heart);
      TH_Ansi  : constant Unbounded_String := TH.Ansi_Image;
      JH       : constant Card.Card_Type
        := Card.Construct (Definitions.Jack, Definitions.Heart);
      JH_Ansi  : constant Unbounded_String := JH.Ansi_Image;
      EH       : constant Card.Card_Type
        := Card.Construct (Definitions.Eight, Definitions.Heart);
      EH_Ansi  : constant Unbounded_String := EH.Ansi_Image;
      Expected : constant Unbounded_String
        := To_Unbounded_String (
          String (Tableau.HEADER_LINE)) & CR_LF &
        To_Unbounded_String ("   7C AS    ") & KD_Ansi &
        To_Unbounded_String ("    ") & TD_Ansi &
        To_Unbounded_String (" ") & CR_LF &
        To_Unbounded_String ("      KC    QS    9C ") & CR_LF &
        To_Unbounded_String ("      ") & TH_Ansi &
        To_Unbounded_String ("    ") & JH_Ansi &
        To_Unbounded_String ("    ") & EH_Ansi &
        To_Unbounded_String (" ") & CR_LF &
        To_Unbounded_String ("      5S             ") & CR_LF &
        To_Unbounded_String ("      ") & KH_Ansi &
        To_Unbounded_String ("             ") & CR_LF;
      Actual   : Unbounded_String;
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
      S.all.Push_Unchecked (TH);
      C := Card.Construct (Definitions.Five, Definitions.Spade);
      S.all.Push_Unchecked (C);
      S.all.Push_Unchecked (KH);

      --  Fill stack four : EMPTY

      --  Fill stack five
      S := Tab.Get_Stack (5);
      S.all.Push_Unchecked (KD);
      C := Card.Construct (Definitions.Queen, Definitions.Spade);
      S.all.Push_Unchecked (C);
      S.all.Push_Unchecked (JH);

      --  Fill stack six : EMPTY

      --  Fill stack seven
      S := Tab.Get_Stack (7);
      S.all.Push_Unchecked (TD);
      C := Card.Construct (Definitions.Nine, Definitions.Club);
      S.all.Push_Unchecked (C);
      S.all.Push_Unchecked (EH);

      Actual := Tab.Ansi_To_String;
      AUnit.
        Assertions.
          Assert (Length (Expected) = Length (Actual),
                  "Expected'Length= " & Length (Expected)'Image &
                      " /= Actual'Length= " & Length (Actual)'Image);
      AUnit.
        Assertions.
          Assert (Expected = Actual,
                  "Expected= " & To_String (Expected) &
                      " /= Actual= " & To_String (Actual));
   end Ansi_To_String_Pattern_2;

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

      Ret.Add_Test (Caller.
                      Create (N & "Ansi_To_String_Empty_Tableau",
                        Ansi_To_String_Empty_Tableau'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Ansi_To_String_Pattern_1",
                        Ansi_To_String_Pattern_1'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Ansi_To_String_Pattern_2",
                        Ansi_To_String_Pattern_2'Access));

      return Ret;
   end Suite;

end Tableau.Tests4_To_String;
