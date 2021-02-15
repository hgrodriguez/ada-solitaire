with AUnit.Assertions;
with AUnit.Test_Caller;

with Definitions;
with Card;
with Pile_Of_Cards.FIFO;

package body Tableau.Tests4_Remove_Mandatory_Cards is

   Candidate_1 : constant Card.Card_Type
     := Card.Construct (Definitions.Ace,
                        Definitions.Diamond);
   Candidate_2 : constant Card.Card_Type := Card.Construct (Definitions.Seven,
                                                            Definitions.Heart);
   Candidate_3 : constant Card.Card_Type := Card.Construct (Definitions.Ten,
                                                            Definitions.Club);
   Candidate_4 : constant Card.Card_Type := Card.Construct (Definitions.Queen,
                                                            Definitions.Spade);
   Candidates  : constant array (1 .. 4) of Card.Card_Type := (Candidate_1,
                                                               Candidate_2,
                                                               Candidate_3,
                                                               Candidate_4);

   function Construct_Candidates (How_Many : Integer)
                                  return Pile_Of_Cards.FIFO.Pile_Type_FIFO;
   function Construct_Candidates (How_Many : Integer)
                                  return Pile_Of_Cards.FIFO.Pile_Type_FIFO is
      FIFO : Pile_Of_Cards.FIFO.Pile_Type_FIFO := Pile_Of_Cards.FIFO.Construct;
   begin
      for J in Integer range 1 .. How_Many loop
         FIFO.Put (Candidates (J));
      end loop;
      return FIFO;
   end Construct_Candidates;

   function Construct_1_Candidate return Pile_Of_Cards.FIFO.Pile_Type_FIFO;
   function Construct_1_Candidate return Pile_Of_Cards.FIFO.Pile_Type_FIFO is
   begin
      return Construct_Candidates (1);
   end Construct_1_Candidate;

   function Construct_2_Candidates return Pile_Of_Cards.FIFO.Pile_Type_FIFO;
   function Construct_2_Candidates return Pile_Of_Cards.FIFO.Pile_Type_FIFO is
   begin
      return Construct_Candidates (2);
   end Construct_2_Candidates;

   function Construct_3_Candidates return Pile_Of_Cards.FIFO.Pile_Type_FIFO;
   function Construct_3_Candidates return Pile_Of_Cards.FIFO.Pile_Type_FIFO is
   begin
      return Construct_Candidates (3);
   end Construct_3_Candidates;

   function Construct_4_Candidates return Pile_Of_Cards.FIFO.Pile_Type_FIFO;
   function Construct_4_Candidates return Pile_Of_Cards.FIFO.Pile_Type_FIFO is
   begin
      return Construct_Candidates (4);
   end Construct_4_Candidates;

   function Construct_Removals (How_Many : Integer)
                                return Pile_Of_Cards.FIFO.Pile_Type_FIFO;
   function Construct_Removals (How_Many : Integer)
                                return Pile_Of_Cards.FIFO.Pile_Type_FIFO is
      FIFO : Pile_Of_Cards.FIFO.Pile_Type_FIFO := Pile_Of_Cards.FIFO.Construct;
   begin
      for J in Integer range 1 .. 4 loop
         if J < 1 + How_Many then
            FIFO.Put (Candidates (J));
         else
            FIFO.Put (Card.
                        Construct (
                          Definitions.Rank'Succ (Candidates (J).Get_Rank),
                          Candidates (J).Get_Suit));
         end if;
      end loop;
      return FIFO;
   end Construct_Removals;

   function Construct_0_Removals return Pile_Of_Cards.FIFO.Pile_Type_FIFO;
   function Construct_0_Removals return Pile_Of_Cards.FIFO.Pile_Type_FIFO is
   begin
      return Construct_Removals (0);
   end Construct_0_Removals;

   function Construct_1_Removal return Pile_Of_Cards.FIFO.Pile_Type_FIFO;
   function Construct_1_Removal return Pile_Of_Cards.FIFO.Pile_Type_FIFO is
   begin
      return Construct_Removals (1);
   end Construct_1_Removal;

   function Construct_2_Removals return Pile_Of_Cards.FIFO.Pile_Type_FIFO;
   function Construct_2_Removals return Pile_Of_Cards.FIFO.Pile_Type_FIFO is
   begin
      return Construct_Removals (2);
   end Construct_2_Removals;

   function Construct_3_Removals return Pile_Of_Cards.FIFO.Pile_Type_FIFO;
   function Construct_3_Removals return Pile_Of_Cards.FIFO.Pile_Type_FIFO is
   begin
      return Construct_Removals (3);
   end Construct_3_Removals;

   function Construct_4_Removals return Pile_Of_Cards.FIFO.Pile_Type_FIFO;
   function Construct_4_Removals return Pile_Of_Cards.FIFO.Pile_Type_FIFO is
   begin
      return Construct_Removals (4);
   end Construct_4_Removals;

   procedure RMC_0_C_Tab_Empty (T : in out Test) is
      pragma Unreferenced (T);
      Tab             : constant Tableau.Tableau_Type := Tableau.Construct;
      Candidates      : constant Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Pile_Of_Cards.FIFO.Construct;
      Cards_To_Remove : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
   begin
      Cards_To_Remove := Tab.Remove_Mandatory_Cards (Candidates);
      AUnit.Assertions.Assert (Cards_To_Remove.Is_Empty,
                               "not FIFO.Is_Empty");
   end RMC_0_C_Tab_Empty;

   procedure RMC_0_C_Tab_Not_Empty (T : in out Test) is
      pragma Unreferenced (T);
      Tab             : constant Tableau.Tableau_Type := Tableau.Construct;
      Candidates      : constant Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Pile_Of_Cards.FIFO.Construct;
      Cards_To_Remove : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
      Tab_Filler      : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_0_Removals;
   begin
      Tab.Push (Tab_Filler);
      Cards_To_Remove := Tab.Remove_Mandatory_Cards (Candidates);
      AUnit.Assertions.Assert (Cards_To_Remove.Is_Empty,
                               "not FIFO.Is_Empty");
   end RMC_0_C_Tab_Not_Empty;

   procedure RMC_1_C_0_R_Tab_Empty (T : in out Test) is
      pragma Unreferenced (T);
      Tab             : constant Tableau.Tableau_Type := Tableau.Construct;
      Candidates      : constant Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_1_Candidate;
      Cards_To_Remove : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
   begin
      Cards_To_Remove := Tab.Remove_Mandatory_Cards (Candidates);
      AUnit.Assertions.Assert (Cards_To_Remove.Is_Empty,
                               "not FIFO.Is_Empty");
   end RMC_1_C_0_R_Tab_Empty;

   procedure RMC_1_C_0_R_Tab_Not_Empty (T : in out Test) is
      pragma Unreferenced (T);
      Tab             : constant Tableau.Tableau_Type := Tableau.Construct;
      Candidates      : constant Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_1_Candidate;
      Cards_To_Remove : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
      Tab_Filler      : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_0_Removals;
   begin
      Tab.Push (Tab_Filler);
      Cards_To_Remove := Tab.Remove_Mandatory_Cards (Candidates);
      AUnit.Assertions.Assert (Cards_To_Remove.Is_Empty,
                               "not FIFO.Is_Empty");
   end RMC_1_C_0_R_Tab_Not_Empty;

   procedure RMC_1_C_1_R_Tab_Not_Empty (T : in out Test) is
      pragma Unreferenced (T);
      Tab             : constant Tableau.Tableau_Type := Tableau.Construct;
      Candidates      : constant Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_1_Candidate;
      Cards_To_Remove : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
      Tab_Filler      : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_1_Removal;
      Size_Expected   : constant Natural := 1;
      Size_Actual     : Natural;
   begin
      Tab.Push (Tab_Filler);
      Cards_To_Remove := Tab.Remove_Mandatory_Cards (Candidates);
      Size_Actual := Cards_To_Remove.Size;
      AUnit.Assertions.Assert (Size_Actual = Size_Expected,
                               "exp=" & Size_Expected'Image &
                                 " /= act=" & Size_Actual'Image);
   end RMC_1_C_1_R_Tab_Not_Empty;

   procedure RMC_2_C_0_R_Tab_Empty (T : in out Test) is
      pragma Unreferenced (T);
      Tab             : constant Tableau.Tableau_Type := Tableau.Construct;
      Candidates      : constant Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_2_Candidates;
      Cards_To_Remove : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
   begin
      Cards_To_Remove := Tab.Remove_Mandatory_Cards (Candidates);
      AUnit.Assertions.Assert (Cards_To_Remove.Is_Empty,
                               "not FIFO.Is_Empty");
   end RMC_2_C_0_R_Tab_Empty;

   procedure RMC_2_C_0_R_Tab_Not_Empty (T : in out Test) is
      pragma Unreferenced (T);
      Tab             : constant Tableau.Tableau_Type := Tableau.Construct;
      Candidates      : constant Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_2_Candidates;
      Cards_To_Remove : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
      Tab_Filler      : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_0_Removals;
   begin
      Tab.Push (Tab_Filler);
      Cards_To_Remove := Tab.Remove_Mandatory_Cards (Candidates);
      AUnit.Assertions.Assert (Cards_To_Remove.Is_Empty,
                               "not FIFO.Is_Empty");
   end RMC_2_C_0_R_Tab_Not_Empty;

   procedure RMC_2_C_1_R_Tab_Not_Empty (T : in out Test) is
      pragma Unreferenced (T);
      Tab             : constant Tableau.Tableau_Type := Tableau.Construct;
      Candidates      : constant Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_2_Candidates;
      Cards_To_Remove : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
      Tab_Filler      : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_1_Removal;
      Size_Expected   : constant Natural := 1;
      Size_Actual     : Natural;
   begin
      Tab.Push (Tab_Filler);
      Cards_To_Remove := Tab.Remove_Mandatory_Cards (Candidates);
      Size_Actual := Cards_To_Remove.Size;
      AUnit.Assertions.Assert (Size_Actual = Size_Expected,
                               "exp=" & Size_Expected'Image &
                                 " /= act=" & Size_Actual'Image);
   end RMC_2_C_1_R_Tab_Not_Empty;

   procedure RMC_2_C_2_R_Tab_Not_Empty (T : in out Test) is
      pragma Unreferenced (T);
      Tab             : constant Tableau.Tableau_Type := Tableau.Construct;
      Candidates      : constant Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_2_Candidates;
      Cards_To_Remove : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
      Tab_Filler      : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_2_Removals;
      Size_Expected   : constant Natural := 2;
      Size_Actual     : Natural;
   begin
      Tab.Push (Tab_Filler);
      Cards_To_Remove := Tab.Remove_Mandatory_Cards (Candidates);
      Size_Actual := Cards_To_Remove.Size;
      AUnit.Assertions.Assert (Size_Actual = Size_Expected,
                               "exp=" & Size_Expected'Image &
                                 " /= act=" & Size_Actual'Image);
   end RMC_2_C_2_R_Tab_Not_Empty;

   procedure RMC_3_C_0_R_Tab_Empty (T : in out Test) is
      pragma Unreferenced (T);
      Tab             : constant Tableau.Tableau_Type := Tableau.Construct;
      Candidates      : constant Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_3_Candidates;
      Cards_To_Remove : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
   begin
      Cards_To_Remove := Tab.Remove_Mandatory_Cards (Candidates);
      AUnit.Assertions.Assert (Cards_To_Remove.Is_Empty,
                               "not FIFO.Is_Empty");
   end RMC_3_C_0_R_Tab_Empty;

   procedure RMC_3_C_0_R_Tab_Not_Empty (T : in out Test) is
      pragma Unreferenced (T);
      Tab             : constant Tableau.Tableau_Type := Tableau.Construct;
      Candidates      : constant Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_3_Candidates;
      Cards_To_Remove : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
      Tab_Filler      : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_0_Removals;
   begin
      Tab.Push (Tab_Filler);
      Cards_To_Remove := Tab.Remove_Mandatory_Cards (Candidates);
      AUnit.Assertions.Assert (Cards_To_Remove.Is_Empty,
                               "not FIFO.Is_Empty");
   end RMC_3_C_0_R_Tab_Not_Empty;

   procedure RMC_3_C_1_R_Tab_Not_Empty (T : in out Test) is
      pragma Unreferenced (T);
      Tab             : constant Tableau.Tableau_Type := Tableau.Construct;
      Candidates      : constant Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_3_Candidates;
      Cards_To_Remove : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
      Tab_Filler      : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_1_Removal;
      Size_Expected   : constant Natural := 1;
      Size_Actual     : Natural;
   begin
      Tab.Push (Tab_Filler);
      Cards_To_Remove := Tab.Remove_Mandatory_Cards (Candidates);
      Size_Actual := Cards_To_Remove.Size;
      AUnit.Assertions.Assert (Size_Actual = Size_Expected,
                               "exp=" & Size_Expected'Image &
                                 " /= act=" & Size_Actual'Image);
   end RMC_3_C_1_R_Tab_Not_Empty;

   procedure RMC_3_C_2_R_Tab_Not_Empty (T : in out Test) is
      pragma Unreferenced (T);
      Tab             : constant Tableau.Tableau_Type := Tableau.Construct;
      Candidates      : constant Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_3_Candidates;
      Cards_To_Remove : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
      Tab_Filler      : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_2_Removals;
      Size_Expected   : constant Natural := 2;
      Size_Actual     : Natural;
   begin
      Tab.Push (Tab_Filler);
      Cards_To_Remove := Tab.Remove_Mandatory_Cards (Candidates);
      Size_Actual := Cards_To_Remove.Size;
      AUnit.Assertions.Assert (Size_Actual = Size_Expected,
                               "exp=" & Size_Expected'Image &
                                 " /= act=" & Size_Actual'Image);
   end RMC_3_C_2_R_Tab_Not_Empty;

   procedure RMC_3_C_3_R_Tab_Not_Empty (T : in out Test) is
      pragma Unreferenced (T);
      Tab             : constant Tableau.Tableau_Type := Tableau.Construct;
      Candidates      : constant Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_3_Candidates;
      Cards_To_Remove : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
      Tab_Filler      : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_3_Removals;
      Size_Expected   : constant Natural := 3;
      Size_Actual     : Natural;
   begin
      Tab.Push (Tab_Filler);
      Cards_To_Remove := Tab.Remove_Mandatory_Cards (Candidates);
      Size_Actual := Cards_To_Remove.Size;
      AUnit.Assertions.Assert (Size_Actual = Size_Expected,
                               "exp=" & Size_Expected'Image &
                                 " /= act=" & Size_Actual'Image);
   end RMC_3_C_3_R_Tab_Not_Empty;

   procedure RMC_4_C_0_R_Tab_Empty (T : in out Test) is
      pragma Unreferenced (T);
      Tab             : constant Tableau.Tableau_Type := Tableau.Construct;
      Candidates      : constant Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_4_Candidates;
      Cards_To_Remove : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
   begin
      Cards_To_Remove := Tab.Remove_Mandatory_Cards (Candidates);
      AUnit.Assertions.Assert (Cards_To_Remove.Is_Empty,
                               "not FIFO.Is_Empty");
   end RMC_4_C_0_R_Tab_Empty;

   procedure RMC_4_C_0_R_Tab_Not_Empty (T : in out Test) is
      pragma Unreferenced (T);
      Tab             : constant Tableau.Tableau_Type := Tableau.Construct;
      Candidates      : constant Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_4_Candidates;
      Cards_To_Remove : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
      Tab_Filler      : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_0_Removals;
   begin
      Tab.Push (Tab_Filler);
      Cards_To_Remove := Tab.Remove_Mandatory_Cards (Candidates);
      AUnit.Assertions.Assert (Cards_To_Remove.Is_Empty,
                               "not FIFO.Is_Empty");
   end RMC_4_C_0_R_Tab_Not_Empty;

   procedure RMC_4_C_1_R_Tab_Not_Empty (T : in out Test) is
      pragma Unreferenced (T);
      Tab             : constant Tableau.Tableau_Type := Tableau.Construct;
      Candidates      : constant Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_4_Candidates;
      Cards_To_Remove : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
      Tab_Filler      : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_1_Removal;
      Size_Expected   : constant Natural := 1;
      Size_Actual     : Natural;
   begin
      Tab.Push (Tab_Filler);
      Cards_To_Remove := Tab.Remove_Mandatory_Cards (Candidates);
      Size_Actual := Cards_To_Remove.Size;
      AUnit.Assertions.Assert (Size_Actual = Size_Expected,
                               "exp=" & Size_Expected'Image &
                                 " /= act=" & Size_Actual'Image);
   end RMC_4_C_1_R_Tab_Not_Empty;

   procedure RMC_4_C_2_R_Tab_Not_Empty (T : in out Test) is
      pragma Unreferenced (T);
      Tab             : constant Tableau.Tableau_Type := Tableau.Construct;
      Candidates      : constant Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_4_Candidates;
      Cards_To_Remove : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
      Tab_Filler      : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_2_Removals;
      Size_Expected   : constant Natural := 2;
      Size_Actual     : Natural;
   begin
      Tab.Push (Tab_Filler);
      Cards_To_Remove := Tab.Remove_Mandatory_Cards (Candidates);
      Size_Actual := Cards_To_Remove.Size;
      AUnit.Assertions.Assert (Size_Actual = Size_Expected,
                               "exp=" & Size_Expected'Image &
                                 " /= act=" & Size_Actual'Image);
   end RMC_4_C_2_R_Tab_Not_Empty;

   procedure RMC_4_C_3_R_Tab_Not_Empty (T : in out Test) is
      pragma Unreferenced (T);
      Tab             : constant Tableau.Tableau_Type := Tableau.Construct;
      Candidates      : constant Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_4_Candidates;
      Cards_To_Remove : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
      Tab_Filler      : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_3_Removals;
      Size_Expected   : constant Natural := 3;
      Size_Actual     : Natural;
   begin
      Tab.Push (Tab_Filler);
      Cards_To_Remove := Tab.Remove_Mandatory_Cards (Candidates);
      Size_Actual := Cards_To_Remove.Size;
      AUnit.Assertions.Assert (Size_Actual = Size_Expected,
                               "exp=" & Size_Expected'Image &
                                 " /= act=" & Size_Actual'Image);
   end RMC_4_C_3_R_Tab_Not_Empty;

   procedure RMC_4_C_4_R_Tab_Not_Empty (T : in out Test) is
      pragma Unreferenced (T);
      Tab             : constant Tableau.Tableau_Type := Tableau.Construct;
      Candidates      : constant Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_4_Candidates;
      Cards_To_Remove : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
      Tab_Filler      : Pile_Of_Cards.FIFO.Pile_Type_FIFO
        := Construct_4_Removals;
      Size_Expected   : constant Natural := 4;
      Size_Actual     : Natural;
   begin
      Tab.Push (Tab_Filler);
      Cards_To_Remove := Tab.Remove_Mandatory_Cards (Candidates);
      Size_Actual := Cards_To_Remove.Size;
      AUnit.Assertions.Assert (Size_Actual = Size_Expected,
                               "exp=" & Size_Expected'Image &
                                 " /= act=" & Size_Actual'Image);
   end RMC_4_C_4_R_Tab_Not_Empty;

   --------------------------------------------------------------------
   --  the test suit construction
   package Caller is new AUnit.Test_Caller
     (Tableau.Tests4_Remove_Mandatory_Cards.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
      N   : constant String := "Tableau.Tests4_Remove_Mandatory_Cards.";
   begin
      --  0 Candidates
      Ret.Add_Test (Caller.
                      Create (N & "RMC_0_C_Tab_Empty",
                        RMC_0_C_Tab_Empty'Access));
      Ret.Add_Test (Caller.
                      Create (N & "RMC_0_C_Tab_Not_Empty",
                        RMC_0_C_Tab_Not_Empty'Access));

      --  1 Candidate
      Ret.Add_Test (Caller.
                      Create (N & "RMC_1_C_0_R_Tab_Empty",
                        RMC_1_C_0_R_Tab_Empty'Access));
      Ret.Add_Test (Caller.
                      Create (N & "RMC_1_C_0_R_Tab_Not_Empty",
                        RMC_1_C_0_R_Tab_Not_Empty'Access));
      Ret.Add_Test (Caller.
                      Create (N & "RMC_1_C_1_R_Tab_Not_Empty",
                        RMC_1_C_1_R_Tab_Not_Empty'Access));

      --  2 Candidates
      Ret.Add_Test (Caller.
                      Create (N & "RMC_2_C_0_R_Tab_Empty",
                        RMC_2_C_0_R_Tab_Empty'Access));
      Ret.Add_Test (Caller.
                      Create (N & "RMC_2_C_0_R_Tab_Not_Empty",
                        RMC_2_C_0_R_Tab_Not_Empty'Access));
      Ret.Add_Test (Caller.
                      Create (N & "RMC_2_C_1_R_Tab_Not_Empty",
                        RMC_2_C_1_R_Tab_Not_Empty'Access));
      Ret.Add_Test (Caller.
                      Create (N & "RMC_2_C_2_R_Tab_Not_Empty",
                        RMC_2_C_2_R_Tab_Not_Empty'Access));

      --  3 Candidates
      Ret.Add_Test (Caller.
                      Create (N & "RMC_3_C_0_R_Tab_Empty",
                        RMC_3_C_0_R_Tab_Empty'Access));
      Ret.Add_Test (Caller.
                      Create (N & "RMC_3_C_0_R_Tab_Not_Empty",
                        RMC_3_C_0_R_Tab_Not_Empty'Access));
      Ret.Add_Test (Caller.
                      Create (N & "RMC_3_C_1_R_Tab_Not_Empty",
                        RMC_3_C_1_R_Tab_Not_Empty'Access));
      Ret.Add_Test (Caller.
                      Create (N & "RMC_3_C_2_R_Tab_Not_Empty",
                        RMC_3_C_2_R_Tab_Not_Empty'Access));
      Ret.Add_Test (Caller.
                      Create (N & "RMC_3_C_3_R_Tab_Not_Empty",
                        RMC_3_C_3_R_Tab_Not_Empty'Access));

      --  4 Candidates
      Ret.Add_Test (Caller.
                      Create (N & "RMC_4_C_0_R_Tab_Empty",
                        RMC_4_C_0_R_Tab_Empty'Access));
      Ret.Add_Test (Caller.
                      Create (N & "RMC_4_C_0_R_Tab_Not_Empty",
                        RMC_4_C_0_R_Tab_Not_Empty'Access));
      Ret.Add_Test (Caller.
                      Create (N & "RMC_4_C_1_R_Tab_Not_Empty",
                        RMC_4_C_1_R_Tab_Not_Empty'Access));
      Ret.Add_Test (Caller.
                      Create (N & "RMC_4_C_2_R_Tab_Not_Empty",
                        RMC_4_C_2_R_Tab_Not_Empty'Access));
      Ret.Add_Test (Caller.
                      Create (N & "RMC_4_C_3_R_Tab_Not_Empty",
                        RMC_4_C_3_R_Tab_Not_Empty'Access));
      Ret.Add_Test (Caller.
                      Create (N & "RMC_4_C_4_R_Tab_Not_Empty",
                        RMC_4_C_4_R_Tab_Not_Empty'Access));

      return Ret;
   end Suite;

end Tableau.Tests4_Remove_Mandatory_Cards;
