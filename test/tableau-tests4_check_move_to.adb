with AUnit.Assertions;
with AUnit.Test_Caller;

with Definitions;

package body Tableau.Tests4_Check_Move_To is

   procedure Check_Move_To_Src_Stack_Equals_Trgt_Stack (T : in out Test) is
      pragma Unreferenced (T);
      Tab       : constant Tableau.Tableau_Type := Tableau.Construct;
      Src_Stack : constant Tableau_Stack.Stack_Type_Access
        := Tab.Get_Stack (1);
      Cards     : constant Some_Cards
        := (Card.Construct (Definitions.Three, Definitions.Diamond),
            Card.Construct (Definitions.Seven, Definitions.Heart),
            Card.Construct (Definitions.Ace,   Definitions.Spade),
            Card.Construct (Definitions.King,  Definitions.Club),
            Card.Construct (Definitions.Eight, Definitions.Diamond),
            Card.Construct (Definitions.Three, Definitions.Spade),
            Card.Construct (Definitions.Jack,  Definitions.Heart));
      Exists    : constant Card.Card_Type := Cards (1);
      Expected  : constant Tableau.Check_Move_To_Result
        := Tableau.Destination_Stack_Equals_Source_Stack;
      Actual    : Tableau.Check_Move_To_Result;
   begin
      for J in Natural range Cards'First .. Cards'Last loop
         Src_Stack.all.Push_Unchecked (Cards (J));
      end loop;
      Actual := Tab.Check_Move_To (1, 1, Exists);
      AUnit.
        Assertions.
          Assert (Expected = Actual,
                  "Expected = " & Expected'Image &
                    " /= Actual=" & Actual'Image);
   end Check_Move_To_Src_Stack_Equals_Trgt_Stack;

   procedure Check_Move_To_Source_Selection_Does_Not_Exist (T : in out Test) is
      pragma Unreferenced (T);
      Tab       : constant Tableau.Tableau_Type := Tableau.Construct;
      Src_Stack : constant Tableau_Stack.Stack_Type_Access
        := Tab.Get_Stack (1);
      Cards     : constant Some_Cards
        := (Card.Construct (Definitions.Three, Definitions.Diamond),
            Card.Construct (Definitions.Seven, Definitions.Heart),
            Card.Construct (Definitions.Ace,   Definitions.Spade),
            Card.Construct (Definitions.King,  Definitions.Club),
            Card.Construct (Definitions.Eight, Definitions.Diamond),
            Card.Construct (Definitions.Three, Definitions.Spade),
            Card.Construct (Definitions.Jack,  Definitions.Heart));
      Not_Exist : constant Card.Card_Type
        := Card.Construct (Definitions.Queen, Definitions.Club);
      Expected  : constant Tableau.Check_Move_To_Result
        := Tableau.Source_Card_Does_Not_Exist;
      Actual    : Tableau.Check_Move_To_Result;
   begin
      for J in Natural range Cards'First .. Cards'Last loop
         Src_Stack.all.Push_Unchecked (Cards (J));
      end loop;
      Actual := Tab.Check_Move_To (1, 2, Not_Exist);
      AUnit.
        Assertions.
          Assert (Expected = Actual,
                  "Expected = " & Expected'Image &
                    " /= Actual=" & Actual'Image);
   end Check_Move_To_Source_Selection_Does_Not_Exist;

   procedure Check_Move_To_1_Trgt_Does_Not_Accept (T : in out Test) is
      pragma Unreferenced (T);
      Tab       : constant Tableau.Tableau_Type := Tableau.Construct;
      --
      Src_Stack : constant Tableau_Stack.Stack_Type_Access
        := Tab.Get_Stack (1);
      Src_Cards : constant Some_Cards
        := (1 => Card.Construct (Definitions.Three, Definitions.Diamond));
      Src_Exist : constant Card.Card_Type := Src_Cards (1);
      --
      Dst_Stack : constant Tableau_Stack.Stack_Type_Access
        := Tab.Get_Stack (2);
      Dst_Cards : constant Some_Cards
        := (1 => Card.Construct (Definitions.Ten, Definitions.Diamond));
      Expected  : constant Tableau.Check_Move_To_Result
        := Tableau.Destination_Stack_Does_Not_Accept;
      Actual    : Tableau.Check_Move_To_Result;
   begin
      for J in Natural range Src_Cards'First .. Src_Cards'Last loop
         Src_Stack.all.Push_Unchecked (Src_Cards (J));
      end loop;
      for J in Natural range Dst_Cards'First .. Dst_Cards'Last loop
         Dst_Stack.all.Push_Unchecked (Dst_Cards (J));
      end loop;
      Actual := Tab.Check_Move_To (1, 2, Src_Exist);
      AUnit.
        Assertions.
          Assert (Expected = Actual,
                  "Expected = " & Expected'Image &
                    " /= Actual=" & Actual'Image);
   end Check_Move_To_1_Trgt_Does_Not_Accept;

   procedure Check_Move_To_1_Src_Not_E_Trgt_E (T : in out Test) is
      pragma Unreferenced (T);
      Tab        : constant Tableau.Tableau_Type := Tableau.Construct;
      Src_Stack  : constant Tableau_Stack.Stack_Type_Access
        := Tab.Get_Stack (1);
      Src_Cards  : constant Some_Cards
        := (Card.Construct (Definitions.Three, Definitions.Diamond),
            Card.Construct (Definitions.Seven, Definitions.Heart),
            Card.Construct (Definitions.King,  Definitions.Heart));
      Src_Exist  : constant Card.Card_Type := Src_Cards (Src_Cards'Last);
      Expected   : constant Tableau.Check_Move_To_Result
        := Tableau.OK;
      Actual     : Tableau.Check_Move_To_Result;
   begin
      for J in Natural range Src_Cards'First .. Src_Cards'Last loop
         Src_Stack.all.Push_Unchecked (Src_Cards (J));
      end loop;
      Actual := Tab.Check_Move_To (1, 2, Src_Exist);
      AUnit.
        Assertions.
          Assert (Expected = Actual,
              "Expected = " & Expected'Image &
                " /= Actual=" & Actual'Image);
   end Check_Move_To_1_Src_Not_E_Trgt_E;

   procedure Check_Move_To_1_Src_Not_E_Trgt_Not_E (T : in out Test) is
      pragma Unreferenced (T);
      Tab        : constant Tableau.Tableau_Type := Tableau.Construct;
      Src_Stack  : constant Tableau_Stack.Stack_Type_Access
        := Tab.Get_Stack (1);
      Src_Cards  : constant Some_Cards
        := (Card.Construct (Definitions.Three, Definitions.Diamond),
            Card.Construct (Definitions.Seven, Definitions.Heart),
            Card.Construct (Definitions.Ten,   Definitions.Heart));
      Src_Exist  : constant Card.Card_Type := Src_Cards (Src_Cards'Last);
      Dst_Stack  : constant Tableau_Stack.Stack_Type_Access
        := Tab.Get_Stack (2);
      Dst_Cards  : constant Some_Cards
        := (Card.Construct (Definitions.Four, Definitions.Heart),
            Card.Construct (Definitions.Jack, Definitions.Club));
      Expected   : constant Tableau.Check_Move_To_Result
        := Tableau.OK;
      Actual     : Tableau.Check_Move_To_Result;
   begin
      for J in Natural range Src_Cards'First .. Src_Cards'Last loop
         Src_Stack.all.Push_Unchecked (Src_Cards (J));
      end loop;
      for J in Natural range Dst_Cards'First .. Dst_Cards'Last loop
         Dst_Stack.all.Push_Unchecked (Dst_Cards (J));
      end loop;
      Actual := Tab.Check_Move_To (1, 2, Src_Exist);
      AUnit.
        Assertions.
          Assert (Expected = Actual,
                  "Expected = " & Expected'Image &
                    " /= Actual=" & Actual'Image);
   end Check_Move_To_1_Src_Not_E_Trgt_Not_E;

   procedure Check_Move_To_1_Src_E_Trgt_E (T : in out Test) is
      pragma Unreferenced (T);
      Tab       : constant Tableau.Tableau_Type := Tableau.Construct;
      Not_Exist : constant Card.Card_Type
        := Card.Construct (Definitions.Queen, Definitions.Club);
      Expected   : constant Tableau.Check_Move_To_Result
        := Tableau.Stack_Empty;
      Actual     : Tableau.Check_Move_To_Result;
   begin
      Actual := Tab.Check_Move_To (1, 1, Not_Exist);
      AUnit.
        Assertions.
          Assert (Expected = Actual,
                  "Expected = " & Expected'Image &
                    " /= Actual=" & Actual'Image);
   end Check_Move_To_1_Src_E_Trgt_E;

   procedure Check_Move_To_1_Src_E_Trgt_Not_E (T : in out Test) is
      pragma Unreferenced (T);
      Tab        : constant Tableau.Tableau_Type := Tableau.Construct;
      Src_Cards  : constant Some_Cards
        := (1 => Card.Construct (Definitions.Ten,  Definitions.Heart));
      Src_Exist  : constant Card.Card_Type := Src_Cards (Src_Cards'Last);
      --
      Dst_Stack  : constant Tableau_Stack.Stack_Type_Access
        := Tab.Get_Stack (2);
      Dst_Cards  : constant Some_Cards
        := (Card.Construct (Definitions.Four, Definitions.Heart),
            Card.Construct (Definitions.Jack, Definitions.Club));
      Expected   : constant Tableau.Check_Move_To_Result
        := Tableau.Stack_Empty;
      Actual     : Tableau.Check_Move_To_Result;
   begin
      for J in Natural range Dst_Cards'First .. Dst_Cards'Last loop
         Dst_Stack.all.Push_Unchecked (Dst_Cards (J));
      end loop;
      Actual := Tab.Check_Move_To (1, 2, Src_Exist);
      AUnit.
        Assertions.
          Assert (Expected = Actual,
                  "Expected = " & Expected'Image &
                    " /= Actual=" & Actual'Image);
   end Check_Move_To_1_Src_E_Trgt_Not_E;

   procedure Check_Move_To_X_Trgt_Does_Not_Accept (T : in out Test) is
      pragma Unreferenced (T);
      Tab       : constant Tableau.Tableau_Type := Tableau.Construct;
      --
      Src_Stack : constant Tableau_Stack.Stack_Type_Access
        := Tab.Get_Stack (1);
      Src_Cards : constant Some_Cards
        := (Card.Construct (Definitions.Three, Definitions.Diamond),
            Card.Construct (Definitions.Seven, Definitions.Heart),
            Card.Construct (Definitions.Jack,  Definitions.Heart));
      Src_Exist : constant Card.Card_Type := Src_Cards (1);
      --
      Dst_Stack : constant Tableau_Stack.Stack_Type_Access
        := Tab.Get_Stack (2);
      Dst_Cards : constant Some_Cards
        := (1 => Card.Construct (Definitions.Ten, Definitions.Diamond));
      Expected   : constant Tableau.Check_Move_To_Result
        := Tableau.Destination_Stack_Does_Not_Accept;
      Actual     : Tableau.Check_Move_To_Result;
   begin
      for J in Natural range Src_Cards'First .. Src_Cards'Last loop
         Src_Stack.all.Push_Unchecked (Src_Cards (J));
      end loop;
      for J in Natural range Dst_Cards'First .. Dst_Cards'Last loop
         Dst_Stack.all.Push_Unchecked (Dst_Cards (J));
      end loop;
      Actual := Tab.Check_Move_To (1, 2, Src_Exist);
      AUnit.
        Assertions.
          Assert (Expected = Actual,
                  "Expected = " & Expected'Image &
                    " /= Actual=" & Actual'Image);
   end Check_Move_To_X_Trgt_Does_Not_Accept;

   procedure Check_Move_To_X_Src_Not_E_Trgt_E (T : in out Test) is
      pragma Unreferenced (T);
      Tab        : constant Tableau.Tableau_Type := Tableau.Construct;
      --
      Src_Stack  : constant Tableau_Stack.Stack_Type_Access
        := Tab.Get_Stack (1);
      Src_Cards  : constant Some_Cards
        := (Card.Construct (Definitions.Three, Definitions.Diamond),
            Card.Construct (Definitions.King,  Definitions.Heart),
            Card.Construct (Definitions.Seven, Definitions.Heart));
      Src_Exist  : constant Card.Card_Type := Src_Cards (2);
      --
      Expected   : constant Tableau.Check_Move_To_Result
        := Tableau.OK;
      Actual     : Tableau.Check_Move_To_Result;
   begin
      for J in Natural range Src_Cards'First .. Src_Cards'Last loop
         Src_Stack.all.Push_Unchecked (Src_Cards (J));
      end loop;
      --
      Actual := Tab.Check_Move_To (1, 2, Src_Exist);
      AUnit.
        Assertions.
          Assert (Expected = Actual,
                  "Expected = " & Expected'Image &
                    " /= Actual=" & Actual'Image);
   end Check_Move_To_X_Src_Not_E_Trgt_E;

   procedure Check_Move_To_X_Src_Not_E_Trgt_Not_E (T : in out Test) is
      pragma Unreferenced (T);
      Tab        : constant Tableau.Tableau_Type := Tableau.Construct;
      --
      Src_Stack  : constant Tableau_Stack.Stack_Type_Access
        := Tab.Get_Stack (1);
      Src_Cards  : constant Some_Cards
        := (Card.Construct (Definitions.Three, Definitions.Diamond),
            Card.Construct (Definitions.Ten,   Definitions.Heart),
            Card.Construct (Definitions.Seven, Definitions.Heart));
      Src_Exist  : constant Card.Card_Type := Src_Cards (2);
      Dst_Stack  : constant Tableau_Stack.Stack_Type_Access
        := Tab.Get_Stack (2);
      Dst_Cards  : constant Some_Cards
        := (Card.Construct (Definitions.Four, Definitions.Heart),
            Card.Construct (Definitions.Jack, Definitions.Club));
      Expected   : constant Tableau.Check_Move_To_Result
        := Tableau.OK;
      Actual     : Tableau.Check_Move_To_Result;
   begin
      for J in Natural range Src_Cards'First .. Src_Cards'Last loop
         Src_Stack.all.Push_Unchecked (Src_Cards (J));
      end loop;
      for J in Natural range Dst_Cards'First .. Dst_Cards'Last loop
         Dst_Stack.all.Push_Unchecked (Dst_Cards (J));
      end loop;
      Actual := Tab.Check_Move_To (1, 2, Src_Exist);
      AUnit.
        Assertions.
          Assert (Expected = Actual,
                  "Expected = " & Expected'Image &
                    " /= Actual=" & Actual'Image);
   end Check_Move_To_X_Src_Not_E_Trgt_Not_E;

   procedure Check_Move_To_X_Src_E_Trgt_Not_E (T : in out Test) is
      pragma Unreferenced (T);
      Tab       : constant Tableau.Tableau_Type := Tableau.Construct;
      Dst_Stack  : constant Tableau_Stack.Stack_Type_Access
        := Tab.Get_Stack (2);
      Dst_Cards  : constant Some_Cards
        := (Card.Construct (Definitions.Four, Definitions.Heart),
            Card.Construct (Definitions.Jack, Definitions.Club));
      Not_Exist  : constant Card.Card_Type
        := Card.Construct (Definitions.Queen, Definitions.Club);
      Expected   : constant Tableau.Check_Move_To_Result
        := Tableau.Stack_Empty;
      Actual     : Tableau.Check_Move_To_Result;
   begin
      for J in Natural range Dst_Cards'First .. Dst_Cards'Last loop
         Dst_Stack.all.Push_Unchecked (Dst_Cards (J));
      end loop;
      Actual := Tab.Check_Move_To (1, 1, Not_Exist);
      AUnit.
        Assertions.
          Assert (Expected = Actual,
                  "Expected = " & Expected'Image &
                    " /= Actual=" & Actual'Image);
   end Check_Move_To_X_Src_E_Trgt_Not_E;

   --------------------------------------------------------------------
   --  the test suit construction
   package Caller is new AUnit.Test_Caller
     (Tableau.Tests4_Check_Move_To.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
      N   : constant String := "Tableau.Tests4_Check_Move_To.";
   begin
      Ret.
        Add_Test (Caller.
                    Create (N & "Check_Move_To_Src_Stack_Equals_Trgt_Stack",
                      Check_Move_To_Src_Stack_Equals_Trgt_Stack'Access));
      Ret.
        Add_Test (Caller.
                    Create (N &
                        "Check_Move_To_Source_Selection_Does_Not_Exist",
                      Check_Move_To_Source_Selection_Does_Not_Exist'Access));
      Ret.
        Add_Test (Caller.
                    Create (N & "Check_Move_To_1_Trgt_Does_Not_Accept",
                      Check_Move_To_1_Trgt_Does_Not_Accept'Access));
      Ret.
        Add_Test (Caller.
                    Create (N & "Check_Move_To_1_Src_Not_E_Trgt_E",
                      Check_Move_To_1_Src_Not_E_Trgt_E'Access));
      Ret.
        Add_Test (Caller.
                    Create (N & "Check_Move_To_1_Src_Not_E_Trgt_Not_E",
                      Check_Move_To_1_Src_Not_E_Trgt_Not_E'Access));
      Ret.
        Add_Test (Caller.
                    Create (N & "Check_Move_To_1_Src_E_Trgt_E",
                      Check_Move_To_1_Src_E_Trgt_E'Access));
      Ret.
        Add_Test (Caller.
                    Create (N & "Check_Move_To_1_Src_E_Trgt_Not_E",
                      Check_Move_To_1_Src_E_Trgt_Not_E'Access));
      Ret.
        Add_Test (Caller.
                    Create (N & "Check_Move_To_X_Trgt_Does_Not_Accept",
                      Check_Move_To_X_Trgt_Does_Not_Accept'Access));
      Ret.
        Add_Test (Caller.
                    Create (N & "Check_Move_To_X_Src_Not_E_Trgt_E",
                      Check_Move_To_X_Src_Not_E_Trgt_E'Access));
      Ret.
        Add_Test (Caller.
                    Create (N & "Check_Move_To_X_Src_Not_E_Trgt_Not_E",
                      Check_Move_To_X_Src_Not_E_Trgt_Not_E'Access));
      Ret.
        Add_Test (Caller.
                    Create (N & "Check_Move_To_X_Src_E_Trgt_Not_E",
                      Check_Move_To_X_Src_E_Trgt_Not_E'Access));

      return Ret;
   end Suite;

end Tableau.Tests4_Check_Move_To;
