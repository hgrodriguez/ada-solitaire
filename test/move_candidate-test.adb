with Card;
with Deck;

with AUnit.Assertions;
with AUnit.Test_Caller;

package body Move_Candidate.Test is

   TEN_HEART    : constant Card.Card_Type := Card.Construct (Deck.Ten,
                                                          Deck.Heart);
   STACK_NUMBER : constant Tableau.Valid_Stacks_Range := 3;
   RANK_DELTA   : constant Integer := 5;

   --------------------------------------------------------------------
   --
   procedure Construct (T : in out Test) is
      pragma Unreferenced (T);
      MC : Move_Candidate_Type;
      pragma Warnings (Off, MC);
   begin
      MC := Move_Candidate.Construct (Card_To_Include => TEN_HEART,
                                      Stack_Number    => STACK_NUMBER,
                                      Rank_Delta      => RANK_DELTA);
   end Construct;

   --------------------------------------------------------------------
   --
   procedure Construct_Check_Card_To_Include (T : in out Test) is
      pragma Unreferenced (T);
      MC : constant Move_Candidate_Type
        := Move_Candidate.Construct (Card_To_Include => TEN_HEART,
                                     Stack_Number    => STACK_NUMBER,
                                     Rank_Delta      => RANK_DELTA);
      R  : constant Card.Card_Type := MC.Get_Card_To_Include;
      use Card; --  for "=" accessible
   begin
      AUnit.Assertions.Assert (R = TEN_HEART,
                               "Card= " & R.Short_Image &
                                 " /= Expected=" & TEN_HEART.Short_Image);
   end Construct_Check_Card_To_Include;

   --------------------------------------------------------------------
   --
   procedure Construct_Check_Stack_Number (T : in out Test) is
      pragma Unreferenced (T);
      MC : constant Move_Candidate_Type
        := Move_Candidate.Construct (Card_To_Include => TEN_HEART,
                                     Stack_Number    => STACK_NUMBER,
                                     Rank_Delta      => RANK_DELTA);
      R  : constant Tableau.Valid_Stacks_Range := MC.Get_Stack_Number;
      use Tableau; --  for "=" accessible
   begin
      AUnit.Assertions.Assert (R = STACK_NUMBER,
                               "Stack_Number= " & R'Image &
                                 " /= Expected=" & STACK_NUMBER'Image);
   end Construct_Check_Stack_Number;

   --------------------------------------------------------------------
   --
   procedure Construct_Check_Rank_Delta (T : in out Test) is
      pragma Unreferenced (T);
      MC : constant Move_Candidate_Type
        := Move_Candidate.Construct (Card_To_Include => TEN_HEART,
                                     Stack_Number    => STACK_NUMBER,
                                     Rank_Delta      => RANK_DELTA);
      R  : constant Integer := MC.Get_Rank_Delta;
   begin
      AUnit.Assertions.Assert (R = RANK_DELTA,
                               "Rank_Delta= " & R'Image &
                                 " /= Expected=" & RANK_DELTA'Image);
   end Construct_Check_Rank_Delta;

   --------------------------------------------------------------------
   --  test suite construction
   package Caller is new AUnit.Test_Caller (Move_Candidate.Test.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
      N   : constant String := "Move_Candidate.Test.";
   begin
      --  Construct
      Ret.Add_Test (Caller.Create (N & "Construct",
                    Construct'Access));

      --  Checks
      Ret.Add_Test (Caller.Create (N & "Construct_Check_Card_To_Include",
                    Construct_Check_Card_To_Include'Access));
      Ret.Add_Test (Caller.Create (N & "Construct_Check_Stack_Number",
                    Construct_Check_Stack_Number'Access));
      Ret.Add_Test (Caller.Create (N & "Construct_Check_Rank_Delta",
                    Construct_Check_Rank_Delta'Access));
      --
      return Ret;
   end Suite;

end Move_Candidate.Test;
