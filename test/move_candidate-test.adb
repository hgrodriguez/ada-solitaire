with Card;
with Deck;

--  with AUnit.Assertions;
with AUnit.Test_Caller;

package body Move_Candidate.Test is

   --------------------------------------------------------------------
   --
   procedure Construct (T : in out Test) is
      pragma Unreferenced (T);
      C  : constant Card.Card_Type := Card.Construct (Rank => Deck.Ten,
                                                      Suit => Deck.Heart);
      MC : Move_Candidate_Type;
      pragma Warnings (Off, MC);
   begin
      MC := Move_Candidate.Construct (Card_To_Include => C,
                                      Stack_Number    => 3,
                                      Rank_Delta      => 5);
   end Construct;

   --------------------------------------------------------------------
   --  test suite construction
   package Caller is new AUnit.Test_Caller (Move_Candidate.Test.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
      N   : constant String := "Move_Candidate.Test.";
   begin
      Ret.Add_Test (Caller.Create (N & "Construct",
                    Construct'Access));

      return Ret;
   end Suite;

end Move_Candidate.Test;
