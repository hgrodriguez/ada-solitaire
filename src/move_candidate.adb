package body Move_Candidate is

   --------------------------------------------------------------------
   --
   function Construct (Card_To_Include : Card.Card_Type;
                       Stack_Number    : Tableau.Valid_Stacks_Range;
                       Rank_Delta      : Integer) return Move_Candidate_Type is
      MC : Move_Candidate_Type;
   begin
      MC.Card_To_Include := Card_To_Include;
      MC.Stack_Number := Stack_Number;
      MC.Rank_Delta := Rank_Delta;
      return MC;
   end Construct;

end Move_Candidate;
