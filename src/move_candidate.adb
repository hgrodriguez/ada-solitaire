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

   --------------------------------------------------------------------
   --
   function Get_Card_To_Include (MC : Move_Candidate_Type)
                                 return Card.Card_Type is
   begin
      return MC.Card_To_Include;
   end Get_Card_To_Include;

   --------------------------------------------------------------------
   --
   function Get_Stack_Number (MC : Move_Candidate_Type)
                              return Tableau.Valid_Stacks_Range is
   begin
      return MC.Stack_Number;
   end Get_Stack_Number;

   --------------------------------------------------------------------
   --
   function Get_Rank_Delta (MC : Move_Candidate_Type) return Integer is
   begin
      return MC.Rank_Delta;
   end Get_Rank_Delta;

end Move_Candidate;
