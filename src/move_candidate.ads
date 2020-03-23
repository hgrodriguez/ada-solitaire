with Card;
with Tableau;

package Move_Candidate is

   type Move_Candidate_Type is tagged private;

   --------------------------------------------------------------------
   --
   function Construct (Card_To_Include : Card.Card_Type;
                       Stack_Number    : Tableau.Valid_Stacks_Range;
                       Rank_Delta      : Integer) return Move_Candidate_Type;

private
   type Move_Candidate_Type is tagged
      record
         Card_To_Include : Card.Card_Type;
         Stack_Number    : Tableau.Valid_Stacks_Range;
         Rank_Delta      : Integer;
      end record;

end Move_Candidate;
