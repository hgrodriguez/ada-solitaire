with Card;
with Tableau;

package Move_Candidate is

   type Move_Candidate_Type is tagged private;

   --------------------------------------------------------------------
   --
   function Construct (Card_To_Include : Card.Card_Type;
                       Stack_Number    : Tableau.Valid_Stacks_Range;
                       Rank_Delta      : Integer) return Move_Candidate_Type;

   --------------------------------------------------------------------
   --
   function Get_Card_To_Include (MC : Move_Candidate_Type)
                                 return Card.Card_Type;
   --------------------------------------------------------------------
   --
   function Get_Stack_Number (MC : Move_Candidate_Type)
                              return Tableau.Valid_Stacks_Range;

   --------------------------------------------------------------------
   --
   function Get_Rank_Delta (MC : Move_Candidate_Type) return Integer;

private
   type Move_Candidate_Type is tagged
      record
         Card_To_Include : Card.Card_Type;
         Stack_Number    : Tableau.Valid_Stacks_Range;
         Rank_Delta      : Integer;
      end record;

end Move_Candidate;
