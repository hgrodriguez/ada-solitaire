with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Cards.Test is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private
   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Rank_Is_Equal_To_Using_Identical_Rank (T : in out Test);
   procedure Rank_Is_Equal_To_Using_Equal_Rank (T : in out Test);
   --  those are testing the function "="
   procedure Rank_IsEqualTo_Using_Identical_Rank (T : in out Test);
   procedure Rank_IsEqualTo_Using_Equal_Rank (T : in out Test);

   procedure Rank_Is_Higher_Than_True (T : in out Test);
   procedure Rank_Is_Higher_Than_False (T : in out Test);
   procedure Rank_Is_Lower_Than_True (T : in out Test);
   procedure Rank_Is_Lower_Than_False (T : in out Test);

   procedure Suit_Is_Equal_To_Using_Identical_Suit (T : in out Test);
   procedure Suit_Is_Equal_To_Using_Equal_Suit (T : in out Test);
   --  those are testing the function "="
   procedure Suit_IsEqualTo_Using_Identical_Suit (T : in out Test);
   procedure Suit_IsEqualTo_Using_Equal_Suit (T : in out Test);

   procedure Is_Equal_To_Identical_Rank_Identical_Suit (T : in out Test);
   procedure Is_Equal_To_Equal_Rank_Identical_Suit (T : in out Test);
   procedure Is_Equal_To_Identical_Rank_Equal_Suit (T : in out Test);
   procedure Is_Equal_To_Equal_Rank_Equal_Suit (T : in out Test);

   --  those are testing the function "="
   procedure IsEqualTo_Identical_Rank_Identical_Suit (T : in out Test);
   procedure IsEqualTo_Equal_Rank_Identical_Suit (T : in out Test);
   procedure IsEqualTo_Identical_Rank_Equal_Suit (T : in out Test);
   procedure IsEqualTo_Equal_Rank_Equal_Suit (T : in out Test);

end Cards.Test;
