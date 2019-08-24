with Tableau.Test;
with Tableau.Tests4_Remove_Mandatory_Cards;
with Tableau.Tests4_To_String;
with Tableau.Tests4_To_String_One_Line;

package body Tableau.All_Tests is

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      Ret.Add_Test (Tableau.Test.Suite);
      Ret.Add_Test (Tableau.Tests4_Remove_Mandatory_Cards.Suite);
      Ret.Add_Test (Tableau.Tests4_To_String.Suite);
      Ret.Add_Test (Tableau.Tests4_To_String_One_Line.Suite);
      return Ret;
   end Suite;

end Tableau.All_Tests;
