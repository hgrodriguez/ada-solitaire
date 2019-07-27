with AUnit.Assertions;
with AUnit.Test_Caller;

package body Stock.Test is

   procedure Construct (T : in out Test) is
      pragma Unreferenced (T);
      S : constant Stock.Stock_Type := Stock.Construct;
   begin
      AUnit.Assertions.Assert (S.Size = 52,
                               "Stock.Construct failed");
   end Construct;

   package Caller is new AUnit.Test_Caller (Stock.Test.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      Ret.Add_Test (Caller.Create ("Stock.Construct", Construct'Access));
      return Ret;
   end Suite;

end Stock.Test;
