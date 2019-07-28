with Ada.Exceptions;

with AUnit.Assertions;
with AUnit.Test_Caller;

with Card;

package body Stock.Test is

   procedure Construct (T : in out Test) is
      pragma Unreferenced (T);
      S             : constant Stock.Stock_Type := Stock.Construct;
      Expected_Size : constant Natural := 52;
   begin
      AUnit.Assertions.Assert (S.Size = Expected_Size,
                               "Stock.Construct failed");
   end Construct;

   procedure Fetch_One_Card (T : in out Test) is
      pragma Unreferenced (T);
      S : constant Stock.Stock_Type := Stock.Construct;
      C : constant Card.Card_Type := S.Fetch;
      pragma Warnings (Off, C);
      Expected_Size : constant Natural := 51;
   begin
      AUnit.Assertions.Assert (S.Size = Expected_Size,
                               "size=" & Expected_Size'Image & "! /= "
                               & S.Size'Image);
   end Fetch_One_Card;

   procedure Fetch_Two_Cards (T : in out Test) is
      pragma Unreferenced (T);
      S  : constant Stock.Stock_Type := Stock.Construct;
      C1 : constant Card.Card_Type := S.Fetch;
      pragma Warnings (Off, C1);
      C2 : constant Card.Card_Type := S.Fetch;
      pragma Warnings (Off, C2);
      Expected_Size : constant Natural := 50;
   begin
      AUnit.Assertions.Assert (S.Size = Expected_Size,
                               "size=" & Expected_Size'Image & "! /= "
                               & S.Size'Image);
   end Fetch_Two_Cards;

   procedure Fetch_52_Cards (T : in out Test) is
      pragma Unreferenced (T);
      S             : constant Stock.Stock_Type := Stock.Construct;
      C             : Card.Card_Type;
      pragma Warnings (Off, C);
      Expected_Size : constant Natural := 0;
   begin
      for J in Integer range 1 .. 52 loop
         C := S.Fetch;
      end loop;

      AUnit.Assertions.Assert (S.Size = Expected_Size,
                               "size=" & Expected_Size'Image & "! /= "
                               & S.Size'Image);
   end Fetch_52_Cards;

   procedure Fetch_53_Cards_Exception;
   procedure Fetch_53_Cards_Exception is
      S             : constant Stock.Stock_Type := Stock.Construct;
      C             : Card.Card_Type;
      pragma Warnings (Off, C);
   begin
      for J in Integer range 1 .. 53 loop
         C := S.Fetch;
      end loop;
   exception
      when Stock_Empty_Exception => raise;
      when Exc : others =>
         AUnit.Assertions.Assert (False,
                                  "Fetch_53_Cards_Exception: " &
                                    "wrong exception raised:" &
                                    Ada.Exceptions.Exception_Name (Exc));
   end Fetch_53_Cards_Exception;

   procedure Fetch_53_Cards (T : in out Test) is
      pragma Unreferenced (T);

   begin
      AUnit.Assertions.Assert_Exception (Fetch_53_Cards_Exception'Access,
                                         "Fetch_53_Cards: " &
                                           "no exception raised");
   end Fetch_53_Cards;

   --------------------------------------------------------------------
   package Caller is new AUnit.Test_Caller (Stock.Test.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      Ret.Add_Test (Caller.Create ("Stock.Construct", Construct'Access));
      Ret.Add_Test (Caller.Create ("Stock.Fetch_One_Card",
                    Fetch_One_Card'Access));
      Ret.Add_Test (Caller.Create ("Stock.Fetch_Two_Cards",
                    Fetch_Two_Cards'Access));
      Ret.Add_Test (Caller.Create ("Stock.Fetch_52_Cards",
                    Fetch_52_Cards'Access));
      Ret.Add_Test (Caller.Create ("Stock.Fetch_53_Cards",
                    Fetch_53_Cards'Access));
      return Ret;
   end Suite;

end Stock.Test;
