with AUnit.Assertions;
with AUnit.Test_Caller;

package body Stock.Test is

   procedure Construct (T : in out Test) is
      pragma Unreferenced (T);
      S             : constant Stock.Stock_Type := Stock.Construct;
      Expected_Size : constant Natural := 52;
   begin
      AUnit.Assertions.Assert (S.Size = Expected_Size,
                               "Stock.Construct failed");
   end Construct;

   procedure Fetch_Once (T : in out Test) is
      pragma Unreferenced (T);
      S             : constant Stock.Stock_Type := Stock.Construct;
      Pile          : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
      Expected_Size : constant Natural := 52 - 1 * 7;
   begin
      Pile := S.Fetch;
      AUnit.Assertions.Assert (Pile.Size = 7,
                               "Pile.Size=7" & "! /= "
                               & Pile.Size'Image);
      AUnit.Assertions.Assert (S.Size = Expected_Size,
                               "S.Size=" & Expected_Size'Image & "! /= "
                               & S.Size'Image);
   end Fetch_Once;

   procedure Fetch_Twice (T : in out Test) is
      pragma Unreferenced (T);
      S             : constant Stock.Stock_Type := Stock.Construct;
      Pile          : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
      pragma Warnings (Off, Pile);
      Expected_Size : constant Natural := 52 - 2 * 7;
   begin
      Pile := S.Fetch;
      Pile := S.Fetch;
      AUnit.Assertions.Assert (Pile.Size = 7,
                               "Pile.Size=7" & "! /= "
                               & Pile.Size'Image);
      AUnit.Assertions.Assert (S.Size = Expected_Size,
                               "size=" & Expected_Size'Image & "! /= "
                               & S.Size'Image);
   end Fetch_Twice;

   procedure Fetch_Thrice (T : in out Test) is
      pragma Unreferenced (T);
      S             : constant Stock.Stock_Type := Stock.Construct;
      Pile          : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
      pragma Warnings (Off, Pile);
      Expected_Size : constant Natural := 52 - 3 * 7;
   begin
      Pile := S.Fetch;
      Pile := S.Fetch;
      Pile := S.Fetch;
      AUnit.Assertions.Assert (Pile.Size = 7,
                               "Pile.Size=7" & "! /= "
                               & Pile.Size'Image);
      AUnit.Assertions.Assert (S.Size = Expected_Size,
                               "size=" & Expected_Size'Image & "! /= "
                               & S.Size'Image);
   end Fetch_Thrice;

   procedure Fetch_4_Times (T : in out Test) is
      pragma Unreferenced (T);
      S             : constant Stock.Stock_Type := Stock.Construct;
      Pile          : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
      pragma Warnings (Off, Pile);
      Expected_Size : constant Natural := 52 - 4 * 7;
   begin
      Pile := S.Fetch;
      Pile := S.Fetch;
      Pile := S.Fetch;
      Pile := S.Fetch;
      AUnit.Assertions.Assert (Pile.Size = 7,
                               "Pile.Size=7" & "! /= "
                               & Pile.Size'Image);
      AUnit.Assertions.Assert (S.Size = Expected_Size,
                               "size=" & Expected_Size'Image & "! /= "
                               & S.Size'Image);
   end Fetch_4_Times;

   procedure Fetch_5_Times (T : in out Test) is
      pragma Unreferenced (T);
      S             : constant Stock.Stock_Type := Stock.Construct;
      Pile          : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
      pragma Warnings (Off, Pile);
      Expected_Size : constant Natural := 52 - 5 * 7;
   begin
      Pile := S.Fetch;
      Pile := S.Fetch;
      Pile := S.Fetch;
      Pile := S.Fetch;
      Pile := S.Fetch;
      AUnit.Assertions.Assert (Pile.Size = 7,
                               "Pile.Size=7" & "! /= "
                               & Pile.Size'Image);
      AUnit.Assertions.Assert (S.Size = Expected_Size,
                               "size=" & Expected_Size'Image & "! /= "
                               & S.Size'Image);
   end Fetch_5_Times;

   procedure Fetch_6_Times (T : in out Test) is
      pragma Unreferenced (T);
      S             : constant Stock.Stock_Type := Stock.Construct;
      Pile          : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
      pragma Warnings (Off, Pile);
      Expected_Size : constant Natural := 52 - 6 * 7;
   begin
      Pile := S.Fetch;
      Pile := S.Fetch;
      Pile := S.Fetch;
      Pile := S.Fetch;
      Pile := S.Fetch;
      Pile := S.Fetch;
      AUnit.Assertions.Assert (Pile.Size = 7,
                               "Pile.Size=7" & "! /= "
                               & Pile.Size'Image);
      AUnit.Assertions.Assert (S.Size = Expected_Size,
                               "size=" & Expected_Size'Image & "! /= "
                               & S.Size'Image);
   end Fetch_6_Times;

   procedure Fetch_7_Times (T : in out Test) is
      pragma Unreferenced (T);
      S             : constant Stock.Stock_Type := Stock.Construct;
      Pile          : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
      pragma Warnings (Off, Pile);
      Expected_Size : constant Natural := 52 - 7 * 7;
   begin
      Pile := S.Fetch;
      Pile := S.Fetch;
      Pile := S.Fetch;
      Pile := S.Fetch;
      Pile := S.Fetch;
      Pile := S.Fetch;
      Pile := S.Fetch;
      AUnit.Assertions.Assert (Pile.Size = 7,
                               "Pile.Size=7" & "! /= "
                               & Pile.Size'Image);
      AUnit.Assertions.Assert (S.Size = Expected_Size,
                               "size=" & Expected_Size'Image & "! /= "
                               & S.Size'Image);
   end Fetch_7_Times;

   procedure Fetch_8_Times (T : in out Test) is
      pragma Unreferenced (T);
      S             : constant Stock.Stock_Type := Stock.Construct;
      Pile          : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
      pragma Warnings (Off, Pile);
      Expected_Size : constant Natural := 0;
   begin
      Pile := S.Fetch;
      Pile := S.Fetch;
      Pile := S.Fetch;
      Pile := S.Fetch;
      Pile := S.Fetch;
      Pile := S.Fetch;
      Pile := S.Fetch;
      Pile := S.Fetch;
      AUnit.Assertions.Assert (Pile.Size = 3,
                               "Pile.Size=7" & "! /= "
                               & Pile.Size'Image);
      AUnit.Assertions.Assert (S.Size = Expected_Size,
                               "size=" & Expected_Size'Image & "! /= "
                               & S.Size'Image);
   end Fetch_8_Times;

   procedure Fetch_9_Times (T : in out Test) is
      pragma Unreferenced (T);
      S             : constant Stock.Stock_Type := Stock.Construct;
      Pile          : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
      pragma Warnings (Off, Pile);
      Expected_Size : constant Natural := 0;
   begin
      Pile := S.Fetch;
      Pile := S.Fetch;
      Pile := S.Fetch;
      Pile := S.Fetch;
      Pile := S.Fetch;
      Pile := S.Fetch;
      Pile := S.Fetch;
      Pile := S.Fetch;
      Pile := S.Fetch;
      AUnit.Assertions.Assert (Pile.Size = 0,
                               "Pile.Size=7" & "! /= "
                               & Pile.Size'Image);
      AUnit.Assertions.Assert (S.Size = Expected_Size,
                               "size=" & Expected_Size'Image & "! /= "
                               & S.Size'Image);
   end Fetch_9_Times;

   --------------------------------------------------------------------
   package Caller is new AUnit.Test_Caller (Stock.Test.Test);

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
      N   : constant String := "Stock.Test.";
   begin
      Ret.Add_Test (Caller.
                      Create (N & "Construct", Construct'Access));

      Ret.Add_Test (Caller.
                      Create (N & "Fetch_Once",
                        Fetch_Once'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Fetch_Twice",
                        Fetch_Twice'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Fetch_Thrice",
                        Fetch_Thrice'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Fetch_4_Times",
                        Fetch_4_Times'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Fetch_5_Times",
                        Fetch_5_Times'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Fetch_6_Times",
                        Fetch_6_Times'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Fetch_7_Times",
                        Fetch_7_Times'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Fetch_8_Times",
                        Fetch_8_Times'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Fetch_9_Times",
                        Fetch_9_Times'Access));
      return Ret;
   end Suite;

end Stock.Test;
