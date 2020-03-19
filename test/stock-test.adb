with Ada.Exceptions;

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

   procedure Fetch_One_Stock_Not_Empty (T : in out Test) is
      pragma Unreferenced (T);
      S             : constant Stock.Stock_Type := Stock.Construct;
      C             : Card.Card_Type;
      pragma Warnings (Off, C);
      Expected_Size : constant Natural := 52 - 1 * 1;
   begin
      C := S.Fetch_One;
      AUnit.Assertions.Assert (S.Size = Expected_Size,
                               "S.Size=" & Expected_Size'Image & "! /= "
                               & S.Size'Image);
   end Fetch_One_Stock_Not_Empty;

   procedure Fetch_One_Stock_Is_Empty_Exception;
   procedure Fetch_One_Stock_Is_Empty_Exception is
      S        : constant Stock.Stock_Type := Stock.Construct;
      C        : Card.Card_Type;
      pragma Warnings (Off, C);
   begin
      while S.Size > 0 loop
         C := S.Fetch_One;
      end loop;
      C := S.Fetch_One;
   exception
      when Stock_Empty_Exception => raise;
      when Exc : others =>
         AUnit.
           Assertions.
             Assert (False,
                     "Peek_Empty_Stack_Exception: " &
                       "wrong exception raised:" &
                       Ada.Exceptions.Exception_Name (Exc));
   end Fetch_One_Stock_Is_Empty_Exception;

   procedure Fetch_One_Stock_Is_Empty (T : in out Test) is
      pragma Unreferenced (T);

   begin
      AUnit.
        Assertions.
          Assert_Exception (Fetch_One_Stock_Is_Empty_Exception'Access,
                            "Fetch_One_Stock_Is_Empty_Exception: " &
                              "no exception raised");
   end Fetch_One_Stock_Is_Empty;

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

   --  Peek
   procedure Peek_Non_Empty_Stack (T : in out Test) is
      pragma Unreferenced (T);
      S        : constant Stock.Stock_Type := Stock.Construct;
      C        : Card.Card_Type;
      pragma Warnings (Off, C);
   begin
      C := S.Peek;
      AUnit.Assertions.Assert (True,
                               "No exception thrown");
   end Peek_Non_Empty_Stack;

   procedure Peek_Empty_Stack_Exception;
   procedure Peek_Empty_Stack_Exception is
      S        : constant Stock.Stock_Type := Stock.Construct;
      POC      : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
      pragma Warnings (Off, POC);
      C        : Card.Card_Type;
      pragma Warnings (Off, C);
   begin
      while S.Size > 0 loop
         POC := S.Fetch;
      end loop;
      C := S.Peek;
   exception
      when Stock_Empty_Exception => raise;
      when Exc : others =>
         AUnit.
           Assertions.
             Assert (False,
                     "Peek_Empty_Stack_Exception: " &
                       "wrong exception raised:" &
                       Ada.Exceptions.Exception_Name (Exc));
   end Peek_Empty_Stack_Exception;

   procedure Peek_Empty_Stack (T : in out Test) is
      pragma Unreferenced (T);

   begin
      AUnit.
        Assertions.
          Assert_Exception (Peek_Empty_Stack_Exception'Access,
                            "Peek_Empty_Stack_Exception: " &
                              "no exception raised");
   end Peek_Empty_Stack;

   procedure To_String_Non_Empty_And_No_Peek (T : in out Test) is
      pragma Unreferenced (T);
      Expected : constant Card.Short_Image_Type := Card.Obscure_Short_Image;
      Actual   : Card.Short_Image_Type;
      S        : constant Stock.Stock_Type := Stock.Construct;
   begin
      Actual := S.To_String;
      AUnit.Assertions.Assert (Expected = Actual,
                               "Expected=" & Expected &
                                 " /= " & Actual);
   end To_String_Non_Empty_And_No_Peek;

   procedure To_String_Non_Empty_And_Peek (T : in out Test) is
      pragma Unreferenced (T);
      S        : constant Stock.Stock_Type := Stock.Construct;
      Expected : constant Card.Short_Image_Type := S.Peek.Short_Image;
      Actual   : Card.Short_Image_Type;
   begin
      Actual := S.To_String (Peek => True);
      AUnit.Assertions.Assert (Expected = Actual,
                               "Expected=" & Expected &
                                 " /= " & Actual);
   end To_String_Non_Empty_And_Peek;

   procedure To_String_Empty (T : in out Test) is
      pragma Unreferenced (T);
      S        : constant Stock.Stock_Type := Stock.Construct;
      Expected : constant Card.Short_Image_Type := Card.Empty_Short_Image;
      Actual   : Card.Short_Image_Type;
      POC      : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
      pragma Warnings (Off, POC);
   begin
      while S.Size > 0 loop
         POC := S.Fetch;
      end loop;
      Actual := S.To_String;
      AUnit.Assertions.Assert (Expected = Actual,
                               "Expected=" & Expected &
                                 " /= " & Actual);
   end To_String_Empty;

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
                      Create (N & "Fetch_One_Stock_Not_Empty",
                        Fetch_One_Stock_Not_Empty'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Fetch_One_Stock_Is_Empty",
                        Fetch_One_Stock_Is_Empty'Access));

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

      Ret.Add_Test (Caller.
                      Create (N & "Peek_Non_Empty_Stack",
                        Peek_Non_Empty_Stack'Access));
      Ret.Add_Test (Caller.
                      Create (N & "Peek_Empty_Stack",
                        Peek_Empty_Stack'Access));

      Ret.Add_Test (Caller.
                      Create (N & "To_String_Non_Empty_And_No_Peek",
                        To_String_Non_Empty_And_No_Peek'Access));
      Ret.Add_Test (Caller.
                      Create (N & "To_String_Non_Empty_And_Peek",
                        To_String_Non_Empty_And_Peek'Access));
      Ret.Add_Test (Caller.
                      Create (N & "To_String_Empty",
                        To_String_Empty'Access));

      return Ret;
   end Suite;

end Stock.Test;
