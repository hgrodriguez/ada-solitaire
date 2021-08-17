with Ada.Text_IO;
with Ada.Characters.Handling;

with Ada.Strings.Unbounded.Text_IO;

with Text_Menu;

with Definitions;
with Deck;
with Card;
with Foundation;
with Stock;
with Tableau;

with Pile_Of_Cards.FIFO;

procedure Solitaire is

   Top_Menu_Input  : Text_Menu.Top_MIs;

   -----------------------------------------------------------------------
   --
   procedure Show_Help;
   procedure Show_Help is
   begin
      Ada.Text_IO.Put_Line (Item => "HELP TEXT");
   end Show_Help;

   -----------------------------------------------------------------------
   --
   procedure Select_Options;
   procedure Select_Options is
      Options_Menu_Input : Text_Menu.Options_MIs;
   begin
      loop
         Options_Menu_Input := Text_Menu.Options_Menu_Selection;
         case Options_Menu_Input is
            when Text_Menu.Help => Show_Help;
            when Text_Menu.Quit => exit;
         end case;
      end loop;
   end Select_Options;

   procedure Play;
   procedure Play is
      F               : Foundation.Foundation_Type;
      S               : Stock.Stock_Type;
      T               : Tableau.Tableau_Type;
      Play_Menu_Input : Text_Menu.Play_MIs;
      Peek_Stock      : Boolean := False;
      Has_Cleaned     : Boolean := False;

      procedure Ansi_Print_Board;
      procedure Ansi_Print_Board is
      begin
         Ada.Strings.Unbounded.Text_IO.Put (F.Ansi_To_String);
         Ada.Text_IO.Put  ("          ");

         Ada.Strings.Unbounded.Text_IO.Put (S.Ansi_To_String (Peek_Stock));
         Ada.Text_IO.New_Line;

         Ada.Text_IO.Put (Item => "=======================");
         Ada.Text_IO.New_Line;

         Ada.Strings.Unbounded.Text_IO.Put (T.Ansi_To_String);
         Ada.Text_IO.New_Line;
      end Ansi_Print_Board;

      procedure Print_Board (As_Ansi : Boolean := True);
      procedure Print_Board (As_Ansi : Boolean := True) is
      begin
         if As_Ansi then
            Ansi_Print_Board;
         else
            Ada.Text_IO.Put (Item => F.To_String);
            Ada.Text_IO.Put (Item => "          ");

            Ada.Text_IO.Put (Item => S.To_String (Peek_Stock));
            Ada.Text_IO.New_Line;

            Ada.Text_IO.Put (Item => "=======================");
            Ada.Text_IO.New_Line;

            Ada.Text_IO.Put_Line (Item => T.To_String);
            Ada.Text_IO.New_Line;
         end if;
      end Print_Board;

      --------------------------------------------------------------------
      --
      procedure Restart;
      procedure Restart is
      begin
         F := Foundation.Construct;
         S := Stock.Construct;
         T := Tableau.Construct;
         T.Init_With (S);
      end Restart;

      --------------------------------------------------------------------
      --
      procedure Clean;
      procedure Clean is
         Foundation_Accepts : Foundation.Acceptable_Type;
         Removables         : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
         C                  : Card.Card_Type;
      begin
         loop
            Foundation_Accepts := F.Accepts;
            Removables := T.Remove_Mandatory_Cards (Foundation_Accepts);
            exit when Removables.Is_Empty;
            Has_Cleaned := True;
            loop
               exit when Removables.Is_Empty;
               C := Removables.Get;
               F.Put (C);
               Print_Board;
            end loop;
         end loop;
      end Clean;

      --------------------------------------------------------------------
      --
      procedure Fetch;
      procedure Fetch is
         Pile : Pile_Of_Cards.FIFO.Pile_Type_FIFO;
      begin
         Pile := S.Fetch;
         T.Push (Pile);
         Print_Board;
         Clean;
      end Fetch;

      --------------------------------------------------------------------
      --
      procedure Move;
      procedure Move is
         Rank_Char_Input : Character;
         Suit_Char_Input : Character;

         Rank            : Definitions.Ranks_Valid_Range;
         Suit            : Definitions.Suits_Valid_Range;
         C               : Card.Card_Type;

         SrcIndex        : Tableau.Valid_Stacks_Range;
         DstIndex_Char   : Character;
         DstIndexValid   : Boolean;
         DstIndex        : Tableau.Valid_Stacks_Range;
         DstIndex_CA     : constant String (1 .. 7) := "1234567";

         CheckMoveResult : Tableau.Check_Move_To_Result;

         use Tableau;

      begin
         Ada.
           Text_IO.
             Put_Line (Item => "Please enter rank [A, 2-9, T-K]: ");
         Ada.Text_IO.Get (Item => Rank_Char_Input);
         Rank_Char_Input := Ada.Characters.Handling.To_Upper (Rank_Char_Input);
         if not Deck.Is_Valid_Rank_Short_Image (Rank_Char_Input) then
            Ada.Text_IO.Put_Line ("The Rank given is not an applicable rank.");
            return;
         end if;

         Ada.
           Text_IO.
             Put_Line (Item => "Please enter suit [D, H, C, S]: ");
         Ada.Text_IO.Get (Item => Suit_Char_Input);
         Suit_Char_Input := Ada.Characters.Handling.To_Upper (Suit_Char_Input);
         Ada.Text_IO.Put_Line (Item => "I got: " &
                                 Rank_Char_Input &
                                 Suit_Char_Input);
         if not Deck.Is_Valid_Suit_Short_Image (Suit_Char_Input) then
            Ada.Text_IO.Put_Line ("The Suit given is not an applicable suit.");
            return;
         end if;

         Rank := Deck.Get_Rank_For_Short_Image (C => Rank_Char_Input);
         Suit := Deck.Get_Suit_For_Short_Image (C => Suit_Char_Input);

         C := Card.Construct (Rank => Rank,
                              Suit => Suit);
         if not T.Has (C) then
            Ada.
              Text_IO.
                Put_Line ("The Card selected does not exist in the tableau.");
            return;
         end if;

         SrcIndex := T.Get_Stack_Index (C);

         Ada.
           Text_IO.
             Put_Line (Item => "Please enter destination stack# [1-7]: ");
         Ada.Text_IO.Get (Item => DstIndex_Char);

         DstIndexValid := False;
         for DstIndex_Check in DstIndex_CA'First .. DstIndex_CA'Last loop
            if DstIndex_Char = DstIndex_CA (DstIndex_Check) then
               DstIndexValid := True;
               exit;
            end if;
         end loop;
         if not DstIndexValid then
            Ada.
              Text_IO.
                Put_Line ("The destination stack # is wrong.");
            return;
         end if;
         DstIndex := Tableau.
           Valid_Stacks_Range (Character'Pos (DstIndex_Char) - 48);

         CheckMoveResult := Tableau.Check_Move_To (T,
                                                   SrcIndex,
                                                   DstIndex,
                                                   C);
         if CheckMoveResult /= Tableau.OK then
            Ada.
              Text_IO.
                Put_Line ("Card not movable.");
            return;
         end if;

         T.Move_To (Src_Index       => SrcIndex,
                    Dst_Index       => DstIndex,
                    Card_To_Include => C);
         Clean;
      end Move;

   begin
      Restart;
      Print_Board;
      Clean;
      Has_Cleaned := False;
      loop
         if T.Size = 0 then
            Ada.
              Text_IO.
                Put_Line ("YOU WON!");
            return;
         end if;
         Play_Menu_Input := Text_Menu.Play_Menu_Selection;
         case Play_Menu_Input is
            when Text_Menu.Help =>
               Show_Help;
            when Text_Menu.Move =>
               Move;
            when Text_Menu.Fetch =>
               Fetch;
            when Text_Menu.Peek =>
               Peek_Stock := not Peek_Stock;
            when Text_Menu.Restart =>
               Restart;
               Clean;
            when Text_Menu.Quit =>
               exit;
         end case;
         if not Has_Cleaned then
            Print_Board;
         end if;
         Has_Cleaned := False;
      end loop;
   end Play;

begin
   Ada.Text_IO.Put (ASCII.ESC & "[2J");
   Ada.Text_IO.Put_Line (Item => "Welcome to the Solitaire Game");
   --  RED
   Ada.Text_IO.Put (ASCII.ESC & "[31m");
   Ada.Text_IO.Put (ASCII.ESC & "[1m");
   Ada.Text_IO.Put ("RED/BOLD");
   Ada.Text_IO.Put (ASCII.ESC & "[0m");
   Ada.Text_IO.New_Line;
   --  BOLD
   --  ITALIC
   loop
      Top_Menu_Input := Text_Menu.Top_Menu_Selection;
      case Top_Menu_Input is
         when Text_Menu.Help => Show_Help;
         when Text_Menu.Options => Select_Options;
         when Text_Menu.Play => Play;
         when Text_Menu.Quit => exit;
      end case;
   end loop;
   Ada.Text_IO.Put_Line (Item => "Bye");
end Solitaire;
