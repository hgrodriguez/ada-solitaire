with Ada.Text_IO;

with Text_Menu;

with Definitions;
with Deck;
with Card;
with Foundation;
with Stock;
with Tableau;

with Pile_Of_Cards.FIFO;

procedure Solitaire is

   Top_Menu_Input : Text_Menu.Top_MIs;

   procedure Show_Help;
   procedure Show_Help is
   begin
      Ada.Text_IO.Put_Line (Item => "HELP TEXT");
   end Show_Help;

   procedure Play;
   procedure Play is
      F               : Foundation.Foundation_Type;
      S               : Stock.Stock_Type;
      T               : Tableau.Tableau_Type;
      Play_Menu_Input : Text_Menu.Play_MIs;

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
         Foundation_Accepts := F.Accepts;
         Removables := T.Remove_Mandatory_Cards (Foundation_Accepts);
         loop
            exit when Removables.Is_Empty;
            C := Removables.Get;
            F.Put (C);
         end loop;
      end Clean;

      --------------------------------------------------------------------
      --
      procedure Move;
      procedure Move is
         Rank_Char_Input : Character;
         Suit_Char_Input : Character;

         Rank            : Definitions.Ranks_Valid_Range;
         Suit            : Definitions.Suits_Valid_Range;
         C               : Card.Card_Type;

      begin
         Ada.
           Text_IO.
             Put_Line (Item => "Please enter rank [A, 2-9, T-K]: ");
         Ada.Text_IO.Get (Item => Rank_Char_Input);
         if not Deck.Is_Valid_Rank_Short_Image (Rank_Char_Input) then
            Ada.Text_IO.Put_Line ("The Rank given is not an applicable rank.");
            return;
         end if;

         Ada.
           Text_IO.
             Put_Line (Item => "Please enter suit [D, H, C, S]: ");
         Ada.Text_IO.Get (Item => Suit_Char_Input);
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

      end Move;

   begin
      Restart;
      loop
         Ada.Text_IO.Put (Item => F.To_String);
         Ada.Text_IO.Put (Item => "          ");
         Ada.Text_IO.Put (Item => S.To_String);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line (Item => T.To_String);
         Ada.Text_IO.New_Line;
         Play_Menu_Input := Text_Menu.Play_Menu_Selection;
         case Play_Menu_Input is
            when Text_Menu.Help =>
               Show_Help;
            when Text_Menu.Move =>
               Move;
            when Text_Menu.Clean =>
               Clean;
            when Text_Menu.Restart =>
               Restart;
            when Text_Menu.Quit =>
               exit;
         end case;
      end loop;
   end Play;

begin
   Ada.Text_IO.Put_Line (Item => "Welcome to the Solitaire Game");
   loop
      Top_Menu_Input := Text_Menu.Top_Menu_Selection;
      case Top_Menu_Input is
         when Text_Menu.Help => Show_Help;
         when Text_Menu.Play => Play;
         when Text_Menu.Quit => exit;
      end case;
   end loop;
   Ada.Text_IO.Put_Line (Item => "Bye");
end Solitaire;
