with Ada.Text_IO;

procedure Solitaire is

   type User_Selection is (Help, Start, Quit);

   User_Input : User_Selection;

   function Get_User_Selection return User_Selection;
   procedure Show_Help;

   function Get_User_Selection return User_Selection is
      User_Wants : Character;
   begin
      loop
         Ada.
           Text_IO.
             Put_Line (Item => "What do you want: H)elp, S)tart, Q)uit:");
         Ada.Text_IO.Get (Item => User_Wants);
         case User_Wants is
            when 'H'
               => return Help;
            when 'S'
               => return Start;
            when 'Q'
               => return Quit;
            when others
               => Ada.Text_IO.Put_Line (Item => "Wrong input. Try again.");
         end case;
      end loop;
   end Get_User_Selection;

   procedure Show_Help is
   begin
      Ada.Text_IO.Put_Line (Item => "HELP TEXT");
   end Show_Help;

begin
   Ada.Text_IO.Put_Line (Item => "Welcome to the Solitaire Game");
   loop
      User_Input := Get_User_Selection;
      case User_Input is
         when Help => Show_Help;
         when Start => null;
         when Quit => exit;
      end case;
   end loop;
   Ada.Text_IO.Put_Line (Item => "Buy");
end Solitaire;
