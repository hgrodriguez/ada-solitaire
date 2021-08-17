with Ada.Text_IO;
with Ada.Characters.Handling;

package body Text_Menu is

   function Top_Menu_Selection return Top_MIs is
      User_Wants : Character;
   begin
      loop
         Ada.
           Text_IO.
             Put_Line (Item => "What do you want: "
                       & "H)elp, O)ptions, P)lay, Q)uit:");
         Ada.Text_IO.Get (Item => User_Wants);
         User_Wants := Ada.Characters.Handling.To_Upper (User_Wants);
         case User_Wants is
            when 'H'
               => return Help;
            when 'O'
               => return Options;
            when 'P'
               => return Play;
            when 'Q'
               => return Quit;
            when others
               => Ada.Text_IO.Put_Line (Item => "Wrong input. Try again.");
         end case;
      end loop;
   end Top_Menu_Selection;

   function Options_Menu_Selection return Options_MIs is
      User_Wants : Character;
   begin
      loop
         Ada.
           Text_IO.
             Put_Line (Item => "What do you want: "
                       & "H)elp," &
                         " Q)uit:");
         Ada.Text_IO.Get (Item => User_Wants);
         User_Wants := Ada.Characters.Handling.To_Upper (User_Wants);
         case User_Wants is
            when 'H'
               => return Help;
            when 'Q'
               => return Quit;
            when others
               => Ada.Text_IO.Put_Line (Item => "Wrong input. Try again.");
         end case;
      end loop;
   end Options_Menu_Selection;

   function Play_Menu_Selection return Play_MIs is
      User_Wants : Character;
   begin
      loop
         Ada.
           Text_IO.
             Put_Line (Item => "What do you want: "
                       & "H)elp, M)ove, F)etch, P)eek, R)estart,"
                       & " Q)uit:");
         Ada.Text_IO.Get (Item => User_Wants);
         User_Wants := Ada.Characters.Handling.To_Upper (User_Wants);
         case User_Wants is
            when 'H'
               => return Help;
            when 'M'
               => return Move;
            when 'F'
               => return Fetch;
            when 'P'
               => return Peek;
            when 'R'
               => return Restart;
            when 'Q'
               => return Quit;
            when others
               => Ada.Text_IO.Put_Line (Item => "Wrong input. Try again.");
         end case;
      end loop;
   end Play_Menu_Selection;

end Text_Menu;
