package Text_Menu is

   type Top_MIs is (Help, Options, Play, Quit);
   function Top_Menu_Selection return Top_MIs;

   type Options_MIs is (Help, Quit);
   function Options_Menu_Selection return Options_MIs;

   type Play_MIs is (Help, Move, Fetch, Peek, Restart, Quit);
   function Play_Menu_Selection return Play_MIs;

end Text_Menu;
