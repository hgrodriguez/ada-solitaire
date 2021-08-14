package Text_Menu is

   type Top_MIs is (Help, Play, Quit);
   function Top_Menu_Selection return Top_MIs;

   type Play_MIs is (Help, Move, Clean, Fetch, Restart, Quit);
   function Play_Menu_Selection return Play_MIs;

end Text_Menu;
