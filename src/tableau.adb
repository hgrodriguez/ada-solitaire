package body Tableau is

   function Construct return Tableau_Type is
      T : Tableau_Type;
   begin
      return T;
   end Construct;

   function Size (T : Tableau_Type) return Natural is
      pragma Unreferenced (T);
   begin
      return 0;
   end Size;

end Tableau;
