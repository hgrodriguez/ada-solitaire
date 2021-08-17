package body Short_Image_FIFO is

   --------------------------------------------------------------------
   --
   function Construct return Short_Image_FIFO_Type is
      SIF : Short_Image_FIFO_Type;
   begin
      return SIF;
   end Construct;

   --------------------------------------------------------------------
   --
   function Is_Empty (SIF : Short_Image_FIFO_Type) return Boolean is
   begin
      return SIF.Size = 0;
   end Is_Empty;

   --------------------------------------------------------------------
   --
   function Size (SIF : Short_Image_FIFO_Type) return Natural is
   begin
      return Natural (Short_Image_DLL.Length (SIF.SIDLL));
   end Size;

   --------------------------------------------------------------------
   --
   procedure Put (SIF : in out Short_Image_FIFO_Type;
                  CSI : Card.Short_Image_Type) is
   begin
      SIF.SIDLL.Append (CSI);
   end Put;

   --------------------------------------------------------------------
   --
   function Get (SIF : in out Short_Image_FIFO_Type)
                 return Card.Short_Image_Type is
      ret_val : Card.Short_Image_Type;
   begin
      if SIF.Is_Empty then
         raise Short_Image_FIFO_Empty_Exception;
      end if;
      ret_val := SIF.SIDLL.First_Element;
      SIF.SIDLL.Delete_First;
      return ret_val;
   end Get;

   function Ansi_Get (SIF : in out Short_Image_FIFO_Type)
                      return Unbounded_String is
      ret_val : Unbounded_String;
      med_val : Card.Short_Image_Type;
   begin
      if SIF.Is_Empty then
         raise Short_Image_FIFO_Empty_Exception;
      end if;
      med_val := SIF.SIDLL.First_Element;
      if med_val (2) = 'D' or med_val (2) = 'H' then
         ret_val := To_Unbounded_String (ASCII.ESC & "[31m"
                              & med_val (1)
                              & med_val (2)
                              & ASCII.ESC & "[0m");
      else
         ret_val := To_Unbounded_String (med_val);
      end if;

      SIF.SIDLL.Delete_First;
      return ret_val;
   end Ansi_Get;

end Short_Image_FIFO;
