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

end Short_Image_FIFO;
