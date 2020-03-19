--  helper for collecting Card.Short_Image_Types for printing
with Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;

with Card;

package Short_Image_FIFO is

   type Short_Image_FIFO_Type is tagged private;
   type Short_Image_FIFO_Type_Access is access all Short_Image_FIFO_Type;

   --------------------------------------------------------------------
   --  construct a short image fifo
   function Construct return Short_Image_FIFO_Type;

   --------------------------------------------------------------------
   --  check, if fifo is empty
   function Is_Empty (SIF : Short_Image_FIFO_Type) return Boolean;

   --------------------------------------------------------------------
   --  return size/depth of fifo
   function Size (SIF : Short_Image_FIFO_Type) return Natural;

   --------------------------------------------------------------------
   --  put one short image into the fifo
   procedure Put (SIF : in out Short_Image_FIFO_Type;
                  CSI : Card.Short_Image_Type);
   --------------------------------------------------------------------
   --  get one short image into the fifo
   --  if fifo is empty -> exception
   Short_Image_FIFO_Empty_Exception : exception;
   function Get (SIF : in out Short_Image_FIFO_Type)
                 return Card.Short_Image_Type;

private
   package Short_Image_DLL is new Ada.Containers.
     Doubly_Linked_Lists (Element_Type => Card.Short_Image_Type);

   type Short_Image_FIFO_Type is tagged record
      SIDLL : Short_Image_DLL.List;
   end record;

end Short_Image_FIFO;
