with Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;

with Card;

package Short_Image_FIFO is

   type Short_Image_FIFO_Type is tagged private;

   Short_Image_FIFO_Empty_Exception : exception;

   function Construct return Short_Image_FIFO_Type;

   function Is_Empty (SIF : Short_Image_FIFO_Type) return Boolean;
   function Size (SIF : Short_Image_FIFO_Type) return Natural;
   procedure Put (SIF : in out Short_Image_FIFO_Type;
                  CSI : Card.Short_Image_Type);
   function Get (SIF : in out Short_Image_FIFO_Type)
                 return Card.Short_Image_Type;

private
   package Short_Image_DLL is new Ada.Containers.
     Doubly_Linked_Lists (Element_Type => Card.Short_Image_Type);

   type Short_Image_FIFO_Type is tagged record
      SIDLL : Short_Image_DLL.List;
   end record;

end Short_Image_FIFO;
