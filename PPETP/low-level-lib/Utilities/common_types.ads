with Ada.Streams;	use Ada.Streams;
with Network;		use Network;
with Ada.Unchecked_Deallocation;


package Common_Types is
   --   type Stream_Array_Pt is access Stream_Element_Array;

   type Socket_Access is access Socket_Type;

   procedure Free is
     new Ada.Unchecked_Deallocation (Socket_Type,
                                     Socket_Access);

end Common_Types;
