package Packets.Components.Encoded is
   type Encoded_Component (Size : Natural) is
     new Abstract_Reduced_Component with
      record
         Payload : Stream_Element_Array (1..Size);
      end record;
end  Packets.Components.Encoded;
