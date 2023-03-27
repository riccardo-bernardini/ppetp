with Ada.Streams;
use  Ada.Streams;

with Stream_Element_Buffers;

package Packets.External is
   type External_Packet is abstract tagged private;

   procedure Write (Packet : in out External_Packet'Class;
                    Value  : in     Integer;
                    N_Bits : in     Positive);

   procedure Write (Packet : in out External_Packet'Class;
                    Value  : in     Integer;
                    Start  : in     Stream_Element_Offset;
                    N_Bits : in     Positive);

   procedure Append (Packet : in out External_Packet'Class;
                     Data   : in     External_Packet'Class);

   procedure Resize (Packet : in out External_Packet'Class;
                     Size   : in     Natural);

   procedure Rewind (Packet : in out External_Packet'Class);

   procedure Read (Packet : in out External_Packet'Class;
                   Value  :    out Integer;
                   N_Bits : in     Positive);

   procedure Read (Packet : in out External_Packet'Class;
                   Value  :    out Integer;
                   Start  : in     Stream_Element_Offset;
                   N_Bits : in     Positive);

   procedure Dump (Packet : in     External_Packet'Class;
                   Data   :    out Stream_Element_Array);

   procedure Load (Packet :    out External_Packet'Class;
                   Data   : in     Stream_Element_Array);

   function Payload_Size (Packet : External_Packet'Class)
     return Natural;
private
   type External_Packet is abstract tagged record
      Cursor : Stream_Element_Offset := 0;
      Buffer : Stream_Element_Buffers.Buffer;
   end record;
end Packets.External;
