with PPETP;				use PPETP;
with PPETP.Attributes.Ack_Target;		use PPETP.Attributes.Ack_Target;

with Packets.Protocol.Data.Building;        use Packets.Protocol.Data.Building;
with Packets.Protocol.Command.Building;     use Packets.Protocol.Command.Building;


package body Packets.Protocol.Building is


   -----------------
   -- Make_Packet --
   -----------------

   function Make_Packet (Source  : Data_Packet)
                         return Network_Packet is
   begin
      return Make_Data_Packet (Source);
   end Make_Packet;

   -----------------
   -- Make_Packet --
   -----------------

   function Make_Packet (Source     : Control_Packet;
                         Routed     : Boolean;
                         Target_ID  : Peer_ID;
                         Ack_Target : ACK_TARGET_Attribute)
                         return Network_Packet is
   begin
      return Make_Control_Packet (Source     => Source,
                                  Routed     => Routed,
                                  Target_ID  => Target_ID,
                                  Ack_Target => Ack_Target);
   end Make_Packet;


end Packets.Protocol.Building;
