with PPETP;				use PPETP;
with PPETP.Attributes.Ack_Target;		use PPETP.Attributes.Ack_Target;

with Packets.Binary.Network;        use Packets.Binary.Network;
with Profiles.Builders;             use Profiles.Builders;


package Packets.Protocol.Command.Building is

   function Make_Control_Packet (Source     : Control_Packet;
                                 Routed     : Boolean;
                                 Target_ID  : Peer_ID;
                                 Ack_Target : ACK_TARGET_Attribute)
                                 return Network_Packet;

end Packets.Protocol.Command.Building;
