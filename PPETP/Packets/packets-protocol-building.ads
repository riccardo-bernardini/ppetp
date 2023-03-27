--                              -*- Mode: Ada -*-
--  Filename        : packets-protocol-building.ads
--  Description     : Procedures to build a network packet
--  Author          : Riccardo Bernardini
--  Created On      : Wed Nov  5 10:26:24 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

--
-- This package provides types and procedures to be used to construct
-- network packets from protocol packets.  The actual  building is
-- done by an object of Packet_Builder type.  If such an object is
-- required to construct packets with profile-related parts, the
-- user must associate with the Packet_Builder a profile and
-- (if needed) a set of default profile parameter values.
--

with PPETP;				use PPETP;
with PPETP.Attributes.Ack_Target;	use PPETP.Attributes.Ack_Target;

with Packets.Protocol.Data;     use Packets.Protocol.Data;
with Packets.Protocol.Command;  use Packets.Protocol.Command;


package Packets.Protocol.Building is
   -- ==================== --
   -- == PACKET_BUILDER == --
   -- ==================== --



   -- Convert a data packet into a raw packet ready to be
   -- sent over the network
   function Make_Packet (Source  : Data_Packet)
                         return Network_Packet;

   -- Convert a data packet into a raw packet ready to be
   -- sent over the network
   function Make_Packet (Source     : Control_Packet;
                         Routed     : Boolean;
                         Target_ID  : Peer_ID;
                         Ack_Target : ACK_TARGET_Attribute)
                         return Network_Packet ;




end Packets.Protocol.Building;
