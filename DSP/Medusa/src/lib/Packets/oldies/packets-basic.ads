--                              -*- Mode: Ada -*-
--  Filename        : packets-byte_packets.ads
--  Description     : Basic packet type. Used for deriving other types
--  Author          : Finta Tartaruga
--  Created On      : Wed Feb 13 17:36:54 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

--
-- In MEDUSA there are several different types of packets "running
-- around": multimedia packets, components, reduced component,
-- network packets and network crumbs.  Among those, the "external"
-- packets (multimedia, network and crumbs) are actually "collection of
-- bytes"
--
generic
   type Payload_Type   is private;
   type Payload_Array  is private;
   type Timestamp_Type is private;
package Byte_Packets is
   type Basic_Packet is tagged;

   procedure Resize (Buffer : in out Basic_Packet;
                     Size   : in     Natural);

   function Payload (P : Basic_Packet'Class) return Payload_Array;
private
   type Payload_Array_Access is access Payload_Array;

   type Payload_Buffer is new Controlled with record
      Pt   : Payload_Array_Access;
      Size : Natural := 0;
   end Byte_Buffer;

   procedure Resize (Buffer : in out Payload_Buffer;
                     Size   : in     Natural);

   procedure Finalize (Object: in out Payload_Buffer);
   procedure Adjust   (Object: in out Payload_Buffer);

   type Basic_Packet is record
      Time   : Timestamp_Type;
      Buffer : Byte_Buffer;
   end record;
end Packets.Byte_Packets;
