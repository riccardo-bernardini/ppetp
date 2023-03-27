--                              -*- Mode: Ada -*-
--  Filename        : packets-protocol-parsing.ads
--  Description     : Definition of packet parser
--  Author          :
--  Created On      : Mon Nov  3 18:05:13 2008
--  Last Modified By: Roberto Cesco Fabbro
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

--
-- This package provides the definitions of types and methods
-- for parsing the packets received from the network.  The most
-- important resource provided by this package is the type
-- Packet_Parser representing a parser of packets. The most
-- important primitive operation of Packet_Parser is
-- Parse_Packet.
--
-- A Packet_Parser must be initialized with the corresponding
-- profile.  If such an inizialization is not done, the parser
-- will be able to parse only profile indipendent packets (e.g.
-- ACK packets).
--

with Ada.Finalization;               use Ada.Finalization;
with Ada.Streams;                    use Ada.Streams;
with Network;                        use Network;
--with Profiles; 			     use Profiles;
with Packets.Binary.Network;         use Packets.Binary.Network;
with byte_arrays;			use byte_arrays;


package Packets.Protocol.Parsing is




   -- ============ --
   -- == PARSER == --
   -- ============ --


   -- Look if the Packet has the PPETP-Magic number
   function Is_PPETP(Data: Byte_Array_Pt) return Boolean;


   --
   -- Convert a bitstring to an internal packet.
   --
   function Parse_Packet (Packet  : Network_Packet;
                          PeerID : PPETP.Peer_ID)
                          return Protocol_Packet'Class;



end Packets.Protocol.Parsing;
