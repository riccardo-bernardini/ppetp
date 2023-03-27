--                              -*- Mode: Ada -*-
--  Filename        : packets-protocol-command-parsing.ads
--  Description     : Functions for parsing command packets
--  Author          : Riccardo Bernardini
--  Created On      : Wed Nov  5 09:53:26 2008
--  Last Modified By: Roberto Cesco Fabbro
--  Last Modified On: Thu, Feb 5 10:10:22 2009
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

with PPETP;
with Packets.Binary.Network;        use Packets.Binary.Network;
with Profiles.Parsers;              use Profiles.Parsers;
with Profiles;			    use Profiles;

package Packets.Protocol.Command.Parsing is
   -- Parse the binary data in Source using Parser for parsing
   -- the profile-related part.  Parser can be null if no
   -- profile-related parts are present in Source.
   -- If Source is not a command packet, exception
   -- Invalid_Packet_Type is raised. If Source has profile-related
   -- parts and Parser is null, Profile_Required is raised.
   function Parse_Command_Packet (Source : Network_Packet;
                                  PeerID : PPETP.Peer_ID)
                                  return Control_Packet;


   -- Check if it is a Routed Packet
   function Is_Routed(Packet: Network_Packet) return Boolean;

end Packets.Protocol.Command.Parsing;
