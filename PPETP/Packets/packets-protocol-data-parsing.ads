--                              -*- Mode: Ada -*-
--  Filename        : packets-protocol-data-parsing.ads
--  Description     : Functions to parse data packets
--  Author          : Riccardo Bernardini
--  Created On      : Wed Nov  5 10:05:22 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

with Packets.Binary.Network;        use Packets.Binary.Network;
with Profiles;			    use Profiles;

package Packets.Protocol.Data.Parsing is
   -- Parse the binary data in Source using Parser for parsing
   -- the profile-related part.  Parser can never be null,
   -- since a data packet will always have profile-related
   -- parts (at least, the payload).
   -- If Source is not a data packet, exception
   -- Invalid_Packet_Type is raised. If  Parser is null,
   -- Profile_Required is raised.

   function Parse_Data_Packet (Source : Network_Packet)
                               return Data_Packet;
end Packets.Protocol.Data.Parsing;
