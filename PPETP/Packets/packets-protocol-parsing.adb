with Packets.Protocol.Data.Parsing;
with Packets.Protocol.Command.Parsing;
with Interfaces;                     use Interfaces;
--with Profiles;                       use Profiles;
with PPETP;			use PPETP;

with Ada.Unchecked_Deallocation;

package body Packets.Protocol.Parsing is

   --------------
   -- Is_PPETP --
   --------------
   function Is_PPETP(Data: Byte_Array_Pt) return Boolean is
   begin
      return PPETP_Magic(Data.all(4)) = PPETP_Magic_Default;
   end Is_PPETP;
   pragma Inline(Is_PPETP);

   ------------------
   -- Parse_Packet --
   ------------------

   function Parse_Packet (Packet : Network_Packet;
                          PeerID : PPETP.Peer_ID)
                          return Protocol_Packet'Class is
   begin
      if Command.Is_Command (Packet) then
         return Command.Parsing.Parse_Command_Packet
           (Source => Packet,
            PeerID => PeerID);
      else
         return Data.Parsing.Parse_Data_Packet
           (Source => Packet);
      end if;
   end Parse_Packet;



end Packets.Protocol.Parsing;
