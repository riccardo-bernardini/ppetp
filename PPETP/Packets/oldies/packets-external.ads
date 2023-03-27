--with Packets.Raw;
--use  Packets.Raw;

package Packets.External is
   type External_Packet is
      record
         Data : Raw_Packet_Pt;
      end record;
end Packets.External;

