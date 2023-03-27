with Packets.Binary.Network;    use Packets.Binary.Network;

package Packets.Protocol.Data.Building is
   function Make_Data_Packet (Source  : Data_Packet)
                             return Network_Packet;
end Packets.Protocol.Data.Building;
