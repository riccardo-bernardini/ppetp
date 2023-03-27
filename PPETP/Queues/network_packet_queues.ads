with Generic_Shared_Queue;
with Packets.Binary.Network; use Packets.Binary.Network;

package Network_Packet_Queues is
   new Generic_Shared_Queue (Element => Network_Packet);
