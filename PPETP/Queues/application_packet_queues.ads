with Packets.Binary.Application;   use  Packets.Binary.Application;

with Generic_Shared_Queue;

package Application_Packet_Queues is
   new Generic_Shared_Queue (Application_Packet);
