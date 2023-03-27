--                              -*- Mode: Ada -*-
--  Filename        : output_task.ads
--  Description     : Definition of the task which send data to peers
--  Author          : Roberto Cesco Fabbro
--  Created On      : Mon, Feb 23 2008
--  Last Modified By:
--  Last Modified On:
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

with Network; use Network;
with Network_Packet_Queues; use Network_Packet_Queues;
with Packets.Binary.Network; use Packets.Binary.Network;
with Common_Types; use Common_Types;


package Output_Task is

   task type Writer(Output_Queue: Network_Packet_Queues.Queue_Pt;
                    Inter_Proc_Port : Port_Type) is

      entry Start;
      --entry Start(Address: Sock_Addr_Type);
      --
      -- Create the output socket and bind it with the Address passed as
      -- parameters
      --

      entry Stop;
      --
      -- Close the socket and terminate the task
      --

      entry Send_Priority_Packet(Data: Network_Packet);
      --
      -- Send a packet with higher priority respect the Output Queue
      --

   end Writer;

   type Writer_Task is access Writer;

   procedure Finalize(X : in out Writer_Task);

end Output_Task;
