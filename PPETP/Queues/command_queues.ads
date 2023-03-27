with Generic_Shared_Queue;
with Packets.Internal;
use  Packets.Internal;

package Command_Queues is
   new Generic_Shared_Queue (Element => Internal_Command_Pt);


