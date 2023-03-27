with Packets.Components;
use  Packets.Components;

with Packets.Multimedia;
use  Packets.Multimedia;

with Generic_Synthesis_Queue;

generic
   with procedure Synthesize (Input   : in     Component_Array;
                              Output  :    out Multimedia_Packet;
                              Success :    out Boolean)
   is <>;
package Generic_Multimedia_Synthesis is
   new Generic_Synthesis_Queue(Crumb_Type    => Component_Type,
                               Crumb_Array   => Component_Array,
                               Complete_Type => Multimedia_Packet,
                               Synthesize    => Synthesize);



