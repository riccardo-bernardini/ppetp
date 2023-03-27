with Packets.External.Network; use Packets.External.Network;
with Packets.Network_Crumbs;   use Packets.Network_Crumbs;
with Generic_Synthesis_Queue;
with Merging;

package Network_Synthesis is
   new Generic_Synthesis_Queue(Crumb_Type    => Network_Crumb,
                               Crumb_Array   => Network_Crumb_Array,
                               No_Crumb      => No_Network_Crumb,
                               Complete_Type => Network_Packet,
                               Synthesize    => Merging.Synthesize);
