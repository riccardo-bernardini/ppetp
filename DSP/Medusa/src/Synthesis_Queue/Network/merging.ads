with Packets.Network;
use  Packets.Network;

with Packets.Network_Crumbs;
use  Packets.Network_Crumbs;

package Merging is
   procedure Synthesize (Input   : in     Network_Crumb_Array;
                         Output  :    out Network_Packet;
                         Success :    out Boolean);

   function Split(Input : Network_Pkt) return Network_Crumb_Array;
end Merging;
