with Packets.Multimedia;
use  Packets.Multimedia;

with Packets.Components;
use  Packets.Components;

package Parsing.Trivial is
   Components_Per_Multimedia : constant Integer := 1;

   procedure Synthesize (Input   : in     Component_Array;
                         Output  :    out Multimedia_Packet;
                         Success :    out Boolean);

   procedure Parse (Input   : in     Multimedia_Packet;
                    Output  :    out Component_Array);
end Parsing.Trivial;
