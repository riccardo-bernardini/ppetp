with Packets.Components.Analog;
with Packets.Reduced.Analog;
with Generic_Packet_Map;
with Generic_Oxidizers;

package Analog_Oxidizers is
   package Analog_Oxi_Pkg is
      new Generic_Oxidizers (Packet_Type => Reduced_Analog_Packet);

   subtype Analog_Oxidizer is Analog_Oxi_Pkg.Generic_Oxidizer;

   procedure Init (Oxidizer  : in out Analog_Oxidizer;
                   N_Packets : in     Positive;
                   Portion   : in     Portion_Type)
     renames Analog_Oxi_Pkg.Init;

   procedure Receive (Oxidizer : in out Analog_Oxidizer;
                      Packet   : in     Reduced_Analog_Packet;
                      Complete :    out Boolean)
     renames Analog_Oxi_Pkg.receive_packet;

   procedure Lost (Oxidizer  : in out Analog_Oxidizer;
                   Timestamp : in      Timestamp_Type)
     renames Analog_Oxi_Pkg.Packet_Lost;

   procedure Recover (Oxidizer         : in out Analog_Oxidizer;
                      Timestamp        : in     Timestamp_Type;
                      Recovered_Packet :    out Analog_Packet;
                      Success          :    out Boolean);

private
   -- package Analog_Packet_Table is
   --    new Generic_Packet_Map (Packet_Type => Reduced_Analog_Packet,
   --                            Index_Type  => Timestamp_Type);
   --
   -- type Analog_Oxidizer is record
   --    Received : Analog_Packet_Table.Packet_Table;
   --    N_Packets : Positive;
   -- end record;
end Analog_Oxidizers;
