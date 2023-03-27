with Ada.Streams;
-- with Conversions;
with Packets.External.Network;
with Packets.Network_Crumbs;
-- with Merging;

package body Input_Data is
   function To_Command_Packet (Data : Ada.Streams.Stream_Element_Array)
                              return Input_Packet_Array is
   begin
      return (1 => (Class   => Command,
                    Command => Conversions.To_String(Data)));
   end To_Command_Packet;

   function To_Crumb_Packet (Data : Ada.Streams.Stream_Element_Array)
                            return Input_Packet_Array is
      use  Packets.External.Network;
      use  Packets.Network_Crumbs;

      Input_Pkt : Network_Packet;
   begin
      Load(Input_Pkt, Data);

      declare
         Crumbs : Network_Crumb_Array := Merging.Split(Input_Pkt);
         Result : Input_Packet_Array(Crumbs'Range);
      begin
         for I in Crumbs'Range loop
            Result(I) := (Class => Crumb,
                          Data  => Crumbs(I));
         end loop;
         return Result;
      end;
   end To_Crumb_Packet;

   function To_Input_Packet (Data  : Ada.Streams.Stream_Element_Array;
                             Class : Input_Packet_Class)
                            return Input_Packet_Array
      is
   begin
      case Class is
         when Command =>
            return To_Command_Packet(Data);
         when Crumb =>
            return To_Crumb_Packet(Data);
      end case;
   end To_Input_Packet;
end Input_Data;
