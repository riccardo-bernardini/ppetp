package body Packets.Binary.Network is
   function New_Packet (Data        : Byte_Array;
                        Remote_Addr : Sock_Addr_Type := No_Sock_Addr)
                       return Network_Packet is
   begin
      return Result : Network_Packet do
        Result.Set(Data);
        Result.Set_Peer(Remote_Addr);
      end return;
   end New_Packet;

   procedure Set_Peer (Packet      : in out Network_Packet;
                       Remote_Addr : in     Sock_Addr_Type) is
   begin
      Packet.Remote_Peer := Remote_Addr;
   end Set_Peer;

   function Peer (Packet : Network_Packet) return Sock_Addr_Type is
   begin
      return Packet.Remote_Peer;
   end Peer;
end Packets.Binary.Network;
