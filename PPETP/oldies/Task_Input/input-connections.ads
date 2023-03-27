with Network;  use Network;
with Packets.Protocol.Parsing;  use Packets.Protocol;
package Input.Connections is
   type Connection_Status is
      record
         Reply_Port  : Port_Type              := No_Port;
         Good_Source : Network.Inet_Addr_Type := No_Inet_Addr;
         Parser      : Parsing.Packet_Parser;
      end record;

   procedure Reset (Connection : in out Connection_Status);

   function Is_Acceptable (Connection : Connection_Status;
                           Sender     : Sock_Addr_Type)
                        return Boolean;
end Input.Connections;
