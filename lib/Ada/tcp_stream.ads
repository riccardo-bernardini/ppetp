with GNAT.Sockets;
use  GNAT.Sockets;

package TCP_Stream is
   type Network_Stream is private;

   function Open_Network_Stream (Server : Sock_Addr_Type)
                                return Network_Stream;

   function Open_Network_Stream (Server : String;
                                 Port   : Port_Type)
                                return Network_Stream;

   procedure Close_Network_Stream (Channel : in Network_Stream;
                                   How     : Shutmode_Type := Shut_Write);

   function To_Stream (Net_Stream : Network_Stream)
                      return Stream_Access;

   procedure Put(Channel : in Network_Stream;
                 What    : in String);

   function Get (Channel : in Network_Stream;
                 Length  : in Natural := 0) return String;

private
   type Network_Stream is record
      Socket  : Socket_Type;
      Channel : Stream_Access;
   end record;
end TCP_Stream;
