with GNAT.Sockets;
use  GNAT.Sockets;

with TCP_Stream;
use  TCP_Stream;

package body TCP_Query is
   function Do_Query (Server : Sock_Addr_Type;
                      Query  : String) return String is
      Channel : Network_Stream := Open_Network_Stream(Server);
   begin
      Put(Channel, Query);
      Close_Network_Stream(Channel, Shut_Write);

      return Get(Channel);
   end Do_Query;

   function Do_Query (Server : String;
                      Port   : Port_Type;
                      Query  : String) return String is
      Addr   : Sock_Addr_Type := (Family => Family_Inet,
                                  Addr   => Inet_Addr(Server),
                                  Port   => Port);
   begin
      return Do_Query(Addr, Query);
   end Do_Query;

end TCP_Query;


