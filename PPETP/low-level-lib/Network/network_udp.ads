with Network;        use Network;
with Ada.Streams;    use Ada.Streams;
with Common_Types;	use Common_Types;

package network_UDP is
   procedure UDP_Send (Data : Stream_Element_Array;
                       To   : Sock_Addr_Type);

   procedure UDP_Send (Data : Stream_Element_Array;
                       To   : Sock_Addr_Type;
                       From : Port_Type);


   -- Send a UDP packet without create or close a socket
   procedure UDP_Send (Socket: Socket_Type;
                       Data  : Stream_Element_Array;
                       To    : Sock_Addr_Type);


   procedure Search_Port  (Port     :    out Port_Type;
                           Input    :    out Socket_Access;
                           Success  :    out Boolean;
                           Addr     : in     Inet_Addr_Type := Any_Inet_Addr;
                           N_Trials : in     Natural := 5);

   procedure Bind_To_Port (Port    : in     Port_Type;
                           Addr    : in     Inet_Addr_Type := Any_Inet_Addr;
                           Input   :    out Socket_Access;
                           Success :    out Boolean);


   function Find_Free_Port(Addr: in Inet_Addr_Type;
                           Res: in Boolean) return Port_Type;

   function Get_Port(Socket : in     Socket_Access) return Port_Type;

end network_UDP;
