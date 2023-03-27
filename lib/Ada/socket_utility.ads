with GNAT.Sockets;

package Socket_Utility is
   Peer_Left : exception;

   function Read(Socket : GNAT.Sockets.Socket_Type) return String;

   function Get_Host_Name return String;

   type Inet_Addr_Array is array(Natural range <>) of Inet_Addr_Type;
   function Get_My_IP_Addresses return Inet_Addr_Array;
end Socket_Utility;
