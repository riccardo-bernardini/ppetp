--                              -*- Mode: Ada -*-
--  Filename        : tcp_query.ads
--  Description     : Send a query over TCP
--  Author          : Finta Tartaruga
--  Created On      : Mon Feb 25 22:00:57 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!
with GNAT.Sockets;
use  GNAT.Sockets;

package TCP_Query is
   --
   -- Send string Query to the TCP port number Port of the server
   -- whose IP address is contained (in dot-separated format)
   -- in Server. After sending the query, does a shutdown on the
   -- write side and waits for an answer which is returned to
   -- the caller.
   --
   function Do_Query (Server : String;
                      Port   : Port_Type;
                      Query  : String) return String;

   --
   -- Similar to the function above, but the address is contained in
   -- a parameter of type Sock_Addr_Type.
   --
   function Do_Query (Server : Sock_Addr_Type;
                      Query  : String) return String;
end TCP_Query;


