--                              -*- Mode: Ada -*-
--  Filename        : tcp_server.ads
--  Description     : Wrapper for TCP servers
--  Author          : Riccardo Bernardini
--  Created On      : Fri Feb 15 11:17:20 2008
--  Last Modified By: R. Bernardini
--  Last Modified On: March 12, 2008
--  Update Count    : 0
--  Status          : Basic test OK (Mar 12 2008).

--
-- This package provides a task type Server_Listener.  After activation,
-- a Server_Listener does an accept on entry Initialize which accepts a
-- port number and a callback function to be called when a new connection
-- is estabilished.  If the given port is busy, the task dies with an
-- exception, unless the third parameter is False.  In the latter case,
-- the port number is increased until a free port is found.  The actual
-- port number used is returned in Port.
--
-- When a new connection is established, the task calls the callback
-- function specified with the Initialize call.  The callback receives the
-- newly created socket and the address of the peer (as with the
-- Accept_Socket function)
--
with GNAT.Sockets;
use  GNAT.Sockets;

package Tcp_Server is
   type Callback_Type is
     access procedure (Socket       : Socket_Type;
                       Peer_Address : Sock_Addr_Type);

   task type Server_Listener is
      entry Initialize (Port       : in out Port_Type;
                        Callback   : in     Callback_Type;
                        Port_Fixed : in     Boolean       := True);
      entry Finish;
   end Server_Listener;
end Tcp_Server;
