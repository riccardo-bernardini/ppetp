with GNAT.Sockets;  use GNAT.Sockets;
with Text_Io;       use Text_Io;

procedure TCP_Callback (Socket       : Socket_Type;
                        Peer_Address : Sock_Addr_Type) is
begin
   Put_Line("Connessione ricevuta");
   Shutdown_Socket(Socket);
end TCP_Callback;
