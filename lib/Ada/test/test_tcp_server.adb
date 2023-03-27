with GNAT.Sockets;  use GNAT.Sockets;
with TCP_Server;    use TCP_Server;
with Text_Io;       use Text_Io;
with TCP_Callback;

procedure Test_TCP_Server is
   Listener : Server_Listener;
   Port : Port_Type := 54321;
begin
   Listener.Initialize(Port     => Port,
                       Callback => TCP_Callback'Access);
   delay 10.0;
   Listener.Finish;
end Test_TCP_Server;


