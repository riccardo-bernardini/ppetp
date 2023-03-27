package body Input.Connections is

   procedure Reset (Connection : in out Connection_Status) is
   begin
      Connection.Reply_Port  := No_Port;
      Connection.Good_Source := No_Inet_Addr;
      -- Reset(Connection.Parser);
   end Reset;

   function Is_Acceptable (Connection : Connection_Status;
                           Sender     : Sock_Addr_Type)
                        return Boolean is
   begin
      return (Sender.Addr = Connection.Good_Source or
                Connection.Good_Source = Network.Any_Inet_Addr);
   end Is_Acceptable;
end Input.Connections;
