with Query_State;
use  Query_State;

with Text_Io;
use  Text_Io;
procedure Mini_Client is
   C : DB_Connection;
begin
   Connect(Connection => C,
           Port       => 54321);

   Put_Line (Get(C, "pippo"));
   Set (C, "monsu", "volo a vapore");
   Put_Line (Get(C, "aglio"));
end Mini_Client;
