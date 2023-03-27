with Network;             use Network;
with Network_UDP;         use Network_UDP;
with Test_Report;         use Test_Report;
with Text_Io;             use Text_Io;
with Ada.Streams;         use Ada.Streams;

procedure Test_Udp is
   Data : Stream_Element_Array (1..5) := (1, 2, 3, 5, 8);
   Dst  : Sock_Addr_Type := (Family => Family_Inet,
                             Addr   => Inet_Addr ("127.0.0.1"),
                             Port   => 54321);
begin
   Udp_Send(Data, Dst);
end Test_Udp;
