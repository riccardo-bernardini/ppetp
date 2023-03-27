with Text_Io;              use Text_Io;
with Test_Report;          use Test_Report;
with Packets.Binary.Application;  use Packets.Binary.Application;
with Packets.Binary.Network;      use Packets.Binary.Network;
--with Ada.Streams;                 use Ada.Streams;
with Byte_Arrays;                 use Byte_Arrays;
with PPETP;                       use PPETP;
with Network;                     use Network;

procedure Binary_Test is
   Addr_1 : Network.Sock_Addr_Type := (Family => Family_Inet,
                                       Addr   => Inet_Addr ("192.115.28.1"),
                                       Port   => 32145);

   Addr_2 : Network.Sock_Addr_Type := (Family => Family_Inet,
                                       Addr   => Inet_Addr ("197.0.0.1"),
                                       Port   => 11123);

   Buf_1 : Byte_Array (1 .. 5) := (others => 4);
   Buf_2 : Byte_Array (1 .. 5) := (others => 3);
   Buf_3 : Byte_Array (1 .. 7) := (others => 2);

   Reporter : Reporter_Type;

   P1, P1a, P2, P3 : Application_Packet;
   N1, N2 : Network_Packet;

   use type Byte_Arrays.Byte;
begin
   for I in Buf_1'Range loop
      Buf_1 (I) := Byte (I);
   end loop;

   P1 := New_Packet (Timestamp => 3321,
                     Data      => Buf_1);

   P1a := New_Packet (Timestamp => 3321,
                      Data      => Buf_2);

   P2 := New_Packet (Timestamp => 33,
                     Data      => Buf_2);

   P3 := New_Packet (Timestamp => 1212,
                     Data      => Buf_3);

   Reporter.New_Suite (Name => "application_packet mixed stuff");

   Reporter.New_Result (P1.Timestamp = 3321);
   Reporter.New_Result (P1.buffer  = Buf_1);
   Reporter.New_Result (P1 = P1);
   Reporter.New_Result (P1 /= P2);
   Reporter.New_Result (P1 /= P3);

   Reporter.New_Suite (Name => "application_packet get/set");

   for I in Buf_1'Range loop
      P1.Set (Index => I, Data => Byte (I) * 2);
   end loop;

   for I in Buf_1'Range loop
      Reporter.New_Result(P1.Get (I) = Byte (I) * 2);
   end loop;

   Reporter.New_Suite ("network_packet mixed stuff");

   N1 := New_Packet (Data => Buf_1, Remote_Addr => Addr_1);
   N2 := New_Packet (Data => Buf_2, Remote_Addr => Addr_2);



   Reporter.New_Result (N1.Buffer = Buf_1);
--   Reporter.New_Result (N1.Peer   = Addr_1);

   N1.Set_Peer (Addr_2);

 --  Reporter.New_Result (N1.Peer   = Addr_2);
   Reporter.Final;
end Binary_Test;
