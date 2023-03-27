with Ada.Text_IO; use Ada.Text_IO;
with Text_IO; 	use Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Packets.Binary.Network; use Packets.Binary.Network;
with Packets.Binary;
with Ada.Streams; use Ada.Streams;
with Ada.Unchecked_Deallocation;
with byte_arrays;	use byte_arrays;
with Network_Utilities;	use Network_Utilities;
with Interfaces;	use Interfaces;

package body Output_Task is

   ------------
   -- Writer --
   ------------

   task body Writer is

      Socket: Socket_Type;-- := Output_Socket.all;
     -- Packet: Network_Packet;
      --Last: Stream_Element_Offset;
      Priority: Boolean := False;
      Close_Task : Boolean := False;

      Inter_Proc_Addr : Sock_Addr_Type := (Family => Family_Inet,
                                           Addr   => Inet_Addr("127.0.0.1"),
                                           Port   => Inter_Proc_Port);


      task Wait_For_Packet_Task is
         entry Start;
         entry Stop;
      end Wait_For_Packet_Task;

      task body Wait_For_Packet_Task is
         Pkt : Network_Packet;
         Continue : Boolean := True;
      begin

         accept Start;

         Main_Loop:
         while Continue loop

            select
               accept Stop do
 --                 Put_Line("Wait_For_Packet_Task: Stop!");
                  Continue := False;
               end Stop;
            else
               null;
            end select;

            select
                  delay 0.1;
            then abort
               --if not Output_Queue.Is_Empty then
                  Output_Queue.Extract(Pkt);
--                  Put_Line("Output Task: Send DATA Packets");
                  Writer.Send_Priority_Packet(Pkt);
                  -- Finalize(Pkt);
               --end if;
            end select;



         end loop Main_Loop;

--         Put_Line("****  Wait_For_Packet: Closed sucessfully  ****");
      exception
         when e: others =>
            Put_Line("Output Task: Wait_For_Packet Task DEAD!!!");
            Put_Line(Exception_Information(e));
      end Wait_For_Packet_Task;


   begin



      declare
         Addr : Sock_Addr_Type := (Family => Family_Inet,
                                   Addr   => Inet_Addr("127.0.0.1"),
                                   Port   => No_Port);
      begin
         Create_Socket(Socket,
                       Mode => Socket_Datagram);
         Bind_Socket(Socket, Addr);
      end;


      Main_Loop:
      while not Close_Task loop
         select

            -- TODO if state = stop ...

            accept Start do


               Wait_For_Packet_Task.Start;

            end Start;

         or
            -- TODO ....
            accept Stop do
               --Close_Socket(Socket);
--               Put_Line("Output Task: Stop received!");
               Wait_For_Packet_Task.Stop;
               Close_Task := True;
            end Stop;

         or

            -- TODO meglio mettere lo stato per evitare che si tenti di mandare
            -- pacchetti con il socket ancora chiuso

            accept Send_Priority_Packet(Data: Network_Packet) do

              -- Packet := Data; -- Packet Variable is used only for debugging

--               Put_Line(Text_IO.Standard_Error,"Packet to: " & image(Data.Peer));



               --***************************************************************
               --*
               --*	Sign the packet
               --*
               --***************************************************************


               --  unisco indirizzo, porta e dati in un unico array
               --  trasmetto questo array in un socket interno.
               --  Il task di input resta in ascolto sia sul socket interno
               --  che su quello esterno. Quando riceve pacchetti dal socket
               --  interno, spedisce il pacchetto.

               if Data.Peer.Addr.Family = Family_Inet then  --IPv4
                  declare

                     IP : Inet_Addr_V4_Buffer;
                     Port : Byte_Array(1..2);
                     Length : Byte_Array_Offset := 1 + 4 + 2 + Data.Buffer'Length;
                     Packet : Byte_Array(1.. Length);
                     Last : Stream_Element_Offset := Packet'Last;
                  begin
                     IP := To_Array(Data.Peer.Addr);
                     Port(1) := byte_arrays.Byte((Unsigned_16(Data.Peer.Port) and 16#FF_00#) / 2**8);
                     Port(2) := byte_arrays.Byte((Unsigned_16(Data.Peer.Port) and 16#00_FF#));

                    -- Put_Line("Port(1): " & Port(1)'img);
                    -- Put_Line("Port(2): " & Port(2)'img);
                     Packet(1) := 4; -- IP version
                     for i in 2 .. 5 loop
                        Packet(Stream_Element_Offset(i)) := byte_arrays.Byte(IP(i-1));
                     end loop;
                     Packet(6..7) := Port;
                     Packet(8..Packet'Length) := Data.Buffer;

--                     Put_Line("Task Output: Data Length: " & Data.Buffer'Length'img);
--                     Put_Line("Task Output: Send a Packet; size: " & Packet'Last'img);
                     Send_Socket(Socket => Socket,
                                 Item   => Stream_Element_Array(Packet),
                                 Last   => Last,
                                 To     => Inter_Proc_Addr);

                  end;
               else    --IPv6
                  declare

                     IP : Inet_Addr_V6_Buffer;
                     Port : Byte_Array(1..2);
                     Length : Byte_Array_Offset := 1 + 16 + 2 + Data.Buffer'Length;
                     Packet : Byte_Array(1.. Length);
                     Last : Stream_Element_Offset := Packet'Last;
                  begin
                     Put_Line("**********  Output Task IPv6  ****************");
                     IP := To_Array(Data.Peer.Addr);
                     Port(1) := byte_arrays.Byte((Unsigned_16(Data.Peer.Port) and 16#FF_00#) / 2**8);
                     Port(2) := byte_arrays.Byte((Unsigned_16(Data.Peer.Port) and 16#00_FF#));

                     Packet(1) := 6; -- IP version
                     for i in 2 .. 17 loop
                        Packet(Stream_Element_Offset(i)) := byte_arrays.Byte(IP(i-1));
                     end loop;
                     Packet(18..19) := Port;
                     Packet(20..Packet'Length) := Data.Buffer;


--                     Put_Line("Task Output: Send a Packet; size: " & Packet'Last'img);
                     Send_Socket(Socket => Socket,
                                 Item   => Stream_Element_Array(Packet),
                                 Last   => Last,
                                 To     => Inter_Proc_Addr);

                  end;
               end if;




            end Send_Priority_Packet;

         end select;


      end loop Main_Loop;

--      Put_Line("****  Output Task: Closed sucessfully  ****");

   exception
      when e: others =>
         Put_Line("Output Task: Dead!!");
         Put_line (Exception_Information(e));
--         New_Line;
 --        Put_Line("Output Task: To: " & Image(Packet.Peer));
  --       Put_Line("Output Task: Size: " & Integer'Image(Packet.Buffer'length));

   end Writer;


   procedure Finalize(X : in out Writer_Task) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Writer, Writer_Task);
   begin
      X.Stop;
      Free (X);
   end Finalize;

end Output_Task;
