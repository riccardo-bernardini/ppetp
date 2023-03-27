
package body udp_rx_tx is

   procedure udp_send(To: in Sock_Addr_Type;
                      Item: in Stream_Element_array;
                      From: in Sock_Addr_Type) is

      Sender: Sock_Addr_Type := From;
      Dest: Sock_Addr_Type := To;
      Msg: Stream_Element_Array := Item;
      Last: Stream_Element_Offset;
      Socket: Socket_Type;
   begin

      Initialize; -- initialize socket

      -- Create and Bind the socket
      Create_Socket(Socket, Family_Inet, Socket_Datagram);

      Set_Socket_Option (Socket, Socket_Level, (Reuse_Address, True));

      Bind_Socket (Socket, Sender);

      -- Send the packet
      Send_Socket(Socket,Msg,Last,Dest);

      Close_Socket(Socket);

   end udp_send;



   procedure udp_receive(Server_Addr: in Sock_Addr_Type;
                         Item: out Stream_Element_Array;
                         Last: out Stream_Element_Offset;
                         From: out Sock_Addr_Type) is

      Server: Socket_Type;
   begin

      Initialize; -- initialize socket

      -- Create and Bind the socket
      Create_Socket(Server, Family_Inet, Socket_Datagram);
      Set_Socket_Option (Server, Socket_Level, (Reuse_Address, True));
      Bind_Socket (Server, Server_Addr);

      -- Wait for incoming packets
      Receive_Socket(Server,Item,Last,From);

      Close_Socket(Server);

   end udp_receive;



   function Str_To_Byte(Input  : in Unbounded_String) return Stream_Element_Array is

      Output : Stream_Element_Array(Stream_Element_Offset(To_String(Input)'First)..
                                      Stream_Element_Offset(To_String(Input)'Last));

   begin

      for Count in To_String(Input)'Range loop
         Output(Stream_Element_Offset(Count)) := Character'Pos(To_String(Input)(Count));
      end loop;

      return Output;

   end Str_To_Byte;


   function Byte_To_Str(Input : in Stream_Element_Array) return Unbounded_String is

      Output : String(Integer(Stream_Element_Offset(Input'first))..
                        Integer(Stream_Element_Offset(Input'Last)));

   begin
      for Count in Input'Range loop
         Output(Integer(Count)) := Character'Val(Input(Stream_Element_Offset(Count)));
      end loop;

      return To_Unbounded_String(Output);

   end Byte_To_Str;


   -- Send a string on a UDP packet
   procedure Send_String(To: in Sock_Addr_Type;
                         Msg: in Unbounded_String;
                         From: in Sock_Addr_Type) is
      Msg_Arr: Stream_Element_Array := Str_To_Byte(msg);
   begin
      udp_send(To, Msg_Arr, From);
   end  Send_String;


   -- Receive string from an UDP packet
   procedure Receive_String(Server: in Sock_Addr_Type;
                            Msg: out Unbounded_String;
                            From: out Sock_Addr_Type) is

      Item: Stream_Element_Array(Stream_Element_Offset(1) ..
                                 Stream_Element_Offset(buffer_size));
      Last: Stream_Element_Offset;
   begin

      udp_receive(Server, Item, Last, From);
      Msg := Byte_To_Str(Item(Item'First..Last));

   end Receive_String;



end udp_rx_tx;

