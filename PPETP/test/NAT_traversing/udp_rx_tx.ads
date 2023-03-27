--                              -*- Mode: Ada -*-
--  Filename        : udp_rx_tx.ads
--  Description     : UDP packets Sender and Receiver
--  Author          : Roberto Cesco Fabbro
--  Created On      : Gen, 27 2009
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : It seems to works

-- This package provide funtions to send and receive data on UDP packets

with GNAT.Sockets; use GNAT.Sockets;
with Ada.Streams; use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package udp_rx_tx is

   -- *** TODO ***  Maybe is better to port this value to the max of UDP packet
   --               size (64KB). In this way no overflow occours.
   -- Size of the buffer for the received packet
   buffer_size: constant Positive := 2048;

   -- This procedure permits to send data on UDP packets
   --	To:	address of the recipient of the packet
   --	Item:	bytes of data to transmit
   --	From:	sender's address
   procedure udp_send(To: in Sock_Addr_Type;
                      Item: in Stream_Element_array;
                      From: in Sock_Addr_Type);

   -- This procedure permits to receive data from UDP packets
   --	Server_Addr:	address where listen for packets
   --	Item:		bytes of recieved data
   --	Last:		Number of recieved bytes
   --	From:		Sender's address
   procedure udp_receive(Server_Addr: in Sock_Addr_Type;
                         Item: out Stream_Element_Array;
                         Last: out Stream_Element_Offset;
                         From: out Sock_Addr_Type);

   -- This procedure permits to send a String over an UDP packet
   --	To:	Address of the recipient of the packet
   --	Msg:	Message to send
   --	From:	Address of the sende
   procedure Send_String(To: in Sock_Addr_Type;
                         Msg: in Unbounded_String;
                         From: in Sock_Addr_Type);

   -- This procedure permits to receive a String from an UDP packet
   --	Server:	Address where listen for packets
   --	Msg:	Received String
   --	From:	Address of the sender
   procedure Receive_String(Server: in Sock_Addr_Type;
                            Msg: out Unbounded_String;
                            From: out Sock_Addr_Type);


   -- This function is used to convert a string into a Stream_Element_Array
   --	Input:	String to convert
   function Str_To_Byte(Input  : in Unbounded_String) return Stream_Element_Array;


   -- This function is used to convert a Stream_Element_Array into a String
   --	Input:	Array to convert
   function Byte_To_Str(Input : in Stream_Element_Array) return Unbounded_String;

end udp_rx_tx;
