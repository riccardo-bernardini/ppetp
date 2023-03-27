--
-- ** What is this? **
-- *******************
--
-- Currently Ada has no standard network interface.  Since I developed this
-- with GNAT, I used the GNAT BSD-like interface to network functions.  In
-- order to gain some independence from GNAT, I wrote this package that exports
-- a slightly higher-level interface to network functions.  This package in
-- turns relies on the lower-level package BSD_Sockets that, in the GNAT case,
-- is just a renaming of GNAT.Sockets.  Should a porting to a system without
-- GNAT.Sockets be necessary, it should suffices to change only the
-- BSD_Sockets package.
--
-- Status: <TESTED> almost full coverage
--
with Ada.Streams;                  use Ada.Streams;
with Ada.Calendar;                 use Ada;
with Ada.IO_Exceptions;

with Splitter_Lib.BSD_Sockets;

package Splitter_Lib.Network is
   Socket_Error : exception renames BSD_Sockets.Socket_Error;
   -- Some renaming from BSD_Sockets to make the interface
   -- of this package as independent as possible from BSD_Sockets

   subtype Inet_Addr_Type is BSD_Sockets.Inet_Addr_Type;

   --------------------------
   -- IP addresses & ports --
   --------------------------

   -- More or less syntactic sugar, but quite convenient.
   Localhost    : constant Inet_Addr_Type :=
     BSD_Sockets.Inet_Addr ("127.0.0.1");

   Any_addr    : constant Inet_Addr_Type :=
                    BSD_Sockets.Inet_Addr ("0.0.0.0");


   -- Return the Which-th IP address of this host.  The 0-th
   -- address is always localhost.  If the Which-th address
   -- does not exist, raise Constraint_Error
   function Self_IP_Address (Which : Natural := 1)
                             return  Inet_Addr_Type;

   -- Return an unused UDP port.  This function does N_Trials random
   -- trials in the range 49_152 .. 61_000 (this range should
   -- be unreserved on any system).  If it does not find an
   -- available port, it raises Port_Unavailable.
   function Free_UDP_Port (N_Trials : Natural := 1024)
                           return Natural;

   Port_Unavailable : exception;

   -----------------
   -- Data Packet --
   -----------------

   -- Type used to represents a packet read from/written to
   -- an UDP socket
   type Packet_Buffer is private;

   subtype Packet_Size is Streams.Stream_Element_Offset range 0 .. 4096;

   -- Get the data carried by a packet buffer
   function Data (X : Packet_Buffer) return Stream_Element_Array;

   -- Get the number of Stream_Element that are in a packet
   function Size (X : Packet_Buffer) return Packet_Size;

   -- Return true if packet is empty
   function Is_Empty (Packet : Packet_Buffer)
                      return Boolean;

   -- Convert a Stream_Element_Array to a packet (e.g., to
   -- use it in Send)
   function To_Packet (X : Stream_Element_Array)
                       return Packet_Buffer;

   -- Make a "string" representation of a packet (useful for
   -- debugging)
   function Image (X : Packet_Buffer) return String;

   -----------------------
   -- Input UDP Sockets --
   -----------------------

   -- Type that represent and UDP socket open in input
   type Input_UDP_Socket  is private;

   type UDP_Socket_List(<>) is private;

   -- Open an input UDP socket to be and bind it to port Port of
   -- this host (Host must be an address of this host)
   function UDP_Input (Host : Inet_Addr_Type;
                       Port : Natural)
                       return Input_UDP_Socket;

   function UDP_List return UDP_Socket_List;

   procedure Append (List : in out UDP_Socket_List;
                     Host : in     Inet_Addr_Type;
                     Port : in     Natural;
                     Src  :    out Natural);

   -- Try to read from socket From.  If no data is read before Timeout,
   -- return an empty buffer
   procedure Read_With_Timeout (From    : in out Input_UDP_Socket;
                                To      :    out Packet_Buffer;
                                Timeout : in     Calendar.Time);

   procedure Read_With_Timeout (From    : in out UDP_Socket_List;
                                To      :    out Packet_Buffer;
                                Source  :    out Natural;
                                Timeout : in     Calendar.Time);

   No_Source : constant Natural;


   -- Close an UDP input socket
   procedure Close (S : in out Input_UDP_Socket);

   procedure Close (S : in out UDP_Socket_List);


   ------------------------
   -- Output UDP Sockets --
   ------------------------

   type Output_UDP_Socket is private;


   -- Open an UDP socket to write to (Host, Port) via procedure
   -- Send
   function UDP_Output (Host : Inet_Addr_Type;
                        Port : Natural)
                        return Output_UDP_Socket;

   -- Send data over a UDP socket
   procedure Send (What : Packet_Buffer;
                   Dst  : Output_UDP_Socket);

   -- Close an UDP output socket
   procedure Close (S : Output_UDP_Socket);

   -----------------
   -- TCP streams --
   -----------------

   type TCP_Stream is private;


   -- Open a TCP connection toward the Host
   function New_TCP_Stream (Host : Inet_Addr_Type;
                            Port : Natural)
                            return TCP_Stream;


   -- Send a CRLF-terminated line over a TCP connection
   procedure Send_Line (To   : TCP_Stream;
                        Line : String);

   -- Read a CRLF-terminated line from a TCP connection
   function Get_Line (From : TCP_Stream)
                      return String;

   -- Read a character from a TCP connection
   function Get_Char (From : TCP_Stream)
                      return Character;

private
   type Input_UDP_Socket  is
      record
         Socket   : BSD_Sockets.Socket_Type;
         Selector : BSD_Sockets.Selector_Access;
      end record;

   type Socket_Array is array (1 .. 128) of BSD_Sockets.Socket_Type;

   type UDP_Socket_List is
      record
         Sockets    : Socket_Array;
         First_Free : Natural;
         Selector   : BSD_Sockets.Selector_Access;
      end record;

   No_Source    : constant Natural := Socket_Array'First - 1;

   type Output_UDP_Socket is new BSD_Sockets.Socket_Type;
   type TCP_Stream        is new BSD_Sockets.Socket_Type;

   type Packet_Buffer is
      record
         Data : Streams.Stream_Element_Array (Packet_Size);
         Size : Packet_Size;
      end record;

end  Splitter_Lib.Network;
