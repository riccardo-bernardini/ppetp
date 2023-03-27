with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Input_Data;
use  Input_Data;

with GNAT.Sockets;
use  GNAT.Sockets;

with Ada.Streams;
use  Ada.Streams;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Conversions;
use  Conversions;

package body Command_Reader is
   task body Reader is
      type Command_Socket is record
         Initialized : Boolean := False;
         Socket      : Socket_Type;
      end record;


      procedure Init_Udp_Socket(Socket  : in out Socket_Type;
                                Port    : in     Port_Type) is
      begin
         Create_Socket (Socket => Socket,
                        Mode   => Socket_Datagram);

         Bind_Socket (Socket  => Socket,
                      Address => (Family => Family_Inet,
                                  Addr   => Any_Inet_Address,
                                  Port   => Port));
      end Init_Udp_Socket;

      procedure Read_Command (Socket  : in out Socket_Type;
                              Command :    out Unbounded_String) is

         Buffer : Stream_Element_Array(1..1024);
      begin
         if (not Socket.Initialized) then
            Socket.Initialized := True;
         end if;

         loop
            Receive_Socket (Socket => Socket,
                            Item   => Buffer,
                            Last   => N_Elements);
            exit when Last >= Buffer'First;
         end loop;

         Command := To_Unbounded_String(Buffer(Buffer'First..Last));
      end Read_Command;

      Socket  : Socket_Type;
      My_Port : Port_Type;
      Command : Unbounded_String;
   begin
      accept Listen_To (Port : Port_Type) do
         My_Port := Port;
      end Listen_To;

      Init_UDP_Socket(Socket, My_Port);

      declare
         Packet_Pt : access Command_Packet;
      begin
         loop
            Read_Command (Command);

            Packet_Pt := new Command_Packet'(Command => Command);

            Command_Queue.Insert(Packet_Pt);
         end loop;
      end;
   end Reader;
end Command_Reader;
