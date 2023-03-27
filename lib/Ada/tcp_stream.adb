with GNAT.Sockets;
use  GNAT.Sockets;

with Ada.Streams;
use  Ada.Streams;

with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

package body TCP_Stream is
   function Open_Network_Stream (Server : Sock_Addr_Type)
                                  return Network_Stream is
      Result : Network_Stream;
      Addr   : Sock_Addr_Type := Server;
   begin
      Create_Socket (Result.Socket);
      Connect_Socket (Result.Socket, Addr);
      Result.Channel := Stream(Result.Socket);

      return Result;
   end Open_Network_Stream;

   function Open_Network_Stream (Server : String;
                                 Port   : Port_Type)
                                return Network_Stream is
      Addr   : Sock_Addr_Type := (Family => Family_Inet,
                                  Addr   => Inet_Addr(Server),
                                  Port   => Port);

   begin
      return Open_Network_Stream(Addr);
   end Open_Network_Stream;


   procedure Close_Network_Stream (Channel : in Network_Stream;
                                   How     : Shutmode_Type := Shut_Write) is
   begin
      Shutdown_Socket(Channel.Socket, How);
   end Close_Network_Stream;

   function To_Stream (Net_Stream : Network_Stream)
                      return Stream_Access is
   begin
      return Net_Stream.Channel;
   end To_Stream;

   function To_Stream_Buf (X : String)
                          return Stream_Element_Array is
      Y : Stream_Element_Array(Stream_Element_Offset(X'First)..
                               Stream_Element_Offset(X'Last));
   begin
      for I in X'Range loop
         Y(Stream_Element_Offset(I)) := Stream_Element(Character'Pos(X(I)));
      end loop;
      return Y;
   end To_Stream_Buf;

   function To_String (X : Stream_Element_Array)
                      return String is
      Y : String(Integer(X'First)..Integer(X'Last));
   begin
      for I in X'Range loop
         Y(Integer(I)) := Character'Val(X(I));
      end loop;

      return Y;
   end To_String;


   procedure Put(Channel : in Network_Stream;
                 What    : in String) is
      Last   : Stream_Element_Offset;
   begin

      Send_Socket(Socket => Channel.Socket,
                  Item   => To_Stream_Buf(What),
                  Last   => Last);
   end Put;

   function Slurp_All (Channel : in Network_Stream)
                      return String is
      Result : Unbounded_String := Null_Unbounded_String;
      Buffer : Stream_Element_Array(1..1024);
      Last   : Stream_Element_Offset;
   begin
      loop
         Receive_Socket (Socket => Channel.Socket,
                         Item   => Buffer,
                         Last   => Last);

         exit when (Last=Buffer'First-1);

         Result := Result & To_String(Buffer(1..Last));
      end loop;

      return To_String(Result);
   end Slurp_All;

   function Get (Channel : in Network_Stream;
                 Length  : in Natural := 0)
                return String is
   begin
      if (Length = 0) then
         return Slurp_All (Channel);
      else
         declare
            Buffer : Stream_Element_Array(1..Stream_Element_Offset(Length));
            Last   : Stream_Element_Offset;

            First  : Stream_Element_Offset := 1;
         begin
            loop
               Receive_Socket (Socket => Channel.Socket,
                               Item   => Buffer(First..Buffer'Last),
                               Last   => Last);

               exit when (Last=First-1) or (Last=Buffer'Last);
               First := Last+1;
            end loop;

            return To_String(Buffer(Buffer'First..Last));
         end;
      end if;
   end Get;
begin
   GNAT.Sockets.Initialize;
end TCP_Stream;
