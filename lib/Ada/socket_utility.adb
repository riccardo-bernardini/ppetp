with GNAT.Sockets;
use  GNAT.Sockets;

with Interfaces.C.Pointers;
with Interfaces.C.Strings;
use  Interfaces.C;

with Ada.Streams;
use  Ada.Streams;

with Conversions;
use  Conversions;

package body Socket_Utility is
   function Read(Socket : GNAT.Sockets.Socket_Type) return String is
      Buffer      : Stream_Element_Array(1..1024);
      Data_Length : Stream_Element_Offset;
   begin
      Receive_Socket(Socket => Socket,
                     Item   => Buffer,
                     Last   => Data_Length);

      if (Data_Length = Buffer'First-1) then
         raise Peer_Left;
      end if;

      return To_String(Buffer(Buffer'First..Buffer'First+Data_Length-1));
   end Read;

   function Get_Host_Name return String is
      use type C.int;
      use type C.Char_Array;

      function Syscall_Gethostname(Name : C.Char_Array;
                                   Len  : C.Size_t) return C.Int;
      pragma Import(C, Syscall_Gethostname, "gethostname");

      Buflen : constant C.Size_T := 1024;
      Buffer : C.Char_Array(1..1024);
      Failure : constant C.int := -1;
      Ok : C.Int;
   begin
      Ok := Syscall_Gethostname(Name => Buffer,
                                Len  => Buffer'Length);

      if (Ok = Failure) then
         raise Constraint_Error;
      end if;

      return To_Ada(Buffer);
   end Get_Host_Name;

   function Get_My_IP_Addresses return Inet_Addr_Array is
      Host    : Host_Entry_Type := Get_Host_By_Name(Get_Host_Name);
      Result  : Inet_Addr_Array(0..Addresses_Length(Host));
   begin
      Result(0) := Inet_Addr("127.0.0.1");
      for I in 1..Result'Last loop
         Result(I) := Addresses(Host, I);
      end loop;

      return Result;
   end Get_My_IP_Addresses;
end Socket_Utility;
