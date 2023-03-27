   with Ada.Text_Io;	use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;	use Ada.Exceptions;

with GNAT;    use GNAT;
with Common_Types; use Common_Types;



package body network_UDP is

   --------------
   -- UDP_Send --
   --------------

   procedure UDP_Send
     (Data : Stream_Element_Array;
      To   : Sock_Addr_Type)
   is
      Sock : Socket_Type;
      Last : Stream_Element_Offset;
   begin
      Create_Socket (Socket => Sock,
                     Family => Family_Inet,
                     Mode   => Socket_Datagram);

      Send_Socket (Socket => Sock,
                   Item   => Data,
                   Last   => Last,
                   To     => To);
      Close_Socket(Sock);
   end UDP_Send;


   procedure UDP_Send (Data : Stream_Element_Array;
                       To   : Sock_Addr_Type;
                       From : Port_Type) is
      Sock : Socket_Type;
      Last : Stream_Element_Offset;
      Addr : Sock_Addr_Type;
   begin
      Create_Socket (Socket => Sock,
                     Family => Family_Inet,
                     Mode   => Socket_Datagram);

      Addr.Addr := Any_Inet_Addr;
      Addr.Port := From;

      Bind_Socket(Socket  => Sock,
                  Address => Addr);

      Send_Socket (Socket => Sock,
                   Item   => Data,
                   Last   => Last,
                   To     => To);
      Close_Socket(Sock);
   end UDP_Send;


   procedure UDP_Send (Socket: Socket_Type;
                       Data  : Stream_Element_Array;
                       To    : Sock_Addr_Type) is
      Last : Stream_Element_Offset;
   begin
      Send_Socket (Socket => Socket,
                   Item   => Data,
                   Last   => Last,
                   To     => To);
   end UDP_Send;







   procedure Free is
     new Ada.Unchecked_Deallocation (Socket_Type,
                                     Socket_Access);



   -- Allocate a new socket and bind it to port Port.  If
   -- successful, set Success to True; otherwise, set
   -- Success to False and destroy the created socket.
   procedure Make_And_Bind (Socket  :    out Socket_Access;
                            Addr    : in     Inet_Addr_Type;
                            Port    : in     Port_Type;
                            Success :    out Boolean) is
   begin
      Success := True;
      Socket  := new Socket_Type;
      Create_Socket (Socket.all, Mode => Socket_Datagram);

      Put_Line("Make and Bind: " & Image(Addr) & ":" & Port'img);
      begin
         Bind_Socket (Socket.all, (Family => Family_Inet,
                                   Addr   => Addr,
                                   Port   => Port));

         --Text_Io.Put_Line (Text_Io.Standard_Error,
         --                  "Bound to UDP " & Sockets.Port_Type'Image(Port));
      exception
         when e: Socket_Error =>
            Put_Line("Make and Bind: Error");
            Put_Line(Exception_Information(e));
            Success := False;
            Close_Socket (Socket.all);
            Free (Socket);
      end;
   end Make_And_Bind;

   -- Create two sockets (Input and Output) and try to bind them
   -- to the UDP port associated with the PPETP port Port.  Return
   -- Sucess=True if succesfull, otherwise destroy the two sockets
   -- and set Success=False
   procedure Bind_To_Port (Port    : in     Port_Type;
                           Addr    : in     Inet_Addr_Type := Any_Inet_Addr;
                           Input   :    out Socket_Access;
                           Success :    out Boolean) is

   begin
      Make_And_Bind (Socket  => Input,
                     Success => Success,
                     Addr    => Addr,
                     Port    => Port);

      if (not Success) then
         Close_Socket (Input.all);
         Free (Input);
      end if;

   end Bind_To_Port;




   -- Search for a valid pair of UDP ports to be used as a
   -- PPETP port.  Do N_Trials trials. Set Success=True if
   -- succesfull.
   procedure Search_Port  (Port     :    out Port_Type;
                           Input    :    out Socket_Access;
                           Success  :    out Boolean;
                           Addr     : in     Inet_Addr_Type := Any_Inet_Addr;
                           N_Trials : in     Natural := 5) is

      type Port_Range is new Natural range 1025 .. 2**16-1;

      package Random_Ports is
        new Ada.Numerics.Discrete_Random (Port_Range);

      Ok         : Boolean;
      Gen        : Random_Ports.Generator;
      This_Trial : Natural;
   begin
      Random_Ports.Reset (Gen);

      This_Trial := 1;
      while This_Trial <= N_Trials loop
         Port := Port_Type(Random_Ports.Random(Gen));

         Bind_To_Port (Port    => Port,
                       Addr    => Addr,
                       Input   => Input,
                       Success => Ok);
         if (Ok) then
            Success := True;
            return;
         end if;

         This_Trial := This_Trial + 1;
      end loop;

      Success := False;
   end Search_Port;


   -- Find a Free port to bind to the address Addr
   function Find_Free_Port(Addr: in Inet_Addr_Type;
                           Res: in Boolean) return Port_Type is
      type Port_Range is new Natural range 1025 .. 2**16-1;

      package Random_Ports is
        new Ada.Numerics.Discrete_Random (Port_Range);

      Gen: Random_Ports.Generator;

      Socket : Socket_Type;
      Port: Port_Type;
   begin
      if Res then
         Random_Ports.Reset (Gen);
      end if;

      while True loop


         Create_Socket (Socket, Mode => Socket_Datagram);

         Port := Port_Type(Random_Ports.Random(Gen));

         begin
            Bind_Socket (Socket, (Family => Family_Inet,
                                  Addr   => Addr,
                                  Port   => Port));

            Close_Socket (Socket);

            return Port;

         exception
            when Socket_Error =>
               Close_Socket (Socket);
         end;

      end loop;

      -- Never reach
      return No_Port;

   end Find_Free_Port;



   -- Return the UDP port from the socket
   function Get_Port(Socket : in     Socket_Access) return Port_Type is
       tmp: Sock_Addr_Type;
   begin
       tmp := Get_Socket_Name (Socket.all);
       return tmp.port;
   end Get_Port;









end network_UDP;
