--                              -*- Mode: Ada -*-
--  Filename        : client.ads
--  Description     : Client per il NAT traversing
--  Author          : Roberto Cesco Fabbro
--  Created On      :
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Under heavy development


--

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces; use Interfaces;

with Gnat.Sockets; use Gnat.Sockets;
with Ada.Streams; use Ada.Streams;
with udp_rx_tx; use udp_rx_tx;
with Commands; use Commands;

with Peer_Info; use Peer_Info;
with Random_Generator; use Random_Generator;
with Tokenize; use Tokenize;
with Server_DB; use Server_DB;
with Ada.Command_Line; use Ada.Command_Line;


with PPETP.Sessions.Initializer; use PPETP.Sessions.Initializer;
with Configuration.Xml; use Configuration.Xml;
with Configuration; use Configuration;


procedure Client is
   Server_Addr: Sock_Addr_Type;
   Client_Addr: Sock_Addr_Type;

   From: Sock_Addr_Type;

   Message: Unbounded_String;
   Msg_List: Token_List;
   peer_id : Peer_Identifier;
   --peer_list_count: Integer;

   peer_list: Peer_DB;
   File : File_Type;

   Session_init: Initializer;
   Config: Configuration.Config_Data(1..1);

begin

   Initialize;
   if ( Argument_Count = 0 ) then
      Put_Line("Use: client.exe client_IP server_IP");
   end if;


   -- se c'è solo un parametro carica la configurazione da file
   if ( Argument_Count = 2 ) then


      Server_Addr.Addr := Inet_Addr(Argument(2));
      --Server_Addr.Addr :=Inet_Addr("127.0.0.1");
      Server_Addr.Port := Port_Type(8080);


      Client_Addr.Addr := Inet_Addr(Argument(1));
      --Client_Addr.Addr := Inet_Addr("127.0.0.1");
      Client_Addr.Port := Port_type(1100 + Random_Nat(5000));

      New_Line;
      Put_Line("Client @ " & Image(Client_Addr));
      -- send play msg

      Put_Line("Client: Send PLAY to the server");
      Send_Command(Server_Addr, PLAY, NO_PARAMETERS, Client_Addr);

      --wait for server msg
      Put_Line("Client: Wait for server response");
      From := No_Sock_Addr;

      Wait_For_Command(Client_Addr, UNKNOWN, Message, From);

      Put_Line("Client: Recevived message from: " & Image(From));
      Put_Line("Client: Message: " & To_String(Message));

      -- controlla quale messaggio è arrivato
      Msg_List := Split(To_String(Message));

      -- peer id
      peer_id := Peer_Identifier( Unsigned_32'Value(To_String(Element(Msg_List,1))));




      -- send HELO ID to the server
      declare
         Msg : Unbounded_String := "HELO " & Image(peer_id);
      begin
         Put_Line("Client: Send: " & To_String(Msg));
         delay 0.2; -- for sync with server
         Send_Command(Server_Addr, HELO, Image(peer_id), Client_Addr);
      end;

      -- waiting for ack
      Put_Line("Client: Waiting for ACK");
      Message := NO_PARAMETERS;
      From := No_Sock_Addr;
      Wait_For_Command(Client_Addr, ACK, Message, From);
      Put_Line("Client: ACK Received");

      -- waiting config file
      Put_Line("Client: Waiting for ACK");
      Message := NO_PARAMETERS;
      From := No_Sock_Addr;
      Wait_For_Command(Client_Addr, UNKNOWN, Message, From);
      Put_Line("Client: Configuration received");

      Put_line("Client: write to file");
      Create(File, Out_File, "config_client.txt");
      for i in integer range 1.. length(Message) loop
         put(File, Element(Message, i));
      end loop;

      Close(file);


   else
      Open(File, In_File, "./server_config.xml");

      Put_Line("Client: Load configuration from file");
      while (not End_Of_File(File)) loop

         declare
            c: character;
         begin
            Get(File, c);
            Message := Message & c;
         end;

      end loop;
   Close(File);

   end if;

   Put_line("Client: Parsing XML");
   Config := Configuration.Xml.Parse(To_String(Message));

   Put_Line("Client: Session init");
   Session_init := Init(Config);

   Put_Line("Client: Session initializated");

   declare
      Listen_Addr: Sock_Addr_Type;
      F: Sock_Addr_Type;
      M: Unbounded_String;
      T: Unbounded_String;
      To: Sock_Addr_Type;
      Ad: Sock_Addr_Type;
      msg_list_1: Token_List;
   begin

      Listen_Addr.Addr := Any_Inet_Addr;
      Listen_Addr.Port := Port_Type(6661);

      loop
         if (Argument_Count /= 2) then

            Put_Line("Client: Waiting on port 6661");
            Put_Line("Client: Started");
            Receive_String(Server => Listen_Addr,
                           Msg    => M,
                           From   => F);



            Msg_List := Split(To_String(M));
            T := Element(Msg_List, 1);

            msg_list_1 := Split(To_String(T),':');
            To.Addr := Inet_Addr(To_String(Element(Msg_List_1, 1)));
            To.Port := Port_Type'Value(To_String(Element(Msg_List_1,2 )));


            T := Element(Msg_List, 2);

            msg_list_1 := Split(To_String(T),':');
            Ad.Addr := Inet_Addr(To_String(Element(Msg_List_1, 1)));
            Ad.Port := Port_Type'Value(To_String(Element(Msg_List_1,2 )));

            Put_Line("Client: Send a Punch to : " & Image(To) &
                     " with parameter who :" & Image(Ad));

            Send_Punch(Session_Init => Session_init,
                       Address      => Ad,
                       To           => To);
         else
            Put_Line("Client: Started");
            delay 5.0;
            -- verify the hole on the NAT
--            Put_Line("Client on " & Image(Client_Addr.Addr));
            if Client_Addr.Addr = Inet_Addr("192.168.0.1") then
               declare
                  Addr   : Inet_Addr_Type := Session_Init.Config(1).Inputs(1).Addr;
                  Port   : Port_Type := Config(1).Inputs(1).Port;

                  Sock_Addr : Sock_Addr_Type;
               begin

                  Sock_Addr.Addr := Addr;
                  Sock_Addr.Port := Port;
  --                Put_Line("Client: Ping to: " & Image(Sock_Addr));

                  if Ping_Peer(Session_Init => Session_init,
                               To           => Sock_Addr ) then
                     Put_Line("NAT Traversed: OK");
                  else
                     Put_Line("NAT Traversed: FAIL");
                  end if;

               end;

            end if;

         end if;

      end loop;


   end;

end Client;
