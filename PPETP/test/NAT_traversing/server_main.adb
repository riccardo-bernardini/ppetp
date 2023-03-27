--                              -*- Mode: Ada -*-
--  Filename        : server_main.ads
--  Description     : Server per il NAT traversing
--  Author          : Roberto Cesco Fabbro
--  Created On      :
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Under heavy development




with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces; use Interfaces;
with Network; use Network;
with Ada.Streams; use Ada.Streams;

with udp_rx_tx; use udp_rx_tx;
with Commands; use Commands;

with Peer_Info; use Peer_Info;
with Server_DB; use Server_DB;
with Random_Generator; use Random_Generator;
with Tokenize; use Tokenize;

with Packets; use Packets;
with Sent_DB; use Sent_DB;
with network_UDP; use network_UDP;
with Packets.Protocol.Command; use Packets.Protocol.Command;
with Packets.Binary.Network; use Packets.Binary.Network;
with Packets.Protocol.Building; use Packets.Protocol.Building;
with Auth.Credentials; use Auth.Credentials;
with Auth.Profiles; use Auth.Profiles;
with byte_arrays; use byte_arrays;

procedure Server_Main is
   Server_Addr: Sock_Addr_Type;
   From: Sock_Addr_Type;

   Message: Unbounded_String;
   Msg_List: Token_List;

   Database: Peer_DB;
   Returned_Peer_Number: constant Integer := 2; -- Number of returned peer in the PLAY response

   -----------------
   -- Random_Port --
   -----------------

   -- Create a random Session Port
   function Random_Port return Positive is
   begin
      return Positive (1100 + Random_Nat(10_000));
   end Random_Port;

   ---------------------
   -- Send_XML_Config --
   ---------------------

   procedure Send_XML_Config(Client  : in Sock_Addr_Type;
                             XML_Text: in Unbounded_String) is
   begin
      Send_Command(Client, UNKNOWN, XML_Text, Server_Addr);
   end Send_XML_Config;

   -----------------------
   -- Create_XML_Config --
   -----------------------

   function Create_XML_Config(Db: in Peer_DB;
                              Peer: in Sock_Addr_Type)
                              return Unbounded_String is
      Txt : Unbounded_String;
   begin

      Txt :=       To_Unbounded_String("<configuration>");
      Txt := Txt & To_Unbounded_String(" <session port=""" & Integer'Image(Integer(Peer.Port)));
      Txt := Txt & To_Unbounded_String( """>");
      Txt := Txt & To_Unbounded_String("   <server address=""192.168.0.2"" port=""12345"" id=""12""/>");
      Txt := Txt & To_Unbounded_String("   <output>");
      Txt := Txt & To_Unbounded_String("     <channel id=""1"" max-target=""3"">");
      Txt := Txt & To_Unbounded_String("       <profile name=""basic""/>");
      Txt := Txt & To_Unbounded_String("     </channel>");
      Txt := Txt & To_Unbounded_String("     <channel id=""2""> ");
      Txt := Txt & To_Unbounded_String("       <profile name=""basic""/>");
      Txt := Txt & To_Unbounded_String("     </channel>");
      Txt := Txt & To_Unbounded_String("   </output>");


      Txt := Txt & To_Unbounded_String("   <input> ");
      if (Server_DB.Count(Db) = 0) then
         -- no peer registered, use a fake one
         Txt := Txt & To_Unbounded_String("     <peer>");
         Txt := Txt & To_Unbounded_String("       <address address=""192.168.0.1"" port=""3214"" channel=""1"" />");
         Txt := Txt & To_Unbounded_String("       <auth-data>");
         Txt := Txt & To_Unbounded_String("         <auth-profile name=""void"" />");
         Txt := Txt & To_Unbounded_String("       </auth-data>");
         Txt := Txt & To_Unbounded_String("     </peer>");
      else

         for i in integer range 1 .. Server_DB.Count(Db) loop
            declare
               Item : DB_Item;
            begin
               Item := Get(Db,i);
               Txt := Txt & To_Unbounded_String("     <peer>");
               Txt := Txt & To_Unbounded_String("       <address address=""");
               Txt := Txt & To_Unbounded_String(Image(Item.Address.Addr));
               Txt := Txt & To_Unbounded_String(""" port=""");
               Txt := Txt & To_Unbounded_String(Integer'Image(Integer(Item.Address.Port)));
               Txt := Txt & To_Unbounded_String(""" channel=""1"" />");
               Txt := Txt & To_Unbounded_String("       <auth-data>");
               Txt := Txt & To_Unbounded_String("         <auth-profile name=""void"" />");
               Txt := Txt & To_Unbounded_String("       </auth-data>");
               Txt := Txt & To_Unbounded_String("     </peer>");
            end;
         end loop;
      end if;

      Txt := Txt & To_Unbounded_String("   </input>");

      Txt := Txt & To_Unbounded_String(" </session>");
      Txt := Txt & To_Unbounded_String("</configuration>");

      return Txt;
   end Create_XML_Config;



   ----------------------
   -- Create_Peer_List --
   ----------------------

   -- Create the list of peer for the PLAY command, it return a sub-list of the
   -- Peer_DB with "Returned_Peer_Number" element
   function Create_Peer_List(Db: in Peer_DB;
                             Size: in Positive) return Peer_DB is
      Num_Of_Peers: Integer;
      Peer_List: Peer_DB;
      Peer: DB_Item;

   begin

      Put_Line("The database contains: " &
               Integer'Image( Server_DB.Count(Db)) & " elements");

      -- how many peer to return
      if Server_DB.Count(Db) < Size then
         Num_Of_Peers := Server_DB.Count(Db);
      else
         Num_Of_Peers := Size;
      end if;


      -- create the list
      while Num_Of_Peers > 0 loop

         begin
            Peer := Get(Db, Random_Pos(Server_DB.Count(Db)));
            Add(Peer_List, Peer);
            Num_Of_Peers := Num_Of_Peers - 1;
         exception
            when peer_yet_present => null; -- if this exception is raised than
         end;				   -- the peer is yet in the list, don't
					   -- decrement num_of_peers
      end loop;

      return Peer_List;

   end Create_Peer_List;


   ----------------
   -- Send_Punch --
   ----------------

   -- Send a PPETP PUNCH message to all the peer indicated in the "db" parameter with
   -- the id of the peer in the "id"
   procedure Send_Punch(Db: Peer_DB; Id: DB_Item) is
      M: Unbounded_String;
      Self_Client: Sock_Addr_Type;
   begin

      Self_Client.Addr := Inet_Addr("127.0.0.1");
      Self_Client.Port := Port_Type(6661);


      for i in integer range 1..Server_Db.Count(Db) loop

         declare
            Item: DB_Item := Get(Db,i);
            Client: Sock_Addr_Type := Item.Address;
         begin
            Put_Line("Send PUNCH msg to: " & Image(Client));
            M := To_Unbounded_String(Image(Client));
            M := M & " ";
            M := M & To_Unbounded_String(Image(Id.Address));

            Send_String(To   => Self_Client,
                        Msg  => M,
                        From => Server_Addr);
         end;

      end loop;

   end Send_Punch;


   ------------------
   -- Play_Session --
   ------------------

   -- Azioni da eseguire a seguito del metodo play
   procedure Play_Session(From: Sock_Addr_Type) is
      Peer_Id: Peer_Identifier;
      Msg: Unbounded_String;
      Peer: DB_Item;
      p: Unbounded_String;

      peer_list: Peer_DB;
   begin

      Put_Line("Play");

      -- Generate a random id for the client
      Peer_Id := Random_Peer_ID;
      Put_Line("ID generated: " & To_String(Image(Peer_Id)));
      Msg := Image(Peer_Id);


      -- create the list of the peers

      -- Msg := Msg & Peer_DB_To_String(Peer_List);


      -- Add the new peer to server db ??
      -- maybe is better to do this procedure at the end of the entire procedure
      Peer.ID := Peer_Id;
      Peer.Address.Addr := From.Addr;
      Peer.Address.Port := Port_Type(Random_Port); -- this port is transmitted to the
      						   -- peer in the XML config



      -- Send the message to the client
      Put_Line("Message to send: " & To_String(Msg));
      Put_Line("Send list to client");
      delay 0.2; -- for sync with client
      Send_Command(From, UNKNOWN, Msg, Server_Addr);


      -- Waiting for the HELO Id
      Put_Line("Waiting for Helo");

      Msg :=  Image(Peer.ID);
      declare
         From1: Sock_Addr_Type;
      begin

         From1 := From;
         Wait_For_Command(Server_Addr, HELO, Msg, From1);
      end;


      -- send ACK
      Put_Line("Send ACK");
      delay 0.2; -- for sync with client

      Send_Command(From, ACK, NO_PARAMETERS, Server_Addr);


      Peer_List := Create_Peer_List(Database, Returned_Peer_Number);

      declare
         XML_Text : Unbounded_String;
      begin
         XML_Text := Create_XML_Config(Peer_List, Peer.Address);
         delay 0.2;
         Put_Line("Send XML Config");
         Send_XML_Config(From,XML_Text);
      end;


      delay 0.2;
      -- Send a punch to all the extracted peer in the peer_list
      Send_Punch(Peer_List, Peer);

      Add(Database, Peer);

   end Play_Session;



begin


   Initialize;
   --Server_Addr.Addr := Addresses(Get_Host_By_Name(Host_Name),1);
   Server_Addr.Addr := Any_Inet_Addr;
   Server_Addr.Port := Port_Type(8080);

   Put_Line("Server @ " & Image(Server_Addr));

   -- Wait for a PLAY command

   loop

      New_Line(2);
      Put_Line("Wait for PLAY message");


      Message := NO_PARAMETERS;
      From := No_Sock_Addr;

      Wait_For_Command(Server_Addr, PLAY, Message, From);
      Put_Line("Recevived message from: " & Image(From));
      Put_Line("Message: " & To_String(Message));

      Play_Session(From);

   end loop;

end Server_Main;

