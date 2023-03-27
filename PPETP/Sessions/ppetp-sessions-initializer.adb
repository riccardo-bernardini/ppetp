with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams; use Ada.Streams;

with Boolean_Mailboxes; use Boolean_Mailboxes;
with Main_Task; use Main_Task;
with Packets.Protocol.Command; use Packets.Protocol.Command;
with Auth.Credentials; use Auth.Credentials;
with Auth.Profiles; use Auth.Profiles;
with byte_arrays; use byte_arrays;


package body PPETP.Sessions.Initializer is


   ----------
   -- Init --
   ----------

   function Init(Conf: Configuration.Config_Data) return Initializer is
      Config  : Configuration.Config_Data(1..1) := Conf;
      Session : PPETP.Sessions.Session;
      Ch_List : Channel_List_Type_Pt;
      Result  : Initializer;
      Addr_Srv: Sock_Addr_Type;
      MBox    : Boolean_Mailboxes.Mailbox_Access := null;
   begin



      -- TODO --
      -- Forse non e' corretto che la porta la prenda dal file di configurazione

      Session := New_Session(Config(1).Port, Config(1).Profile);

      Add_Channels_To_Session(Session, Config, Ch_List);

      Send_Hello_To_Sources(Session, Config);

      Result := new Initializer_Record'(Session => Session,
                                        Config  => Config,
                                        Channel_List => Ch_List);


      Addr_Srv.Addr := Config(1).Server.Addr;
      Addr_Srv.Port := Config(1).Server.Port;


      Auto_Punch(Session  => Session,
                 Address  => Addr_Srv,
                 Reply_To => MBox);


      return Result;


   end Init;

   ----------------
   -- Send_Punch --
   ----------------
   procedure Send_Punch(Session_Init: Initializer;
                        Address     : Sock_Addr_Type;
                        To          : Sock_Addr_Type) is

      Auth       : Auth_Data;
      Void_Array : Byte_Array(1..4);

   begin

      for i in integer range 1..4 loop
         Void_Array(Stream_Element_Offset(i)) := 0;
      end loop;



      Auth := New_Credential(Data    => Void_Array,
                             Profile => Void_profile);


      Session_Init.Session.Control.Send_Command(What	=> Control_Packet'(Command =>
                                                                          Packets.Protocol.Command.Punch,
                                                                        Timestamp => <>,
                                                                        Peer => <>,
                                                                        Who => Address,
                                                                        Server_Cred => Auth),
                                                To       => To,
                                                Reply_To => null);

   end Send_Punch;



   ------------------
   -- Add_Channels --
   ------------------

   procedure Add_Channels_To_Session(Session : in out PPETP.Sessions.Session;
                                     Config  : in out Config_Data;
                                     Ch_List :    out Channel_List_Type_Pt) is
   begin
      Ch_List := new Channel_List_Type(1..Config(1).Outputs'length);


      for i in 1..Config(1).Outputs'length loop

         declare
            Ch_ID : PPETP.Channel_ID;

         begin

            New_Channel(Session, Config(1).Outputs(i).Profile,
                        Config(1).Outputs(i).Parameters, Ch_ID);


            Ch_List(PPETP.PPETP_Channel_ID(Config(1).Outputs(i).Id)) := Ch_ID;

         end;
      end loop;

   end Add_Channels_To_Session;


   -----------------
   -- Add_Sources --
   -----------------

   procedure Send_Hello_To_Sources(Session : in out PPETP.Sessions.Session;
                                   Config  : in out Config_Data) is

      Mbox: Boolean_Mailboxes.Mailbox_Access := new Boolean_Mailboxes.Mailbox;
      OK: Boolean;
   begin

      for i in 1..Config(1).Inputs'length loop
         declare
            Addr   : Inet_Addr_Type := Config(1).Inputs(i).Addr;
            Port   : Port_Type := Config(1).Inputs(i).Port;

            Sock_Addr : Sock_Addr_Type;
         begin

            Sock_Addr.Addr := Addr;
            Sock_Addr.Port := Port;


            --
            Auto_Punch(Session  => Session,
                       Address  => Sock_Addr,
                       Reply_To => Mbox);

            Mbox.Wait(OK);
            -- how many sources
            if OK then
               null;
            else
               null;
            end if;

         end;

      end loop;

   end Send_Hello_To_Sources;


   function Send_Hello(Session  : PPETP.Sessions.Session;
                       Dst_Peer : Network.Sock_Addr_Type) return Boolean is

      Mbox      : Boolean_Mailboxes.Mailbox_Access := new Boolean_Mailboxes.Mailbox;
      OK        : Boolean;
      Hello_Pkt : Control_Packet(Hello);
   begin

      Hello_Pkt.Reply_Port := Session.Control_Port;
      Hello_Pkt.Profile    := Session.Profile;


      -- Send a HELLO to the peer and wait for ACK
      Session.Control.Send_Command(What	    => Hello_Pkt,
                                   To       => Dst_Peer,
                                   Reply_To => Mbox);


      Mbox.Wait(OK);


      return OK;

   end Send_Hello;


   ----------------
   -- Auto_Punch --
   ----------------

   -- Corrisponde a mandare un hello dalla porta di ricezione (senza ack)
   -- ed uno da quella di trasmissione (con ack)
   -- viene attivato il processo Send_Hello_And_Wait del Main cosi da non bloccare
   -- il processo principale in attesa dell'ack
   -- si può evitare mettendo una entry nel main che richiama la procedura
   -- Send_Hello_And_Wait... che si chiami Send_Double_Hello.... molto più'
   -- elegante e chiaro
   procedure Auto_Punch(Session : in out PPETP.Sessions.Session;
                        Address : in     Sock_Addr_Type;
                        Reply_To: in out Boolean_Mailboxes.Mailbox_Access) is
      Auth       : Auth_Data;
      Void_Array : Byte_Array(1..4);
      Addr       : Sock_Addr_Type;
   begin

      for i in integer range 1..4 loop
         Void_Array(Stream_Element_Offset(i)) := 0;
      end loop;

      Addr.Addr := Inet_Addr("127.0.0.1");
      Addr.Port := Session.Control_Port;

      Put_Line("Session: Auto_Punch");

      Auth := New_Credential(Data    => Void_Array,
                             Profile => Void_profile);

      -- Send a Punch message to the peer itself with the passed address
      Session.Control.Send_Command(What	=> Control_Packet'(Command =>
                                                             Packets.Protocol.Command.Punch,
                                                           Timestamp => <>,
                                                           Peer => <>,
                                                           Who => Address,
                                                           Server_Cred => Auth),
                                   To       => Addr,
                                   Reply_To => Reply_To);

      --Usare qualcosa tipo (il main_Task qui non è visibile
      --Main_Task.Send_Double_Hello(To   => Address,
      --                            Auth => Auth);



   end Auto_Punch;


   ---------------
   -- Ping_Peer --
   ---------------
   function Ping_Peer(Session_Init: Initializer;
                      To          : Sock_Addr_Type) return boolean is
   begin
      return Send_Hello(Session  => Session_Init.Session,
                        Dst_Peer => To);
   end Ping_Peer;





end PPETP.Sessions.Initializer;
