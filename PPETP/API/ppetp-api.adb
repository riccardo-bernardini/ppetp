with Ada.Streams;
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;		use Ada.Text_IO;


with Network; use Network;
with Main_Task;
with Command_Queues;
with Session_Tables;
with PPETP.Sessions;
with PPETP.Channels;

with Network;
with network_UDP;      use network_UDP;
with Packets.Binary.Application;  use Packets.Binary.Application;

with Profiles; 			use Profiles;
with Profiles.Parameters; 	use Profiles.Parameters;
with Auth.Profiles;		use Auth.Profiles;
with Auth.Credentials;		use Auth.Credentials;
with byte_arrays;		use byte_arrays;

with Control_Task.Commands;	use Control_Task.Commands;

with PPETP_Mailboxes;

with Configuration;		use Configuration;
with Configuration.Xml;		use Configuration.Xml;

with Peer_Manager;	use Peer_Manager;
with Peer_Informations; use Peer_Informations;

with PPETP.Attributes;			use PPETP.Attributes;
with PPETP.Attributes.New_Peer;		use PPETP.Attributes.New_Peer;

package body PPETP.API is

   Session_Table : Session_Tables.Table;

   ----------------------
   -- Check_Session_Id --
   ----------------------
   procedure Check_Session_Id (Session : Session_ID) is
   begin
      if (not Session_Table.Exists (Session)) then
         raise Invalid_Session;
      end if;
   end Check_Session_Id;







   -----------------
   -- New_Session --
   -----------------
   function New_Session (Ctl             : Port_Type := Any_Port;
                         My_Public_IP    : Inet_Addr_Type := No_Inet_Addr;
                         Profile         : Profile_Type := Basic_Profile;
                         Prof_Parameters : Parameters_Class_Pt;
                         PeerID          : Peer_ID := 0;
                         N_Trials        : Natural := 1) return Session_ID is

      New_Id : Session_Id;
      Tmp_PeerID : Peer_ID;

      package Random_PeerID is
        new Ada.Numerics.Discrete_Random (Peer_ID);

      G : Random_PeerID.Generator;

   begin

      -- If not PeerID assigned generate a random one
      if PeerID = 0 then
         Random_PeerID.Reset(G);
         Tmp_PeerID := Random_PeerID.Random(G);
      else
         Tmp_PeerID := PeerID;
      end if;

      --* TODO Niente IP, lo trova automaticamente

      --* TODO Niente Profilo e parametri ... si può fare

      Session_table.Reserve (New_Id);
      Session_Table.Replace (New_Id, PPETP.Sessions.New_Session(Ctl_Port        => Ctl,
                                                                My_Public_IP    => My_Public_IP,
                                                                Profile         => Profile,
                                                                Prof_Parameters => Prof_Parameters,
                                                                PeerID          => Tmp_PeerID,
                                                                N_Trials        => N_Trials));
      return New_Id;

   exception
      when Session_Tables.Table_Full =>
         raise Too_Many_Sessions;
      when others =>
         raise;
   end New_Session;


   -----------------
   -- New_Session --
   -----------------
   function New_Session(XML_Configuration: String;
                        Ctl              : Port_Type := Any_Port;
                        My_Public_IP     : Inet_Addr_Type := No_Inet_Addr;
                        PeerID           : Peer_ID := 0;
                        N_Trials         : Natural := 1) return Session_ID is

      Data : Config_Data := Parse(XML_Configuration);
      New_Id : Session_Id;
   begin

--        Put_Line("****************************************************************");
--        Dump(Data);
--        Put_Line("****************************************************************");

      -- Create the new session
      New_Id := New_Session(Ctl             => Ctl,
                            My_Public_IP    => My_Public_IP,
                            Profile         => Data(1).Profile,
                            Prof_Parameters => Data(1).Parameters,
                            PeerID          => PeerID,
                            N_Trials        => N_Trials);

      -- Create all the channels
      if Data(1).Outputs /= null then
--         Put_Line("Ch: " & Data(1).Outputs'Length'img);
         for i in 1 .. Data(1).Outputs'Length loop
            New_Channel(Session    => New_Id,
                        Profile    => Data(1).Outputs(i).Profile,
                        Parameters => Data(1).Outputs(i).Parameters,
                        Channel    => Data(1).Outputs(i).Id);
         end loop;
      end if;

      -- Connect to peers
      if Data(1).Inputs /= null then
      --   Put_Line("Peer: " & Data(1).Inputs'Length'img);
         for i in 1 .. Data(1).Inputs'Length loop
            -- if a peer has more than one channel, connect to all channel
            for ch in 1..Data(1).Inputs(i).Channels'Length loop

               Put_Line("API connect to peer: " & Image(Data(1).Inputs(i).Addr) );
               Put_Line("               port: " & Data(1).Inputs(i).Port'img);
               Put_Line("                 ch: " & Data(1).Inputs(i).Channels(ch).Id'img);
               Connect_To_Peer(Session   => New_id,
                               Peer_Addr => Data(1).Inputs(i).Addr,
                               Port      => Data(1).Inputs(i).Port,
                               Channel   => Data(1).Inputs(i).Channels(ch).Id,
                               PeerID    => Data(1).Inputs(i).Id);
            end loop;
         end loop;
      end if;

      return New_Id;

   end New_Session;


   -----------
   -- Close --
   -----------
   procedure Close (Session : Session_ID) is
      Parent: Sessions.Session;
   begin
      Check_Session_Id (Session);

      Parent := Session_Table.Get(Session);
      Sessions.Destroy (Parent);
      Session_Table.Delete (Session);
   end Close;



   ----------------
   -- Get_PeerID --
   ----------------
   function Get_PeerID (Session : in Session_ID)
                        return Peer_ID is
   begin
      Check_Session_Id (Session);
      return Session_Table.Get(Session).My_PeerID.all;
   end Get_PeerID;


   ----------------
   -- Set_PeerID --
   ----------------
   procedure Set_PeerID (Session: in Session_ID;
                         PeerID: in     Peer_ID)is
   begin
      Check_Session_Id (Session);

      if Session_Table.Get (Session).State.Started then
         raise Session_Yet_Started;
      end if;

      Sessions.Set_PeerID(Parent => Session_Table.Get (Session),
                          PeerID => PeerID);
   end Set_PeerID;


   -----------------
   -- Set_Address --
   -----------------
   procedure Set_Address (Session: in Session_ID;
                          Addr:    in Inet_Addr_Type) is
   begin
      Check_Session_Id (Session);

      if Session_Table.Get (Session).State.Started then
         raise Session_Yet_Started;
      end if;

      Sessions.Set_New_Address(Parent => Session_Table.Get(Session),
                               Addr   =>  Addr);

   end Set_Address;

   -----------------
   -- Get_Address --
   -----------------
   function Get_Address (Session : in Session_ID)
                         return Inet_Addr_Type is
   begin
      Check_Session_Id (Session);

      return Session_Table.Get(Session).My_Address;
   end Get_Address;

   --------------
   -- Set_Port --
   --------------
   procedure Set_Port (Session: in Session_ID;
                       Port:    in Port_Type) is
   begin
      Check_Session_Id (Session);

      if Session_Table.Get (Session).State.Started then
         raise Session_Yet_Started;
      end if;

      Sessions.Set_New_Port(Parent => Session_Table.Get (Session),
                            Port   => Port);

   end Set_Port;

   --------------
   -- Get_Port --
   --------------
   function Get_Port (Session : in Session_ID)
                      return Port_Type is
   begin
      Check_Session_Id (Session);

      return Session_Table.Get(Session).Control_Port;
   end Get_Port;



   -----------------------
   -- Get_Internal_Port --
   -----------------------
   function Get_Internal_Port(Session: in Session_ID) return Port_Type is
   begin
      Check_Session_Id (Session);
      return Get_Port (Session_Table.Get(Session).Inter_Proc_Socket);
   end Get_Internal_Port;


   -----------------
   -- Set_Profile --
   -----------------
   procedure Set_Profile(Session: in Session_ID;
                         Profile: Profile_Type := Basic_Profile;
                         Parameters: Parameters_Class_Pt := null) is
   begin
      Check_Session_Id (Session);

      if Session_Table.Get (Session).State.Started then
         raise Session_Yet_Started;		--* TODO  solo perchè per ora non si possono liberare i parametri
      end if;

      -- Delete old parameters
      if Session_Table.Get (Session).State.Profile then
         Free(Session_Table.Get (Session).Prof_Parameters);
      end if;

      Put_Line(" ** Set_Low_Level -> True");
      Session_Table.Get (Session).State.Profile := True;

      Session_Table.Get(Session).Profile := Profile;
      Session_Table.Get(Session).Prof_Parameters := Parameters;


      Sessions.Set_Ready_Flag( Session_Table.Get (Session).State);

   end Set_Profile;



   ----------------------
   -- Set_Sequence_Num --
   ----------------------
   procedure Set_Sequence_Num(Session  : in Session_ID;
                              StreamID : in Stream_ID;
                              Seq_Num  : in Data_Sequence_Number) is
   begin
      Check_Session_Id (Session);

      Session_Table.Get(Session).Sequence_Num(StreamID) := Seq_Num;
   end Set_Sequence_Num;


   ---------------------------
   --  Set_Default_StreamID --
   ---------------------------
   procedure Set_Default_StreamID(Session  : in Session_ID;
                                  StreamID : in Stream_ID) is
   begin
      Check_Session_Id (Session);

      Session_Table.Get(Session).Default_StreamID := StreamID;
   end Set_Default_StreamID;


   -----------------
   -- New_Channel --
   -----------------
   procedure New_Channel (Session    : Session_ID;
                          Profile    : Profiles.Profile_Type;
                          Parameters : Profiles.Parameters.Parameters_Class_Pt := null;
                          Channel    : PPETP_Channel_ID) is

   begin
      Check_Session_Id (Session);


      Session_Table.Get (Session).Transmitter.New_Channel (Profile,
                                                           Parameters,
                                                           Session_Table.Get(Session).Output_Queue,
                                                           Channel);

   end New_Channel;


   -----------------
   -- Get_Channel --
   -----------------
   procedure Get_Channel (Session    : in     Session_ID;
                          Channel    : in     PPETP_Channel_ID;
                          Profile    :    out Profiles.Profile_Type;
                          Parameters :    out Profiles.Parameters.Parameters_Class_Pt) is

      Ch : PPETP.Channels.Channel;

   begin
      Check_Session_Id (Session);

      Session_Table.Get (Session).Transmitter.Get_Channel(Channel, Ch);



      Profile    := Ch.Get_Profile;
      Parameters := new Root_Parameters'Class'(Ch.Get_Parameters.all);

   end Get_Channel;



   -------------
   -- Connect --
   -------------
   procedure Connect_To_Peer (Session   : Session_ID;
                              Peer_Addr : Inet_Addr_Type;
                              Port      : Port_Type;  -- <-- in genere potrebbe essere la porta UDP di un NAT
                              Channel   : PPETP_Channel_ID;
                              PeerID    : Peer_ID) is

      Addr       : Sock_Addr_Type;
      My_Addr    : Sock_Addr_Type;
      Auth       : Auth_Data;
      --Void_Array : Byte_Array(1..4);
      Mbox : PPETP_Mailboxes.Mailbox_Access := new PPETP_Mailboxes.Mailbox;
      PM_Result : Boolean;
   begin

      Check_Session_Id (Session);

      -- Il server dovrebbe aver mandato il punch
      -- Hole Punching

      Addr.Addr := Peer_Addr;
      Addr.Port := Port;

      Put_Line("Addr " & Image(Addr));


      -- Sostituire con le credenziali... se ce ne saranno
--        for i in integer range 1..4 loop
--           Void_Array(Ada.Streams.Stream_Element_Offset(i)) := 0;
--        end loop;
--
--        Auth := New_Credential(Data    => Void_Array,
--                               Profile => Void_profile);


      --***************************   TODO *************************************
      --*
      --*	Finchè il network manager non supporta il
      --* 	PeerID non è possibile fare le prove con più
      --*	peer
      --*
      --************************************************************************

      if not Is_Present(PM   => Session_Table.Get(Session).Peer_Manag,
                        Addr => Addr) then

--           Put_Line("API: Add Peer in Peer Manager");
--           Put_Line("  Addr: " & Image(Addr));
--           Put_Line("  PeerID: " & PeerID'img);

         -- Inserisco il peer nel Peer_Manager
         Add_Peer(PM           => Session_Table.Get(Session).Peer_Manag,
                  PeerID       => PeerID, -- per ora è quello del server streaming specificato in config.txt
                  PeerKind     => Upper_Peer,
                  Address      => Addr,
                  Peer_Cred    => null,
                  Puncturing   => null,
                  Routing_Prob => null,
                  Result       => PM_Result
                 ) ;
      end if;

      -- Si può mandare un pacchetto NUL per verificare che sia aperto
      -- se si riceve l'ack allora è ok!


      My_Addr.Addr := Session_Table.Get (Session).My_Address;
      My_Addr.Port := Port_Type( Session_Table.Get (Session).Control_Port );




      -- una volta effettuato l'hole punching si deve dire al peer che aggiunga
      -- il target e inizi a mandare dati



--        Send_Start_Data_Command(Control         => Session_Table.Get (Session).Control,
--                                Dst_Peer        => Addr,
--                                From_Input_Port => False,
--                                Target          => My_Addr,
--                                Channel         => Channel,
--                                Credentials     => Auth, -- Prima o poi mettere le vere credenziali
--                                Credentials_2   => Auth, -- Prima o poi mettere le vere credenziali
--                                Mbox            => MBox); -- Se riceve l'ack so che il peer remoto mi manderà pacchetti


      --* TODO  l'attributo scritto cosi è solo per i test
      declare
         my_attr : Access_Attribute_Class := new NEW_PEER_Attribute;

      begin

        Set_Attribute(Object     => NEW_PEER_Attribute(my_Attr.all),
                      Address    => My_Addr,
                      T_Protocol => 17,
                      PeerID     => Session_Table.Get (Session).My_PeerID.all);

--           my_attr := Access_Attribute_Class'( new NEW_PEER_Attribute'(Attr_Type  => NEW_PEER_Attribute_Index,
--                                                                       T_Protocol => 17,
--                                                                       Address    => My_Addr,
--                                                                       PeerID     => PeerId));

--           Put_Line("API: MY Attribute");
--           Image(NEW_PEER_Attribute(my_Attr.all));
--           New_Line;

         Send_Start_Command(Control         => Session_Table.Get (Session).Control,
                            Dst_Peer        => Addr,

                            Start_Channel  => Channel,
                            Start_New_Peer => my_attr,
                            Mbox           => MBox);

      end;



   end Connect_To_Peer;


   --------------
   -- Add_Peer --
   --------------
   procedure Add_Peer (Session  : Session_ID;
                       PeerID   : Peer_ID;
                       Address  : Sock_Addr_Type;
                       PeerKind : Peer_Kind;
                       Peer_Cred    : Access_Attribute_Class := null;
                       Puncturing   : Access_Attribute_Class := null;
                       Routing_Prob : Access_Attribute_Class := null;
                       Result   : in out Boolean) is
   begin
      Check_Session_Id (Session);

      Add_Peer(PM           => Session_Table.Get (Session).Peer_Manag,
               PeerID       => PeerID,
               Address      => Address,
               PeerKind     => PeerKind,
               Peer_Cred    => Peer_Cred,
               Puncturing   => Puncturing,
               Routing_Prob => Routing_Prob,
               Result       => Result);

   end Add_Peer;


   -----------------
   -- Delete_Peer --
   -----------------
   procedure Delete_Peer (Session  : Session_ID;
                          PeerID   : Peer_ID;
                          Result   : in out Boolean) is
   begin

      Check_Session_Id (Session);

      Remove_Peer(PM     => Session_Table.Get (Session).Peer_Manag,
                  PeerID => PeerID,
                  Result => Result);

   end Delete_Peer;




   ----------
   -- Send --
   ----------
   procedure Send (Session   : Session_ID;
                   StreamID  : PPETP.Stream_ID;
                   Data      : Streams.Stream_Element_Array)   is


   begin

      Check_Session_Id (Session);

      Sessions.Send(Parent   => Session_Table.Get (Session),
                    StreamID => StreamID,
                    Data     => Data);

   end Send;
   pragma inline(Send);

   ----------
   -- Recv --
   ----------
   procedure Recv (Session   : in     Session_ID;
                   StreamID  :    out PPETP.Stream_ID;
                   Data      :    out Streams.Stream_Element_Array;
                   Last      :    out Streams.Stream_Element_Offset)
   is

   begin
      Check_Session_Id (Session);

      Sessions.Recv(Parent   => Session_Table.Get (Session),
                    StreamID => StreamID,
                    Data     => Data,
                    Last     => Last);
   end Recv;
   pragma inline(Recv);


   ------------------------
   -- Send_Routed_Packet --
   ------------------------
   procedure Send_Routed_Packet (Session    : in Session_ID;
                                 Target_ID  : in Peer_ID;
                                 Command    : in Command_Class;
                                 Channel    : in Channel_ID;
                                 Attributes : in Attributes_Record;
                                 Mbox :       in PPETP_Mailboxes.Mailbox_Access) is
      My_Addr: Sock_Addr_Type;
   begin

      My_Addr.Addr := Session_Table.Get (Session).My_Address;
      My_Addr.Port := Port_Type(Session_Table.Get (Session).Control_Port);

      Session_Table.Get (Session).main.Send_Routed_Command(My_Addr    => My_Addr,
                                                           Target_ID  => Target_ID,
                                                           Command    => Command,
                                                           Channel    => Channel,
                                                           Attributes => Attributes,
                                                           Mbox       => Mbox);
   end Send_Routed_Packet;




   -----------------
   -- Print_State --
   -----------------
   procedure Print_State(Session   : Session_ID) is
   begin
      Check_Session_Id (Session);

      Put_Line("Transmitter ->");
      Session_Table.Get (Session).Transmitter.Print(1);
   end Print_State;



   -----------------------------------------------------------------------------
   --
   --		NEW API
   --
   -----------------------------------------------------------------------------

   -----------------
   -- New_Session --
   -----------------
   function New_Session (Host_Port     : in Port_Type := No_Port;
                         Host_IP       : in Inet_Addr_Type := Any_Inet_Addr;
                         Internal_Port : in Port_Type := No_Port;
                         PeerID :        in Peer_ID := No_Peer_ID) return Session_ID is

      New_Id : Session_Id;

      H_Port: Port_Type;
      I_Port: Port_Type;

      New_PeerID : Peer_ID := PeerID;

   begin


      -- If not Host Port is specified... find ones
      if (Host_Port = Any_Port) or (Host_Port = No_Port) then
         H_Port := Find_Free_Port(Addr => Host_IP,
                                  Res => False);
      end if;

      -- If not Internal Port is specified... find ones
      if (Internal_Port = Any_Port) or (Internal_Port = No_Port) then
         I_Port := Find_Free_Port(Addr => Inet_Addr("127.0.0.1"),
                                  Res => True);
      end if;


      Put_Line("API 1: " & Image(Host_IP) & ":" & H_Port'img );

      -- If not PeerID generate one
      if PeerID = No_Peer_ID then

         declare
            package Random_PeerID is
              new Ada.Numerics.Discrete_Random (Peer_ID);

            G : Random_PeerID.Generator;

         begin

            Random_PeerID.Reset(G);

            New_PeerID := Random_PeerID.Random(G);


         end;

      end if;


      Session_table.Reserve (New_Id);
      Session_Table.Replace (New_Id, PPETP.Sessions.New_Session(Host_Port     => H_Port,
                                                                Host_IP       => Host_IP,
                                                                Internal_Port => I_Port,
                                                                PeerID        => New_PeerID) );




      Sessions.Set_Low_Level_Values(Parent         => Session_Table.Get (New_ID),
                                    Max_Trials     => 2,
                                    Timeout        => 5.0,
                                    Routed_Timeout => 10.0 );

      Put_Line(" ** Set_Low_Level -> True");
      Session_Table.Get (New_Id).State.Low_Level := True;

      return New_Id;

   exception
      when Session_Tables.Table_Full =>
         raise Too_Many_Sessions;
      when others =>
         raise;


   end New_Session;


   -------------------
   -- Start_Session --
   -------------------
   procedure Start_Session (Session : Session_ID) is

   begin
      Check_Session_Id (Session);


      if not Session_Table.Get (Session).State.Ready then
         Put_Line("EXCEPTION Session_Not_Ready!");
         raise Session_Not_Ready;

      end if;

      Sessions.Start_Session(Parent => Session_Table.Get (Session));

      Session_Table.Get (Session).State.Started := True;




   end Start_Session;



begin
   Session_Table.Resize(256);


end PPETP.API;
