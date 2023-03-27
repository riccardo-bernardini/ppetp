with Ada.Text_IO;		use Ada.Text_IO;

with PPETP.API;

with Configuration;		use Configuration;
with Configuration.Xml;		use Configuration.Xml;

with PPETP_Mailboxes;
with PPETP_Mail;		use PPETP_Mail;

with PPETP.Attributes.Peer_Credential;		use PPETP.Attributes.Peer_Credential;
with PPETP.Attributes.Routing_Probability;	use PPETP.Attributes.Routing_Probability;
with PPETP.Attributes.Puncturing;		use PPETP.Attributes.Puncturing;
with PPETP.Attributes.New_Peer;			use PPETP.Attributes.New_Peer;
with PPETP.Attributes.Old_Peer;			use PPETP.Attributes.Old_Peer;
with PPETP.Attributes.Attributes_Records;	use PPETP.Attributes.Attributes_Records;

with Packets.Internal; 		use Packets.Internal;


package body PPETP.External_API is

   -------------------
   -- Set_Peer_Info --
   -------------------
   procedure Set_Peer_Info(Peer :   in out Peer_Type;
                           PeerID : in     Peer_ID;
                           Addr :   in     Sock_Addr_Type;
                           Kind :   in     Peer_Kind) is
   begin
      Peer.PeerID   := PeerID;
      Peer.Address  := Addr;
      Peer.PeerKind := Kind;
      -- the other fields are null if not setted;
   end Set_Peer_Info;




   -------------------------
   -- Set_Peer_Credential --
   -------------------------
   procedure Set_Peer_Credential(Peer : in out Peer_Type;
                                 Data : in     Byte_Array) is
   begin


      Free(Peer.Credential);

      Peer.Credential := new PEER_CREDENTIAL_Attribute(Data'Length);
      Set_Attribute(Object => PEER_CREDENTIAL_Attribute(Peer.Credential.all),
                    Data   => Data);

   end Set_Peer_Credential;


   -------------------------
   -- Set_Peer_Puncturing --
   -------------------------
   procedure Set_Peer_Puncturing(Peer      : in out Peer_Type;
                                 Punct_Info: in     Puncturing_Info) is

   begin

      Free(Peer.Puncturing);

      Peer.Puncturing := new PUNCTURING_Attribute(Punct_Info.Mode);
      Set_Attribute(Object => PUNCTURING_Attribute(Peer.Puncturing.all),
                    Data   => Punct_Info);

   end Set_Peer_Puncturing;


   ----------------------
   -- Set_Peer_Routing --
   ----------------------
   procedure Set_Peer_Routing(Peer : in out Peer_Type;
                              Num :  in     Byte;
                              Den :  in     Byte) is
   begin

      Free(Peer.Routing);

      Peer.Routing := new ROUTING_PROBABILITY_Attribute;
      Set_Attribute(Object => ROUTING_PROBABILITY_Attribute(Peer.Routing.all),
                    Num    => Num,
                    Den    => Den);

   end Set_Peer_Routing;



   ---------------------
   -- Get_Peer_PeerID --
   ---------------------
   function Get_Peer_PeerID(Peer : in Peer_Type) return Peer_ID is
   begin
      return Peer.PeerID;
   end;

   -------------------
   -- Get_Peer_Addr --
   -------------------
   function Get_Peer_Addr(Peer : in Peer_Type) return Sock_Addr_Type is
   begin
      return Peer.Address;
   end;

   -------------------
   -- Get_Peer_Kind --
   -------------------
   function Get_Peer_Kind(Peer : in Peer_Type) return Peer_Kind is
   begin
      return Peer.PeerKind;
   end;

   -------------------------
   -- Get_Peer_Credential --
   -------------------------
   function Get_Peer_Credential(Peer : in Peer_Type) return Access_Attribute_Class is
   begin
      return Peer.Credential;
   end;

   -------------------------
   -- Get_Peer_Puncturing --
   -------------------------
   function Get_Peer_Puncturing(Peer : in Peer_Type) return Access_Attribute_Class is
   begin
      return Peer.Puncturing;
   end;

   ----------------------
   -- Get_Peer_Routing --
   ----------------------
   function Get_Peer_Routing(Peer : in Peer_Type) return Access_Attribute_Class is
   begin
      return Peer.Routing;
   end;




   --
   -----------------------------------------------------------------------------
   --


   -----------------
   -- New_Session --
   -----------------
   function New_Session return Session_ID is
   begin
      Put_Line(" EXTERNAL API -> New_Session");
      return PPETP.API.New_Session;
   end New_Session;

   -------------------
   -- Close_Session --
   -------------------
   procedure Close_Session (Session : in Session_ID) is
   begin
      Put_Line(" EXTERNAL API -> Close_Session");
      PPETP.API.Close(Session);
   end Close_Session;

   -----------
   -- Start --
   -----------
   procedure Start (Session : in Session_ID) is
   begin
      Put_Line(" EXTERNAL API -> Start");
      PPETP.API.Start_Session(Session);
   end Start;



   --
   -----------------------------------------------------------------------------
   --


   -------------------
   -- Set_Info --
   -------------------
   procedure Set_Info (Session :   in Session_ID;
                       Host_Port : in Port_Type := No_Port;
                       Host_IP :   in Inet_Addr_Type := Any_Inet_Addr;
                       PeerID :    in Peer_ID := No_Peer_ID) is
   begin

      Put(" EXTERNAL API -> Set_Info: ");

      -- if the parameters have the default values than is not necessary
      -- modify the session because they have been initialized when the
      -- session were created

      if Host_Port /= No_Port then
         PPETP.API.Set_Port(Session => Session,
                            Port    => Host_Port);
         Put_Line("Port: " & Host_Port'img);
      end if;

      if Host_IP /= Any_Inet_Addr then
         PPETP.API.Set_Address(Session => Session,
                               Addr    => Host_IP);
         Put_Line("Address: " & Image(Host_IP));
      end if;

      if PeerID /= No_Peer_ID then
         PPETP.API.Set_PeerID(Session => Session,
                              PeerID  => PeerID);
         Put_Line("PeerID : " & PeerID'img);
      end if;

   end Set_Info;


   -----------------
   -- Set_Profile --
   -----------------
   procedure Set_Profile (Session :   in Session_ID;
                          Profile:    in Profile_Type := Basic_Profile;
                          Parameters: in Parameters_Class_Pt := null) is
      -- we must make a copy of the parameters, so the application cannot modify
      Param : Parameters_Class_Pt := new Root_Parameters'Class'(Parameters.all);
   begin

      Put_Line(" EXTERNAL API -> Set_Profile");

      PPETP.API.Set_Profile(Session    => Session,
                            Profile    => Profile,
                            Parameters => Param);
   end Set_Profile;


   -----------------
   -- New_Channel --
   -----------------
   procedure New_Channel (Session:    in Session_ID;
                          Channel:    in PPETP_Channel_ID;
                          Profile:    in Profile_Type := Basic_Profile;
                          Parameters: in Parameters_Class_Pt := null) is
      -- we must make a copy of the parameters, so the application cannot modify
      Param : Parameters_Class_Pt := new Root_Parameters'Class'(Parameters.all);
   begin

      Put_Line(" EXTERNAL API -> New_Channel");

      PPETP.API.New_Channel(Session    => Session,
                            Profile    => Profile,
                            Parameters => Parameters,
                            Channel    => Channel);
   end New_Channel;


   ---------------------
   -- Connect_To_Peer --
   ---------------------
   procedure Connect_To_Peer(Session   : Session_ID;
                             Peer_Addr : Inet_Addr_Type;
                             Port      : Port_Type;
                             Channel   : PPETP_Channel_ID;
                             PeerID    : Peer_ID) is
   begin

      Put_Line(" EXTERNAL API -> Connect_To_Peer");
      PPETP.API.Connect_To_Peer(Session   => Session,
                                Peer_Addr => Peer_Addr,
                                Port      => Port,
                                Channel   => Channel,
                                PeerID    => PeerID);
   end Connect_To_Peer;





   -------------------
   -- XML_Configure --
   -------------------
   procedure XML_Configure (Session  : in Session_ID;
                            XML_Conf : in String) is

      Data : Config_Data := Parse(XML_Conf);
   begin

      Put_Line(" EXTERNAL API -> XML_Configure");

      --*TODO add the server (if any) to the Peer Manager
--        declare
--           Peer_Server : Peer_Type;
--           Res : Boolean;
--        begin
--           Set_Peer_Info(Peer   => Peer_Server,
--                         PeerID => Data(1).Server.Id,
--                         Addr   => Sock_Addr_Type'(Family => Family_Inet,
--                                                   Addr => Data(1).Server.Addr,
--                                                   Port => Data(1).Server.Port),
--                         Kind   => Other_Peer);
--
--           Add_Peer (Session => Session,
--                     Peer    => Peer_Server,
--                     Result  => Res);
--
--
--           if Res then
--              Put_Line("Server added in the Peer Manager");
--           else
--              Put_Line("ERROR:  Server NOT added in the Peer Manager");
--           end if;
--        end;




      -- Set the profile for the disentangler
      PPETP.API.Set_Profile(Session    => Session,
                            Profile    => Data(1).Profile,
                            Parameters => Data(1).Parameters);

      -- Set the StreamID to use when the No_StreamID is used in the Send function
      PPETP.API.Set_Default_StreamID(Session  => Session,
                                     StreamID => Data(1).Default_StreamID);


      Put_Line("Session started");
      Start(Session => Session);			--  Here start the session !!!

      -- Create all the channels
      if Data(1).Outputs /= null then
         for i in 1 .. Data(1).Outputs'Length loop
            PPETP.API.New_Channel(Session    => Session,
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


               --* Qua da qualche parte c'è la chiamata ad ICE


               Put_Line("API connect to peer: " & Image(Data(1).Inputs(i).Addr) );
               Put_Line("               port: " & Data(1).Inputs(i).Port'img);
               Put_Line("                 ch: " & Data(1).Inputs(i).Channels(ch).Id'img);

               PPETP.API.Connect_To_Peer(Session   => Session,
                                         Peer_Addr => Data(1).Inputs(i).Addr,
                                         Port      => Data(1).Inputs(i).Port,
                                         Channel   => Data(1).Inputs(i).Channels(ch).Id,
                                         PeerID    => Data(1).Inputs(i).Id);
            end loop;
         end loop;
      end if;

   end XML_Configure;



   ----------------------
   -- Set_Sequence_Num --
   ----------------------
   procedure Set_Sequence_Num(Session  : in Session_ID;
                              StreamID : in Stream_ID;
                              Seq_Num  : in Data_Sequence_Number) is
   begin

      Put_Line(" EXTERNAL API -> Set_Sequence_Num");

      PPETP.API.Set_Sequence_Num(Session  => Session,
                                 StreamID => StreamID,
                                 Seq_Num  => Seq_Num);
   end Set_Sequence_Num;



   --
   -----------------------------------------------------------------------------
   --


   --------------
   -- Add_Peer --
   --------------
   procedure Add_Peer (Session: in     Session_ID;
                       Peer:    in     Peer_Type;
                       Result:     out Boolean) is
   begin

      Put_Line(" EXTERNAL API -> Add_Peer");

      Result := True;

      PPETP.API.Add_Peer (Session  => Session,
                          PeerID   => Get_Peer_PeerID(Peer),
                          Address  => Get_Peer_Addr(Peer),
                          PeerKind => Get_Peer_Kind(Peer),
                          Peer_Cred    => Get_Peer_Credential(Peer),
                          Puncturing   => Get_Peer_Puncturing(Peer),
                          Routing_Prob => Get_Peer_Routing(Peer),
                          Result   => Result);
   end Add_Peer;

   -----------------
   -- Remove_Peer --
   -----------------
   procedure Remove_Peer (Session: in     Session_ID;
                          PeerID : in     Peer_ID;
                          Result:  in out Boolean) is
   begin

      Put_Line(" EXTERNAL API -> Remove_Peer");

      PPETP.API.Delete_Peer (Session => Session,
                             PeerID  => PeerID,
                             Result  => Result);
   end Remove_Peer;




   --
   -----------------------------------------------------------------------------
   --

   ----------------
   -- Get_PeerID --
   ----------------
   function Get_PeerID (Session : in Session_ID) return Peer_ID is
   begin

      Put_Line(" EXTERNAL API -> Get_PeerID");

      return PPETP.API.Get_PeerID(Session => Session);
   end Get_PeerID;

   -----------------
   -- Get_Address --
   -----------------
   function Get_Address (Session : in Session_ID) return Inet_Addr_Type is
   begin
      Put_Line(" EXTERNAL API -> Get_Address");
      return PPETP.API.Get_Address(Session => Session);
   end Get_Address;


   --------------
   -- Get_Port --
   --------------
   function Get_Port (Session : in Session_ID) return Port_Type is
   begin
      Put_Line(" EXTERNAL API -> Get_Port");
      return PPETP.API.Get_Port(Session => Session);
   end Get_Port;


   --
   -----------------------------------------------------------------------------
   --

   ----------
   -- Send --
   ----------
   procedure Send (Session   : Session_ID;
                   StreamID  : PPETP.Stream_ID;
                   Data      : Stream_Element_Array) is
   begin
      PPETP.API.Send(Session  => Session,
                     StreamID => StreamID,
                     Data     => Data);
   end Send;
   pragma Inline (Send);

   ----------
   -- Recv --
   ----------
   procedure Recv (Session   : in     Session_ID;
                   StreamID  :    out PPETP.Stream_ID;
                   Data      :    out Stream_Element_Array;
                   Last      :    out Stream_Element_Offset) is
   begin
      PPETP.API.Recv(Session  => Session,
                     StreamID => StreamID,
                     Data     => Data,
                     Last     => Last);
   end Recv;
   pragma Inline (Recv);



   -- Routed Packets
   --****************************     TODO      ********************************


   -----------------------
   -- Send_Routed_Hello --
   -----------------------
   procedure Send_Routed_Hello(Session:    in Session_ID;
                               Target :    in Peer_ID;

                               Credential: in Byte_Array_Pt := null;

                               Ack_Received: out Boolean;
                               Ack_Response: out ACK_Motivation
                              ) is

      Mbox : PPETP_Mailboxes.Mailbox_Access := new PPETP_Mailboxes.Mailbox;
      OK :   PPETP_Mail_Ack_Type;

      Cred: Access_Attribute_Class := null;

      Attr : Attributes_Record;
   begin

      -- Creating Attributes
      if Credential /= null then
         Attr.Peer_Credential := new PEER_CREDENTIAL_Attribute(Credential'length);
         Set_Attribute(Object => PEER_CREDENTIAL_Attribute(Attr.Peer_Credential.all),
                       Data   => Credential.all);
      end if;


      PPETP.API.Send_Routed_Packet(Session    => Session,
                                   Target_ID  => Target,
                                   Command    => Hello,
                                   Channel    => 0,   -- Set to 0, but not used in Hello Pkt
                                   Attributes => Attr,
                                   Mbox       => Mbox);

      -- Waiting for the ACK
      Mbox.Wait(OK);

      Ack_Received := OK.Received;
      Ack_Response := OK.Reason;

   end Send_Routed_Hello;


   -----------------------
   -- Send_Routed_Start --
   -----------------------
   procedure Send_Routed_Start(Session: in Session_ID;
                               Target : in Peer_ID;

                               Channel: in PPETP.Channel_ID;

                               New_Peer_Address: in Sock_Addr_Type;
                               New_Peer_PeerID:  in Peer_ID;

                               Credential:   in Byte_Array_Pt := null;

                               Punct_Info:   in Puncturing_Info;

                               Routing_Info: in Puncturing_Info;

                               Ack_Received: out Boolean;
                               Ack_Response: out ACK_Motivation
                              ) is

      Mbox : PPETP_Mailboxes.Mailbox_Access := new PPETP_Mailboxes.Mailbox;
      OK :   PPETP_Mail_Ack_Type;


      Attr : Attributes_Record;
   begin

      -- Creating Attributes

      Attr.New_Peer := new NEW_PEER_Attribute;
      Set_Attribute(Object     => NEW_PEER_Attribute(Attr.New_Peer.all),
                    Address    => New_Peer_Address,
                    T_Protocol => 17,
                    PeerID     => New_Peer_PeerID);


      -- Not Mandatory
      if Credential /= null then
         Attr.Peer_Credential := new PEER_CREDENTIAL_Attribute(Credential'length);
         Set_Attribute(Object => PEER_CREDENTIAL_Attribute(Attr.Peer_Credential.all),
                       Data   => Credential.all);
      end if;

      if Punct_Info.Mode /= Null_Puncturing then
         Attr.Puncturing := new PUNCTURING_Attribute(Punct_Info.Mode);
         Set_Attribute(Object => PUNCTURING_Attribute(Attr.Puncturing.all),
                       Data   => Punct_Info);
      end if;

      if Routing_Info.Mode /= Null_Puncturing then
         -- Routing MUST be Probabilistic

         if Routing_Info.Mode /= Probabilistic then
            raise Program_Error;
         end if;

         Attr.Routing := new ROUTING_PROBABILITY_Attribute;
         Set_Attribute(Object => ROUTING_PROBABILITY_Attribute(Attr.Routing.all),
                       Num    => Routing_Info.Num,
                       Den    => Routing_Info.Den);
      end if;


      PPETP.API.Send_Routed_Packet(Session    => Session,
                                   Target_ID  => Target,
                                   Command    => Start,
                                   Channel    => Channel,
                                   Attributes => Attr,
                                   Mbox       => Mbox);

      -- Waiting for the ACK
      Mbox.Wait(OK);

      Ack_Received := OK.Received;
      Ack_Response := OK.Reason;

   end Send_Routed_Start;


   ----------------------
   -- Send_Routed_Stop --
   ----------------------
   procedure Send_Routed_Stop(Session: in Session_ID;
                              Target : in Peer_ID;

                              Channel: in PPETP.Channel_ID;

                              Old_Peer_Address: in Sock_Addr_Type;
                              Old_Peer_PeerID:  in Peer_ID;

                              Ack_Received: out Boolean;
                              Ack_Response: out ACK_Motivation
                             ) is

      Mbox : PPETP_Mailboxes.Mailbox_Access := new PPETP_Mailboxes.Mailbox;
      OK :   PPETP_Mail_Ack_Type;

      Attr : Attributes_Record;
   begin


      -- Creating Attributes
      Attr.Old_Peer := new OLD_PEER_Attribute;
      Set_Attribute(Object     => OLD_PEER_Attribute(Attr.Old_Peer.all),
                    Address    => Old_Peer_Address,
                    T_Protocol => 17,
                    PeerID     => Old_Peer_PeerID);


      PPETP.API.Send_Routed_Packet(Session    => Session,
                                   Target_ID  => Target,
                                   Command    => Stop,
                                   Channel    => Channel,
                                   Attributes => Attr,
                                   Mbox       => Mbox);

      -- Waiting for the ACK
      Mbox.Wait(OK);

      Ack_Received := OK.Received;
      Ack_Response := OK.Reason;

   end Send_Routed_Stop;


      --************     TODO      ****************************
   -----------------------
   -- Send_Routed_Punch --
   -----------------------
   procedure Send_Routed_Punch(Session: in Session_ID;
                               Target : in Peer_ID;

                               --* TODO cosa mettere  di questi quattro parametri???
                               NAT_Method:   in Byte;
                               NAT_Param:    in Byte := 0;
                               Start_Too:    in Boolean;
                               Redirect_Too: in Boolean;

                               Channel: in PPETP.Channel_ID;

                               New_Peer_Address:    in Sock_Addr_Type := No_Sock_Addr;
                               New_Peer_PeerID:     in Peer_ID := No_Peer_ID;

                               Credential:   in Byte_Array_Pt := null;

                               Punct_Info:   in Puncturing_Info;

                               Routing_Info: in Puncturing_Info;

                               Ack_Received: out Boolean;
                               Ack_Response: out ACK_Motivation
                              ) is

      Mbox : PPETP_Mailboxes.Mailbox_Access := new PPETP_Mailboxes.Mailbox;
      OK :   PPETP_Mail_Ack_Type;

      Attr : Attributes_Record;
   begin

      -- Creating Attributes

      -- Not Mandatory
      if Credential /= null then
         Attr.Peer_Credential := new PEER_CREDENTIAL_Attribute(Credential'length);
         Set_Attribute(Object => PEER_CREDENTIAL_Attribute(Attr.Peer_Credential.all),
                       Data   => Credential.all);
      end if;

      if Punct_Info.Mode /= Null_Puncturing then
         Attr.Puncturing := new PUNCTURING_Attribute(Punct_Info.Mode);
         Set_Attribute(Object => PUNCTURING_Attribute(Attr.Puncturing.all),
                       Data   => Punct_Info);
      end if;

      if Routing_Info.Mode /= Null_Puncturing then
         -- Routing MUST be Probabilistic

         if Routing_Info.Mode /= Probabilistic then
            raise Program_Error;
         end if;

         Attr.Routing := new ROUTING_PROBABILITY_Attribute;
         Set_Attribute(Object => ROUTING_PROBABILITY_Attribute(Attr.Routing.all),
                       Num    => Routing_Info.Num,
                       Den    => Routing_Info.Den);
      end if;


      --* Creare il NAT_Parameter Attribute!!!

      PPETP.API.Send_Routed_Packet(Session    => Session,
                                   Target_ID  => Target,
                                   Command    => Punch,
                                   Channel    => Channel,
                                   Attributes => Attr,
                                   Mbox       => Mbox);

      -- Waiting for the ACK
      Mbox.Wait(OK);

      Ack_Received := OK.Received;
      Ack_Response := OK.Reason;

   end Send_Routed_Punch;



   --------------------------
   -- Send_Routed_Redirect --
   --------------------------
   procedure Send_Routed_Redirect(Session: in Session_ID;
                                  Target : in Peer_ID;

                                  Channel: in PPETP.Channel_ID;

                                  New_Peer_Address:    in Sock_Addr_Type;
                                  New_Peer_PeerID:     in Peer_ID;

                                  Old_Peer_Address: in Sock_Addr_Type;
                                  Old_Peer_PeerID:  in Peer_ID;

                                  Credential:   in Byte_Array_Pt := null;

                                  Punct_Info:   in Puncturing_Info;

                                  Routing_Info: in Puncturing_Info;

                                  Ack_Received: out Boolean;
                                  Ack_Response: out ACK_Motivation
                                 ) is

      Mbox : PPETP_Mailboxes.Mailbox_Access := new PPETP_Mailboxes.Mailbox;
      OK :   PPETP_Mail_Ack_Type;

      Attr : Attributes_Record;
   begin


      -- Creating Attributes

      Attr.New_Peer := new NEW_PEER_Attribute;
      Set_Attribute(Object     => NEW_PEER_Attribute(Attr.New_Peer.all),
                    Address    => New_Peer_Address,
                    T_Protocol => 17,
                    PeerID     => New_Peer_PeerID);

      Attr.Old_Peer := new OLD_PEER_Attribute;
      Set_Attribute(Object     => OLD_PEER_Attribute(Attr.Old_Peer.all),
                    Address    => Old_Peer_Address,
                    T_Protocol => 17,
                    PeerID     => Old_Peer_PeerID);


      -- Not Mandatory
      if Credential /= null then
         Attr.Peer_Credential := new PEER_CREDENTIAL_Attribute(Credential'length);
         Set_Attribute(Object => PEER_CREDENTIAL_Attribute(Attr.Peer_Credential.all),
                       Data   => Credential.all);
      end if;

      if Punct_Info.Mode /= Null_Puncturing then
         Attr.Puncturing := new PUNCTURING_Attribute(Punct_Info.Mode);
         Set_Attribute(Object => PUNCTURING_Attribute(Attr.Puncturing.all),
                       Data   => Punct_Info);
      end if;

      if Routing_Info.Mode /= Null_Puncturing then
         -- Routing MUST be Probabilistic

         if Routing_Info.Mode /= Probabilistic then
            raise Program_Error;
         end if;

         Attr.Routing := new ROUTING_PROBABILITY_Attribute;
         Set_Attribute(Object => ROUTING_PROBABILITY_Attribute(Attr.Routing.all),
                       Num    => Routing_Info.Num,
                       Den    => Routing_Info.Den);
      end if;


      PPETP.API.Send_Routed_Packet(Session    => Session,
                                   Target_ID  => Target,
                                   Command    => Redirect,
                                   Channel    => Channel,
                                   Attributes => Attr,
                                   Mbox       => Mbox);


      -- Waiting for the ACK
      Mbox.Wait(OK);

      Ack_Received := OK.Received;
      Ack_Response := OK.Reason;

   end Send_Routed_Redirect;







end PPETP.External_API;
