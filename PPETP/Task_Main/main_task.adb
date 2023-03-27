with Ada.Text_IO;	use Ada.Text_IO;
with Ada.Exceptions;	use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Indefinite_Ordered_Maps;

with Control_Task;
with Control_Task.Commands;	use Control_Task.Commands;

with Packets.Internal;		use Packets.Internal;
with Packets.Protocol.Data;	use Packets.Protocol.Data;
with Packets.Protocol.Command;	use Packets.Protocol.Command;
with Packets.Binary.Application; use Packets.Binary.Application;

with Profiles;
with Profiles.Disentanglers;	use Profiles.Disentanglers;
with Profiles.Entangled;	use Profiles.Entangled;
with Profiles.Builders;		use Profiles.Builders;
with Profiles.Parameters;	use Profiles.Parameters;
with Profiles_Utility;		use Profiles_Utility;

with Network; 			use Network;

with PPETP_Mailboxes;
with PPETP_Mail;		use PPETP_Mail;
with PPETP;			use PPETP;

with PPETP.Channels;
with New_Integer_Lists_3;
with byte_arrays;		use byte_arrays;


with PPETP.Attributes.Old_Peer;		use PPETP.Attributes.Old_Peer;
with PPETP.Attributes.New_Peer;		use PPETP.Attributes.New_Peer;

with Peer_Manager;			use Peer_Manager;
with Peer_Informations;			use Peer_Informations;
with Command_Manager;			use Command_Manager;

with Packets.Internal;			use Packets.Internal;

--for debug only
with Profiles.Parameters.Vandermonde;	use Profiles.Parameters.Vandermonde;
with Galois_Field;			use Galois_Field;

with PPETP.Attributes;			use PPETP.Attributes;
with PPETP.Attributes.Routing_Probability;	use PPETP.Attributes.Routing_Probability;


package body Main_Task is

   package Sequence_Num_List is
     new New_Integer_Lists_3(Item_Type => PPETP.Data_Sequence_Number);


   use Sequence_Num_List;

   package Data_Sequence_Num_List is
     new Ada.Containers.Indefinite_Ordered_Maps(Key_Type     => Stream_ID,
                                                Element_Type => Sequence_Num_List.New_Integer_List_3_pt);


   ----------
   -- Main --
   ----------

   task body Handler is
      Request : Internal_Command_Pt;

      -- Creazione del disentangler in base al profilo
      Disentangler : Disentangler_Class_Pt;


      Commands     : Command_Queues.Queue_Pt;
      Transmitter  : Transmitters.Handler_Pt;
      Control      : Control_Task.Task_Pt;
      Profile      : Profiles.Profile_Type;
      Again        : Boolean := False;
      Data_Ready_Queue : Application_Packet_Queues.Queue_Pt;

      Received_Packets_List : Data_Sequence_Num_List.Map;

      Peer_Manag    : Peer_Manager_pt;
      Command_Manag : Command_Manager_Handler;


      M_Max_Sup:    Natural;   -- Data duplicate control


      --------------------------------------------------------------------------
      task type Send_Routed_Packets_Task is
         entry Send_Routed_Command(My_Addr:     in Network.Sock_Addr_Type;
                                   Target_ID  : in Peer_ID;
                                   Command    : in Command_Class;
                                   Channel    : in Channel_ID;
                                   Attributes : in Attributes_Record;
                                   Mbox :       in PPETP_Mailboxes.Mailbox_Access);

      end Send_Routed_Packets_Task;

      task body Send_Routed_Packets_Task is

      begin
         select
            accept Send_Routed_Command(My_Addr:     in Network.Sock_Addr_Type;
                                       Target_ID  : in Peer_ID;
                                       Command    : in Command_Class;
                                       Channel    : in Channel_ID;
                                       Attributes : in Attributes_Record;
                                       Mbox :       in PPETP_Mailboxes.Mailbox_Access) do


               case Command is
                  when Hello =>

                     Send_Hello_Command(Control         => Control,
                                        Dst_Peer        => My_Addr,

                                        Routed          => True,
                                        Target_ID       => Target_ID,

                                        Hello_Peer_Cred	=> Attributes.Peer_Credential,

                                        Mbox            => Mbox);
                  when Start =>

                     Send_Start_Command(Control         => Control,
                                        Dst_Peer        => My_Addr,

                                        Routed          => True,
                                        Target_ID       => Target_ID,

                                        Start_Channel      => Channel,
                                        Start_New_Peer     => Attributes.New_Peer,
                                        Start_Peer_Cred    => Attributes.Peer_Credential,
                                        Start_Puncturing   => Attributes.Puncturing,
                                        Start_Routing_Prob => Attributes.Routing,

                                        Mbox            => Mbox);
                  when Stop =>

                     Send_Stop_Command(Control         => Control,
                                       Dst_Peer        => My_Addr,

                                       Routed          => True,
                                       Target_ID       => Target_ID,

                                       Stop_Channel    => Channel,
                                       Stop_Old_Peer   => Attributes.Old_Peer,

                                       Mbox            => Mbox);
                  when Punch =>

                     null;
--                       Send_Punch_Command(Control         => Control,
--                                          Dst_Peer        => My_Addr,
--
--                                          Routed          => True,
--                                          Target_ID	    => Target_ID,
--
--                                          NAT_Method      : Byte;
--                                          NAT_Param       : Byte := 0;
--                                          Start_Too       : Boolean;
--                                          Redirect_Too    : Boolean;
--                                          Punch_NAT_Attr  => Attributes.Nat_Parameter,
--
--                                          -- Used only if the Start_Too flag is true
--                                          Punch_Channel      => Channel,
--                                          Punch_New_Peer     => Attributes.New_Peer,
--                                          Punch_Peer_Cred    => Attributes.Peer_Credential,
--                                          Punch_Puncturing   => Attributes.Puncturing,
--                                          Punch_Routing_Prob => Attributes.Routing_Probability,
--
--                                          Mbox            => Mbox);
                  when Redirect =>

                     Send_Redirect_Command(Control         => Control,
                                           Dst_Peer        => My_Addr,

                                           Routed          => True,
                                           Target_ID       => Target_ID,

                                           Redir_Channel      => Channel,
                                           Redir_New_Peer     => Attributes.New_Peer,
                                           Redir_Old_Peer     => Attributes.Old_Peer,
                                           Redir_Peer_Cred    => Attributes.Peer_Credential,
                                           Redir_Puncturing   => Attributes.Puncturing,
                                           Redir_Routing_Prob => Attributes.Routing,

                                           Mbox            => Mbox);
                  when others =>
                     raise Program_Error;

               end case;


            end Send_Routed_Command;
         end select;

      end Send_Routed_Packets_Task;


      type Send_Routed_Packets_Task_Ptr is access Send_Routed_Packets_Task;

      --------------------------------------------------------------------------

      task type Send_Hello_Task is
         entry Start(Dst_Peer: Peer_ID);
         entry Start_Routed(PeerID  : Peer_ID;
                            My_Addr : Sock_Addr_Type);
      end Send_Hello_Task;


      task body Send_Hello_Task is
         use PPETP_Mailboxes;

         Mbox : PPETP_Mailboxes.Mailbox_Access := new PPETP_Mailboxes.Mailbox;

         OK : PPETP_Mail_Ack_Type;
      begin

         Put_Line("Send_Hello_Task:     Alive");

         select
            accept Start(Dst_Peer: Peer_ID) do

               Put_Line("Send_Hello_Task: Send_Command");
               Put_Line("   to Peer_ID: " & Dst_Peer'img & " [" & Image(Get_Addr(PM     => Peer_Manag,
                                                                                 PeerID => Dst_Peer)) & "]");

               -- Send a HELLO to the peer and wait for ACK
               Send_Hello_Command(Control  => Control,
                                  Dst_Peer => Get_Addr(PM     => Peer_Manag,
                                                       PeerID => Dst_Peer),
                                  Mbox     => Mbox);

            end Start;
         or
            accept Start_Routed(PeerID  : Peer_ID;
                                My_Addr : Sock_Addr_Type) do

               Put_Line("Send_Hello_Task Routed: Send_Command");
               Put_Line("   to Peer_ID: " & PeerID'img);

               Send_Hello_Command(Control  => Control,
                                  Dst_Peer => My_Addr,

                                  Routed    => True,
                                  Target_ID => PeerID,

                                  Mbox     => Mbox);
            end Start_Routed;
         end select;


         Mbox.Wait(OK);
         --* TODO     vedere se è un ACK o NACK... serve?
         --*          l'Hello dovrebbe avere solo positive ACK

         if OK.Received then
            Put_Line("Send_Hello_Task: ACK Received");
         end if;

         Put_Line("****  Send_Hello_Task: Closed sucessfully  *****");

      exception
         when e: others =>
            Put_Line("Main Task: Send_Hello_Task dead!!");
            Put_Line(Exception_Information(e));

      end Send_Hello_Task;


      type Send_Hello_Task_Ptr is access Send_Hello_Task;



      --------------------------------------------------------------------------

      task type Send_Set_Default_Task is
         entry Start(New_Peer_Addr: Peer_ID;
                     Ch:            PPETP.Channel_ID;
                     Ack_To : 	    Network.Sock_Addr_Type;
                     Start_Sequence_Num:     PPETP.Command_Sequence_Number;
                     Start_Sub_Sequence_Num: PPETP.Sub_Sequence_Number);
      end Send_Set_Default_Task;


      task body Send_Set_Default_Task is
         Mbox :        PPETP_Mailboxes.Mailbox_Access := new PPETP_Mailboxes.Mailbox;
         OK :          PPETP_Mail_Ack_Type;
         Dest:         Peer_ID;
         Sequence_Num: PPETP.Command_Sequence_Number;
         Sub_Sequence_Num: PPETP.Sub_Sequence_Number;
         Channel:      PPETP.Channel_ID;
         Ack_To_Addr:         Network.Sock_Addr_Type;
         Builder: Builder_Class_Pt := New_Builder(Profile);

         Default: Byte_Array_Pt;

      begin
         accept Start(New_Peer_Addr: Peer_ID;
                      Ch:            PPETP.Channel_ID;
                      Ack_To : 	     Network.Sock_Addr_Type;
                      Start_Sequence_Num: PPETP.Command_Sequence_Number;
                      Start_Sub_Sequence_Num: PPETP.Sub_Sequence_Number) do

            declare
               Param:   Parameters_Class_Pt;
               Canale : PPETP.Channels.Channel;
            begin
               Dest         := New_Peer_Addr;
               Channel      := Ch;
               Sequence_Num     := Start_Sequence_Num;
               Sub_Sequence_Num := Start_Sub_Sequence_Num;
               Ack_To_Addr  := Ack_To;

               --* TODO
               --*      Attenzione POTREBBE esserci un baco da qualche parte,
               --*      nella restituzione del canale. Sarebbe meglio controllare
               Transmitter.Get_Channel(Channel => Ch,
                                       Result  => Canale);

               Param := PPETP.Channels.Get_Parameters(Canale.all );

               Put_Line("Send_Set_Default: ");
               Put_Line("              Ch: " & Ch'img);

               --Put_Line("              RF: " & Vandermonde_Par(Param.all).Reduction_Factor'img);
               --Put_Line("              GE: " & Image(Vandermonde_Par(Param.all).Galois_Element));
               --Put_Line("              GS: " & Vandermonde_Par(Param.all).GF_Size'img);

               Format_Parameters(Handler    => Builder.all ,
                                 Parameters => Param,
                                 Result     => Default);

               Put_Line("Send_Set_Default: Send Packet");
               Send_Set_Default_Command(Control         => Control,
                                        Dst_Peer        => Get_Addr(PM     => Peer_Manag,
                                                                    PeerID => Dest),
                                        Channel         => Ch,
                                        Default         => Default,
                                        Mbox            => Mbox);

            end;

         end Start;


         Mbox.Wait(OK);
         --* TODO controllare se ACK o NACK

         if OK.Received then
            Put_Line("Send_Set_Default: ACK received: Add Target");

            Put_Line("     New Target: ");
            Put_Line("         Ch: "   & Channel'img);
            Put_Line("         Addr: " & image(Get_Addr(PM     => Peer_Manag,
                                                        PeerID => Dest)) );

            declare
               Target : PPETP.Target_ID;
            begin

               Transmitter.New_Target (Channel => Channel,
                                       Address => Get_Addr(PM     => Peer_Manag,
                                                           PeerID => Dest),
                                       Target  => Target,
                                       Punct   => Get_Puncturing(PM     => Peer_Manag,
                                                                 PeerID => Dest)
                                      );

               Transmitter.Target_Ready(Channel => Channel,
                                        Target  => Target);
            exception
               when PPETP.Channels.Target_Yet_Present =>
                  Put_Line("Target yet present");
            end;


            Put_Line("Main: Send_ACK: ");
            Put_Line("  to:  " & Image(Ack_To_Addr));
            Put_Line("  Seq_Num to ack: " & Sequence_Num'img);

            Send_Ack_Command(Control            => Control,
                             Dst_Peer           => Ack_To_Addr,
                             Sequence_Num_ACKed     => Sequence_Num,
                             Sub_Sequence_Num_ACKed => Sub_Sequence_Num,
                             Mbox               => null);


            --*TODO
            --* Aggiungere le informazioni (es. routing_probability) del peer in un DB


         else
            Put_Line("Send_Set_Default: ACK Not received");

            Put_Line("Main: Send_NACK: ");
            Put_Line("  to:  " & Image(Ack_To_Addr));
            Put_Line("  Seq_Num to ack: " & Sequence_Num'img);


            declare
               Result : Boolean;
            begin

               Remove_Peer(PM     => Peer_Manag,
                           PeerID => Dest,
                           Result => Result);
               if (not Result) then
                  Put_Line("Main: Unable to remove the Peer from the Peer_Manager");
               end if;

            end;

            Send_NAck_Command(Control             => Control,
                              Dst_Peer            => Ack_To_Addr,
                              Sequence_Num_NACKed     => Sequence_Num,
                              Sub_Sequence_Num_NACKed => Sub_Sequence_Num,
                              NAcked_Reason	  => No_Replay,
                              Mbox                => null);

         end if;


         Command_Manag.Ack_Sent(PeerID  => Dest,
                                Seq_Num => Sequence_Num);

         Free(Default);


         Free(Builder);


      exception
         when e: others =>
            Put_Line("Main Task: Send_Set_Default Task dead!!");
            Put_Line(Exception_Information(e));
      end Send_Set_Default_Task;


      type Send_Set_Default_Task_Ptr is access Send_Set_Default_Task;

      --------------------------------------------------------------------------

      --
      -- Here, there are pointers!!!!
      --

      Hello_Task         : Send_Hello_Task_Ptr;
      Set_Def_Task       : Send_Set_Default_Task_Ptr;




      --------------------------------------------------------------------------


      -------------------------
      -- Send_Hello_And_Wait --
      -------------------------
      procedure Send_Hello_And_Wait(To: in Peer_ID) is

      begin
--         Put_Line("****  Send_Hello_Task: Created  *****");
         Hello_Task := new Send_Hello_Task;
         Hello_Task.Start(To);

      end Send_Hello_And_Wait;

      procedure Send_Hello_And_Wait(To:      in Peer_ID;
                                    My_Addr: in Sock_Addr_Type) is

      begin
--         Put_Line("****  Send_Hello_Task: Created  *****");
         Hello_Task := new Send_Hello_Task;
         Hello_Task.Start_Routed(To, My_Addr);

      end Send_Hello_And_Wait;


      -------------------------------
      -- Send_Set_Default_And_Wait --
      -------------------------------
      procedure Send_Set_Default_And_Wait(New_Peer_Addr: Peer_ID;
                                          Ch:            PPETP.Channel_ID;
                                          Ack_To : 	 Network.Sock_Addr_Type;
                                          Start_Sequence_Num:     PPETP.Command_Sequence_Number;
                                          Start_Sub_Sequence_Num: PPETP.Sub_Sequence_Number) is
      begin
         Set_Def_Task := new Send_Set_Default_Task;
         Set_Def_Task.Start(New_Peer_Addr, Ch, Ack_To, Start_Sequence_Num, Start_Sub_Sequence_Num);
      end Send_Set_Default_And_Wait;



      procedure Send_Routed_Packets_And_Wait(My_Addr:     in Network.Sock_Addr_Type;
                                             Target_ID  : in Peer_ID;
                                             Command    : in Command_Class;
                                             Channel    : in Channel_ID;
                                             Attributes : in Attributes_Record;
                                             Mbox :       in PPETP_Mailboxes.Mailbox_Access) is

         Send_Rout_Pkts_Task : Send_Routed_Packets_Task_Ptr;
      begin

         Send_Rout_Pkts_Task := new Send_Routed_Packets_Task;
         Send_Rout_Pkts_Task.Send_Routed_Command(My_Addr    => My_Addr,
                                                 Target_ID  => Target_ID,
                                                 Command    => Command,
                                                 Channel    => Channel,
                                                 Attributes => Attributes,
                                                 Mbox       => Mbox);
      end Send_Routed_Packets_And_Wait;



      --------------------------------------------------------------------------
      --------------------------------------------------------------------------
      --------------------------------------------------------------------------

      ------------------
      -- Process_Data --
      ------------------
      procedure Process_Data (Peer         : PPETP.Peer_ID;
                              StreamID     : PPETP.Stream_ID;
                              Channel      : PPETP.Channel_ID;
                              Sequence_Num : PPETP.Data_Sequence_Number;
                              Payload      : in out Raw_Data
                             ) is

         Recovered : Queue_Element;

      begin

--           Put_Line("Main: Process Data: StreamID:" & StreamID'img & " Seq_Num: " & Sequence_Num'img &
--                    " Data Length: " & Payload.Data'Length'img);


--         Put_Line(Sequence_Num'img);
         if Received_Packets_List.Contains(Key => StreamID) then  -- If this stream exists

          --  Put_Line("Main: Stream Exist!");
            -- This Seq_Num is not duplicate
            if not Received_Packets_List.Element(Key => StreamID).Contains(Sequence_Num) then



--               Put_Line("Main: Data to Disentangler!");
               Disentangler.Process (Peer         => Peer,
                                     StreamID     => StreamID,
                                     Channel      => Channel,
                                     Sequence_Num => Sequence_Num,
                                     Packet       => Payload);


            else
--                 New_Line(2);
--                 Dump(Received_Packets_List.Element(Key => StreamID).all);
--                 New_Line(2);
--                 Put_Line("Main: Duplicate packet ID: " & StreamID'img & " Seq: " & Sequence_Num'img );
               Free(Payload.Data);
--               raise Program_Error;
            end if;

         else -- a new Stream_ID

            Put_Line("Main: New Data Stream_ID; create Sequence_Num List");
            Put_Line("  StreamID: " & StreamID'img);
            Put_Line("  First Seq Num: " & Sequence_Num'img);

            -- First create the Seq_Num list
            declare
               List : Sequence_Num_List.New_Integer_List_3_pt := new Sequence_Num_List.New_Integer_List_3;
            begin

               Sequence_Num_List.Initialize(List.all);
               List.Set_Sup_Limit(Data_Sequence_Number(M_Max_Sup));

               List.Insert(Item         => Sequence_Num-1,
                           On_Duplicate => Sequence_Num_List.Ignore);

               Received_Packets_List.Insert(Key      => StreamID,
                                            New_Item => List);
            end;



            Disentangler.Process (Peer         => Peer,
                                  StreamID     => StreamID,
                                  Channel      => Channel,
                                  Sequence_Num => Sequence_Num,
                                  Packet       => Payload);
         end if;


         while Disentangler.Any_Ready loop

            Disentangler.Get (Recovered);
--            Put_Line("Data Ready: ID: " & Recovered.StreamID'img & " Seq: " & Recovered.Sequence_Num'img );

            -- Send packet to the Transmitter and to the Output Queue. If the
            -- packet is an Entangled packet send it only to the Transmitter
            if Recovered.Entangled then

               -- Come ????
               raise Program_Error;
            else


--                 Put_Line("Main: Recovered.Timestamp: " & Recovered.Timestamp'img);
--                 Put_Line("Main: Recovered.Data Length: " & Recovered.Data.Data.all'length'img);

               declare

                  -- Se è un Binary contiene lo stesso un Raw_Data ma l'unica parte significativa è il Data
                  App_Pkt : Application_Packet := New_Packet (Sequence_Num => Recovered.Sequence_Num,
                                                              StreamID     => Recovered.StreamID,
                                                              Data         => Recovered.Data.Data.all);

               begin




                  -- Insert the new packet in the list
                  declare
                     List : Sequence_Num_List.New_Integer_List_3_pt;
                  begin
  --                   Put_Line("Main: Timestamp inserito");
                     List := Received_Packets_List.Element(Key => StreamID);
                     List.Insert(Item         => Recovered.Sequence_Num,
                                 On_Duplicate => Sequence_Num_List.Ignore);
                  end;
                  -- To Output Queue
--                  Put_Line("Main:  Data to queue");
                  Data_Ready_Queue.Insert( App_Pkt );
                  -- To Transmitter
--                Put_Line("Main:  Re-Send Data");
                  Transmitter.Transmit (App_Pkt);

               exception
                  when e: others =>
                     Put_Line(Exception_Information(e));
               end;



               Free (Recovered.Data.Data);

            end if;
         end loop;

      end Process_Data;



      ---------------------
      -- Process Command --
      ---------------------
      procedure Process_Command(Request : Internal_Command_Pt) is
      begin


         Put("Main received: " & Request.Class'img);
         Put(" : Seq Num:" & Request.Seq_Number'img);
         Put_Line(" [Sub: " & Request.Sub_Seq_Number'img & "]");

         -- ACK, NACK or Forward packets is not necessary elaboration. I can receive
         -- multiple copy without problems


         if ( Request.Class /= ACK and Request.Class /= Nack and Request.Class /= Forward) then

            declare
               Command_Act : Command_Action;
            begin

               --       if a packet is in execution dicard it
               --       if a packet is too old, discard it
               --       if a packet is been ACK-ed too much (but is not very old) discard it
               --       if a packet id been ACK-ed, little times, ACK it
               --       else execute the command



               -- if it is a Start packet the real Id is in the attribute New_Peer
               if Request.Class = Start then
                  Request.Id := Get_PeerID(NEW_PEER_Attribute(Request.Start_New_Peer.all));
               end if;


               Command_Act := Command_Manag.Process_Command(PeerID  => Request.Id,
                                              		Seq_Num => Request.Seq_Number);

               Put_Line("Command_Manger: " & Command_Act'img);

               if Command_Act = Packet_In_Execution then
                  Put_Line("******************   PACKET IN EXECUTION   ********************");
                  Put_Line("PeerID : " & Request.Id'img);
                  Put_Line("Seq_Num: " & Request.Seq_Number'img);
                  New_Line;
                  Command_Manag.Image;
                  Put_Line("***************************************************************");
               end if;


               case Command_Act is

                  -- Do nothing and discard the packet
                  when Packet_In_Execution | Packet_Too_Old | Packet_Too_Acked =>
                     return;

                  when Send_Ack =>
                     -- Send an ack packet

                     New_line(2);
                     Command_Manag.Image;
                     New_Line(2);

                     Send_Ack_Command(Control            => Control,
                                      Dst_Peer           => Get_Addr(PM     => Peer_Manag,
                                                                     PeerID => Request.Id),
                                      Sequence_Num_ACKed     => Request.Seq_Number,
                                      Sub_Sequence_Num_ACKed => Request.Sub_Seq_Number,
                                      Mbox               => null);

                     Command_Manag.Ack_Sent(PeerID  => Request.Id,
                                            Seq_Num => Request.Seq_Number);

                     return;

                  when Execute =>
                     Command_Manag.Set_Packet_In_Execution(PeerID  => Request.Id,
                                             	       Seq_Num => Request.Seq_Number);

               end case;

            end;

         end if;


         case Request.Class is



            when ACK =>
               Put_Line("  Acked Number: " & Request.Sequence_Num_ACKed'img &
                        " [Sub: " & Request.Sub_Seq_Num_ACKed'img);


               Control.ACK_Received(Sequence_Num => Request.Sequence_Num_ACKed,
                                    Reason       => OK);
            when Nack =>

               Put_Line("  NAcked Number: " & Request.Sequence_Num_NACKed'img &
                        " [Sub: " & Request.Sub_Seq_Num_NACKed'img);
               Put_Line("  Reason: " & Request.NAcked_Reason'img);


               Control.ACK_Received(Sequence_Num => Request.Sequence_Num_NACKed,
                                    Reason       => Request.NAcked_Reason);

            when Hello =>

               Put_Line("Main: Send_Ack: ");
               Put_Line("     Seq_Num to ack: " & Request.Seq_Number'img);


               Send_Ack_Command(Control            => Control,
                                Dst_Peer           => Request.Source,
                                Sequence_Num_ACKed     => Request.Seq_Number,
                                Sub_Sequence_Num_ACKed => Request.Sub_Seq_Number,
                                Mbox               => null);

               Command_Manag.Ack_Sent(PeerID  => Request.Id,
                                      Seq_Num => Request.Seq_Number);

            when Set_Default =>

               Put_Line("   Id: " & Request.Id'img);
               Put_Line("   Ch: " & Request.Chann_Def'img);


               Disentangler.Set_default(Request.Id, Request.Chann_Def, Request.Default);
               Free(Request.Default);

               Put_Line("Main: Send_Ack: ");
               Put_Line("     Seq_Num to ack: " & Request.Seq_Number'img);

               Send_Ack_Command(Control            => Control,
                                Dst_Peer           => Request.Source,
                                Sequence_Num_ACKed     => Request.Seq_Number,
                                Sub_Sequence_Num_ACKed => Request.Sub_Seq_Number,
                                Mbox               => null);

               Command_Manag.Ack_Sent(PeerID  => Request.Id,
                                      Seq_Num => Request.Seq_Number);

            when Start =>

               Put_Line("  New_Peer Addr : " & Network.Image(Get_Address(NEW_PEER_Attribute(Request.Start_New_Peer.all))) );
               Put_Line("  Ch : " & Channel_ID'Image(Request.Start_Channel));
               Put_Line("  PeerID : " & Get_PeerID(NEW_PEER_Attribute(Request.Start_New_Peer.all))'img);

               declare
                  Result : Boolean;

                  New_PeerID : peer_ID;
                  my_attr : Access_Attribute_Class := new ROUTING_PROBABILITY_Attribute;
               begin


                  --* TODO Controllare il canale !!!

                  New_PeerID := Get_PeerID(NEW_PEER_Attribute(Request.Start_New_Peer.all)) ;

                  --* TODO  this "if " is used only for my test... if you find it, Delete it
                  if New_PeerID = 1003 then
                     Put_Line("Main: Don't propagate messages to New_PeerID");
                     Set_Attribute(Object => ROUTING_PROBABILITY_Attribute(my_Attr.all),
                                   Num => 0,
                                   Den => 0);
                  else

                     Set_Attribute(Object => ROUTING_PROBABILITY_Attribute(my_Attr.all),
                                   Num => 1,
                                   Den => 0);
                  end if;

--                  Set_Attribute(Object     => NEW_PEER_Attribute(my_Attr.all),

                  -- A peer could be yet insert in the Peer_Manager, for example if it is
                  -- yet in an another channel; if not...
                  -- Add the new peer in the Peer_Manager, if the procedure fail
                  -- delete if (in the Send_Set_Default_And_Wait task)
                  if not Is_Present(PM     => Peer_Manag,
                                    PeerID => Get_PeerID(NEW_PEER_Attribute(Request.Start_New_Peer.all)) ) then

                     Put_Line("  Insert Peer in the Peer_Manager");
                     Add_Peer(PM           => Peer_Manag,
                              PeerID       => Get_PeerID(NEW_PEER_Attribute(
                                Request.Start_New_Peer.all)),
                              Address      => Get_Address(NEW_PEER_Attribute(
                                Request.Start_New_Peer.all)),
                              PeerKind     => Lower_Peer,
                              Peer_Cred    => Request.Start_Peer_Cred,
                              Puncturing   => Request.Start_Puncturing,
--                              Routing_Prob => my_attr,				--* Solo per prova
                              Routing_Prob => Request.Start_Routing_Prob,    --CORRETTO
                              Result       => Result);

                  else
                     Result := True;
                  end if;

                  --* TODO se è già nello stesso canale
                  declare
                     Ch: PPETP.Channels.Channel;
                  begin

                     Transmitter.Get_Channel(Channel => Request.Start_Channel,
                                             Result  => Ch);

                     if Ch.Target_Exist(Addr => Get_Address(NEW_PEER_Attribute(
                                        Request.Start_New_Peer.all))) then

                        Put_Line("   A Peer with this Address exists yet in this Channel");

                     end if;

                  end;


                  if Result then
                     -- Ack_To could be different from the New_Peer_Addr; if the ack is not received
                     -- remove the peer from the Targets and from the Peer_Manager
                     Send_Set_Default_And_Wait(New_Peer_Addr      => Get_PeerID(NEW_PEER_Attribute(
                                               					Request.Start_New_Peer.all)),
                                               Ch                 => Request.Start_Channel,
                                               Ack_To  	          => Request.Source,
                                               Start_Sequence_Num     => Request.Seq_Number,
                                               Start_Sub_Sequence_Num => Request.Sub_Seq_Number);
                  else
                     Put_Line("Main: Unable to Add the Peer in the Peer_Manager");

                     -- send a NACK Message
                     Send_NAck_Command(Control                 => Control,
                                       Dst_Peer                => Request.Source,
                                       Sequence_Num_NACKed     => Request.Seq_Number,
                                       Sub_Sequence_Num_NACKed => Request.Sub_Seq_Number,
                                       NAcked_Reason           => No_Resource,
                                       Mbox                    => null);

--                       Ack_Sent(CM      => Command_Manag,
--                                PeerID  => Request.Id,
--                                Seq_Num => Request.Seq_Number);

                  end if;
               end;


            when Stop =>

               Put_Line("  Ch: " & Channel_ID'Image(Request.Stop_Channel));
               Put_Line("  Addr: " & Network.Image(Get_Address(OLD_PEER_Attribute(Request.Stop_Old_Peer.all))) );



               declare
                  Found: Boolean;
                  Result : Boolean;
               begin


                  -- Remove from the Channel
                  Found := Transmitter.Delete_Target(Channel => Request.Stop_Channel,
                                                     Peer    => Get_Address(OLD_PEER_Attribute(
                                                       Request.Stop_Old_Peer.all)) );

                  -- Remove from the Peer_Manager
                  Remove_Peer(PM     => Peer_Manag,
                              PeerID => Get_PeerID(OLD_PEER_Attribute(
                                Request.Stop_Old_Peer.all)),
                              Result => Result);
                  if (not Result) then
                     Put_Line("Main: Unable to remove the Peer from the Peer_Manager");
                  end if;




                  if Found then
                     Put_Line("Main: Send_Ack: ");
                     Put_Line("  Seq_Num to ack: " & Request.Seq_Number'img &
                              " [sub: " & Request.Sub_Seq_Number'img & "]");

                     Send_Ack_Command(Control            => Control,
                                      Dst_Peer           => Request.Source,
                                      Sequence_Num_ACKed     => Request.Seq_Number,
                                      Sub_Sequence_Num_ACKed => Request.Sub_Seq_Number,
                                      Mbox               => null);


                  else
                     Put_Line("Main: Send_NACK: ");
                     Put_Line("  Seq_Num to ack: " & Request.Seq_Number'img);

                     Send_NAck_Command(Control             => Control,
                                       Dst_Peer            => Get_Addr(PM     => Peer_Manag,
                                                                       PeerID => Request.Id),
                                       Sequence_Num_NACKed     => Request.Seq_Number,
                                       Sub_Sequence_Num_NACKed => Request.Sub_Seq_Number,
                                       NAcked_Reason       => No_Target,
                                       Mbox                => null);


                  end if;

                  Command_Manag.Ack_Sent(PeerID  => Request.Id,
                                         Seq_Num => Request.Seq_Number);


               end;


            when Punch =>

               --*************************      TODO         *******************
               --*
               --*	Ancora da vedere come fare
               --*
               --***************************************************************

               --* Contattare ICE per ricevere Address e Porta corretti

               if Request.Start_Too then
                  Put_Line("   Punch & Start");
                  --* ???? Se si inserisse nella coda dei pacchetti uno start con gli
                  --*      opportuni parametri? (Timestamp, Indirizzo ottenuto con ICE ecc.?)
--                    Commands.Insert
--                      (new Internal_Command'
--                         (Class           => Start,
--                          Source          => No_Sock_Addr, --Source_Addr,  -- ?????
--                          Id              => Peer_ID(0), -- fake peer_id, because if i receive a Start
--                          -- i haven't the peer into Peer Manager
--
--                          Seq_Number      => Request.Seq_Number,
--                          Sub_Seq_Number  => Request.Sub_Seq_Number,
--
--                          Start_Channel      => Request.Punch_Channel,
--                          Start_New_Peer     => Request.Punch_New_Peer,         -- Modificare con indirizzo ICE
--                          Start_Peer_Cred    => Request.Punch_Peer_Cred,
--                          Start_Puncturing   => Request.Punch_Puncturing,
--                          Start_Routing_Prob => Request.Punch_Routing_Prob));

               elsif Request.Redirect_Too then
                  Put_Line("   Punch & Redirect");
                  --* ???? Se si inserisse nella coda dei pacchetti uno redirect con gli
                  --*      opportuni parametri? (Timestamp, Indirizzo ottenuto con ICE ecc.?)

--                    Commands.Insert
--                      (new Internal_Command'
--                         (Class	      => Redirect,
--                          Source	      => No_Sock_Addr, --Source_Addr,
--                          Id                 => Request.Id,
--
--                          Seq_Number         => Request.Seq_Number,
--                          Sub_Seq_Number     => Request.Sub_Seq_Number,
--
--                          Redir_Channel      => Request.Punch_Channel,
--                          Redir_New_Peer     => Request.Punch_New_Peer,
--                          Redir_Old_Peer     => Request.Punch_Old_Peer,
--                          Redir_Peer_Cred    => Request.Punch_Peer_Cred,
--                          Redir_Puncturing   => Request.Punch_Puncturing,
--                          Redir_Routing_Prob => Request.Punch_Routing_Prob));


               else
                  Put_Line("   Only Punch");
                  --* manda l'ACK ?
               end if;



               raise Program_Error;





            when Redirect =>

               --*************************   TODO   ****************************
               --*
               --*	Per adesso è solo uno stop seguito da uno start, in
               --*	seguito sarà da modificarlo
               --*
               --***************************************************************

               -- Stop Sending Data to the OLD Peer and start sending to NEW Peer


               declare
                  Found: Boolean;
                  Result : Boolean;
               begin

                  -- Remove from the Channel
                  Found := Transmitter.Delete_Target(Channel => Request.Stop_Channel,
                                                     Peer    => Get_Address(OLD_PEER_Attribute(
                                                       Request.Stop_Old_Peer.all)) );

                  -- Remove from the Peer_Manager
                  Remove_Peer(PM     => Peer_Manag,
                              PeerID => Get_PeerID(OLD_PEER_Attribute(
                                Request.Stop_Old_Peer.all)),
                              Result => Result);
                  if (not Result) then
                     Put_Line("Main: Unable to remove the Peer from the Peer_Manager");
                  end if;



                  -- A peer could be yet insert in the Peer_Manager, for example if it is
                  -- yet in an another channel; if not...
                  -- Add the new peer in the Peer_Manager, if the procedure fail
                  -- delete if (in the Send_Set_Default_And_Wait task)
                  if not Is_Present(PM     => Peer_Manag,
                                    PeerID => Get_PeerID(NEW_PEER_Attribute(Request.Start_New_Peer.all)) ) then

                     Put_Line("  Insert Peer in the Peer_Manager");
                     Add_Peer(PM           => Peer_Manag,
                              PeerID       => Get_PeerID(NEW_PEER_Attribute(
                                Request.Start_New_Peer.all)),
                              Address      => Get_Address(NEW_PEER_Attribute(
                                Request.Start_New_Peer.all)),
                              PeerKind     => Lower_Peer,
                              Peer_Cred    => Request.Start_Peer_Cred,
                              Puncturing   => Request.Start_Puncturing,
                              Routing_Prob => Request.Start_Routing_Prob,
                              Result       => Result);
                  end if;

                  --* TODO se è già nello stesso canale

                  declare
                     Ch: PPETP.Channels.Channel;
                  begin

                     Transmitter.Get_Channel(Channel => Request.Start_Channel,
                                             Result  => Ch);

                     if Ch.Target_Exist(Addr => Get_Address(NEW_PEER_Attribute(
                                        Request.Start_New_Peer.all))) then

                        Put_Line("   A Peer with this Address exists yet in this Channel");

                     end if;

                  end;


                  if Result then
                     -- Ack_To could be different from the New_Peer_Addr; if the ack is not received
                     -- remove the peer from the Targets and from the Peer_Manager
                     Send_Set_Default_And_Wait(New_Peer_Addr      => Get_PeerID(NEW_PEER_Attribute(
                                               Request.Start_New_Peer.all)),
                                               Ch                 => Request.Start_Channel,
                                               Ack_To  	          => Request.Source,
                                               Start_Sequence_Num     => Request.Seq_Number,
                                               Start_Sub_Sequence_Num => Request.Sub_Seq_Number);
                  else
                     Put_Line("Main: Unable to Add the Peer in the Peer_Manager");

                     -- send a NACK Message
                     Send_NAck_Command(Control                 => Control,
                                       Dst_Peer                => Request.Source,
                                       Sequence_Num_NACKed     => Request.Seq_Number,
                                       Sub_Sequence_Num_NACKed => Request.Sub_Seq_Number,
                                       NAcked_Reason           => No_Resource,
                                       Mbox                    => null);

                     Command_Manag.Ack_Sent(PeerID  => Request.Id,
                                            Seq_Num => Request.Seq_Number);
                  end if;

               end;

            when Forward =>

               New_Line;
               Put_Line("******** Peer Manager Info  *************");
               Image(Peer_Manag);
               Put_Line("*****************************************");
               New_Line(2);
               Command_Manag.Image;
               New_Line(2);


               Put_Line("Send Forward data to all Low Peer");


               Start_Iteration(PM => Peer_Manag);

               while Iterate_Again(PM => Peer_Manag) loop

                  declare
                     Addr: Sock_Addr_Type;
                     Tx : Boolean;
                  begin

                     Next_Iteration(PM      => Peer_Manag,
                                    Address => Addr,
                                    Tx      => Tx);
                     if Tx then
                        Put_line("Forward to: " & Image(Addr) );

                        Send_Forward_Command(Control  => Control,
                                             Dst_Peer => Addr,
                                             Data     => Request.Forward_Data);
                     end if;

                  end;
               end loop;




            when others =>
               Put_Line("Main received not Command Packet in Process_Command procedure");
               raise Program_Error;


         end case;

      end Process_Command;




   begin

      Init_Loop:
      while not Again loop
         select
            accept Set_Low_Level_Values(Max_Sup:        in Natural := 100;    -- Data duplicate control
                                        Re_ACK:         in Natural := 20;     -- Control duplicate control
                                        Max_Oldest:     in Natural := 50_000; -- Control duplicate control
                                        Max_ACK:        in Natural := 8)  do   -- Control duplicate control

               M_Max_Sup := Max_Sup;

               Command_Manag.Set_Re_Ack(Re_ACK);
               Command_Manag.Set_Max_Oldest(Max_Oldest);
               Command_Manag.Set_Max_ACK(Max_ACK);

               Again := False;

            end Set_Low_Level_Values;

         or

            accept Init (Queue       : Command_Queues.Queue_Pt;
                         Output      : Transmitters.Handler_Pt;
                         Ctl         : Control_Task.Task_Pt;
                         Prof_Type   : Profiles.Profile_Type;
                         Prof_Param  : Parameters_Class_Pt;
                         Ready_Queue : Application_Packet_Queues.Queue_Pt;
                         Peer_Man  : Peer_Manager.Peer_Manager_pt) do

               Commands     := Queue;
               Transmitter  := Output;
               Control      := Ctl;
               Profile      := Prof_Type;
               Data_Ready_Queue := Ready_Queue;
               Disentangler := New_Disentangler(Profile    => Profile,
                                                Parameters => Prof_Param);

               Peer_Manag := Peer_Man;

               Again := True;

            end Init;
         end select;

      end loop Init_Loop;



      Main_Loop:
      while Again loop
         select

            delay 0.1;

         then abort

            Commands.Extract (Request);

            case Request.Class is

               when Timeout =>
                  -- Signal_Timeout (Session => Request.Path.Session,
                  --                 Source  => Request.Path.Source);
                  --                  Put_Line("         PROGRAM ERROR");

                  raise Program_Error;

               when Invalid =>
                  null;


               when Data =>
--                  Put_Line("Main:  Received Data Packet:");
--                  Put_Line("                from:      " & image(Request.Source));
--                  Put_Line("   timestamp: " & Request.Data_Seq_Number'img);




                  Process_Data(Peer         => Request.Id,
                               StreamID     => Request.StreamID,
                               Channel      => Request.Sending_Ch,
                               Sequence_Num => Request.Data_Seq_Number,
                               Payload      => Request.Payload);

               when others =>   -- Commands

                  Process_Command(Request);


            end case;

            if Request.Class = Data then
               Free(Request.Payload.data);
            end if;


            Free(Request);

         end select;

         select
            accept Stop do
--               Put_line("Main Task:  Stop received!");


               declare
               begin
                  abort  Hello_Task.all; -- Non è bellissimo ma... a mali estremi...
                  abort  Set_Def_Task.all;
                  -- If Send_Task doesn't point to an active task, the abort raise an exception
               exception
                  when e: others =>
                     null;
               end;



               Again := False;

            end Stop;
         or
            accept Send_Routed_Command(My_Addr: Network.Sock_Addr_Type;
                                       Target_ID  : in Peer_ID;
                                       Command    : in Command_Class;
                                       Channel    : in Channel_ID;
                                       Attributes : in Attributes_Record;
                                       Mbox :       in PPETP_Mailboxes.Mailbox_Access) do


               -- if i send the packet directly to all low peer, i aspect
               -- more than one ACK, so this node send only one packet to itself.
               -- It is seen as a packet to be routed (forward) to other peer



               --********************   TODO    ***********************
               --* Prima di spedire il pacchetto devo inserire il peer
               --* di destinazione nel Peer_Manager, altrimenti non sono
               --* in grado di ricevere l'ack. forse lo deve fare l'applicazione
               --* o l'API
               --*
               --*
               --******************************************************



               Send_Routed_Packets_And_Wait(My_Addr    => My_Addr,
                                            Target_ID  => Target_ID,
                                            Command    => Command,
                                            Channel    => Channel,
                                            Attributes => Attributes,
                                            Mbox       => Mbox);

            end Send_Routed_Command;



         else
            null; -- se non c'è una entry in attesa esce subito dalla select
         end select;

--         Put_Line("Main:  Again: " & Again'img);
      end loop Main_Loop;
--      Put_Line("****  Main Task: Closed sucessfully  *****");

   exception
      when e: others =>
         Put_line("Main task dead!!!");
         Put_line(Exception_Information(e));
   end Handler;



   procedure Finalize (X : in out Task_Pt) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Handler, Task_Pt);
   begin
      X.Stop;
      Free (X);
   end Finalize;

end Main_Task;
