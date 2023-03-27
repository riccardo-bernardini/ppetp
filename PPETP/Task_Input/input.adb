with Ada.Text_IO;    		use Ada.Text_IO;
with Ada.Exceptions; 		use Ada.Exceptions;
with Ada.Calendar; 		use Ada.Calendar;
with Ada.Streams;               use Ada.Streams;
with Ada.Unchecked_Deallocation;

with Interfaces;		use Interfaces;

with Byte_Arrays;               use Byte_Arrays;
with Packets.Protocol;          use Packets.Protocol, Packets;
with Packets.Protocol.Parsing;
with Packets.Protocol.Command;
with Packets.Protocol.Data;
with Packets.Internal;
with Packets.Binary.Network;	use Packets.Binary.Network;

with Packets.Protocol.Command;	use Packets.Protocol.Command;
with Packets.Protocol.Command.Parsing;
with Timeout_Socket;


with Network;                   use Network;
with Command_Queues;            use Command_Queues;

with Generic_Shared_Queue;

with PPETP;

with Input.Internal_Packets;   use Input.Internal_Packets;
--with Input.Address_List;       use Input.Address_List;

with Peer_Manager;		use Peer_Manager;

with Network_Utilities;
with Packets.Protocol.Utilities;	use Packets.Protocol.Utilities;

with Parsing_Buffers;

with Other_Input_Applications;
with Other_Input_Applications.NULL_Application; use Other_Input_Applications.NULL_Application;
with Other_Input_Applications.PIPPO_Application; use Other_Input_Applications.PIPPO_Application;

package body Input is
   use type Ada.Calendar.Time;



   ------------
   -- Reader --
   ------------

   task body Reader is



      package Pkt_Buf is new Generic_Shared_Queue
        (Element => Internal_Packet_Pt);

      Internal_Queue : Pkt_Buf.Queue_Pt := Pkt_Buf.New_Queue;


      Socket_Ext : Network.Socket_Type := Input_Socket.all;
      Socket_Int : Network.Socket_Type := Inter_Proc_Socket.all;
--      Inter_Proc_Socket : Socket_Type := Inter_Proc_Socket.all;

      State  : Input_Task_State := Closed;

      PPETP_Ready : Boolean := False;


      --* TODO  Scriverlo come processo indipendente ?

      -----------------
      -- Send_Packet --
      -----------------
      procedure Send_Packet(Data: Byte_Array) is

      begin

--         Put_Line("Input->Send_Message");

         if Data(1) = 4 then --IPv4
            declare
               IP : Network_Utilities.Inet_Addr_V4_Buffer;
               Port : Bit_Field_16;
               Data_Length : Byte_Array_Offset := Data'Length - (1 + 4 + 2);
            begin
--               Put_Line("Input->Send_Message: IPv4");
--               Put_Line("Port(1): " & Data(6)'img);
--               Put_Line("Port(2): " & Data(7)'img);
               Port := Bit_Field_16((Natural(Data(6)) * 2**8 + Natural(Data(7))) mod 2**16);
--               Put_Line("Input->Send_Message: Port: " & Port'img);
               for i in  2 .. 5 loop
                  IP(i-1) := Interfaces.Unsigned_8(Data( Stream_Element_Offset(i)));
               end loop;

--               Put_Line("Input->Send_Message: Addr: " & Image(Network_Utilities.Inet_Addr(IP)));


               declare
                  Addr : Sock_Addr_Type := (Family => Family_Inet,
                                            Addr   => Network_Utilities.Inet_Addr(IP),
                                            Port   => Port_Type(Port));
                  Buffer : Stream_Element_Array := Stream_Element_Array(Data(8..Data'Length));
                  Last : Stream_Element_Offset := Buffer'Last;
               begin

--                  Put_Line("Input->Send_Message: Pkt size: " & Buffer'First'img & " " & Buffer'Last'img);


                  Send_Socket(Socket => Socket_Ext,
                              Item   => Buffer,
                              Last   => Last,
                              To     => Addr);

--                  Put_Line("Input->Send_Message: Pkt sent! ");

               end;

            end;

         else  --IPv6
            declare
               IP : Network_Utilities.Inet_Addr_V6_Buffer;
               Port : Bit_Field_16;
               Data_Length : Byte_Array_Offset := Data'Length - (1 + 16 + 2);
            begin
               Put_Line("Input. Send_Message: IPv6");
               Port := Bit_Field_16((Natural(Data(18)) * 2**8 + Natural(Data(19))) mod 2**16);
--               Put_Line("Input->Send_Message: Port: " & Port'img);
               for i in  2 .. 17 loop
                  IP(i-1) := Interfaces.Unsigned_8(Data( Stream_Element_Offset(i)));
               end loop;

               declare
                  Addr : Sock_Addr_Type := (Family =>Family_Inet6,
                                            Addr   => Network_Utilities.Inet_Addr(IP),
                                            Port   => Port_Type(Port));
                  Buffer : Stream_Element_Array := Stream_Element_Array(Data(20..Data'Length));
                  Last : Stream_Element_Offset := Buffer'Last;
               begin




                  Send_Socket(Socket => Socket_Ext,
                              Item   => Buffer,
                              Last   => Last,
                              To     => Addr);
               end;
            end;
         end if;


      end Send_Packet;


      --------------------------
      -- Sub-Task Packager --
      --------------------------

      task Packager is

         entry Start;
         entry Set_Timeout (T : Duration);
         entry Unset_Timeout;
         entry Close;   -- Take attention, this Kill the packager process!!!
         entry Packet_Received(Peer   : Sock_Addr_Type;
                               Buffer : Stream_Element_Array;
                               Last   : Stream_Element_Offset);
      end Packager;

      task body Packager is

         Timeout_Armed : Boolean := False;
--         Peer_List     : Peer_Table;
         Close_Task    : Boolean := False;
      begin

         accept Start do
            null;
           -- Put_Line("packager started");
         end Start;

         Main_Loop :
         while not Close_Task loop


            select

               when State = Listening or State = Opened =>
               accept Packet_Received(Peer   : Sock_Addr_Type;
                                      Buffer : Stream_Element_Array;
                                      Last   : Stream_Element_Offset) do

                  --Put_Line("Input.Packager: Packet Received" );

--                    if Timeout_Armed then
--                       Insert(Peer_List, Peer.Addr);
--                    end if;

                  --Put_Line("Input.Packager: Insert in queue:" );
                  Internal_Queue.Insert
                    (new Internal_Packet'
                       (Class => Received_Data,
                        Data  => New_Byte_Array(Buffer (Buffer'First .. Last)),
                        Peer  => Peer));
                  --Put_Line("Input.Packager: Insert in queue: OK" );
               end;

            or
               accept Set_Timeout (T : Duration) do
                  Timeout_Armed := True;
--                  Set_Timeout(Peer_List, T);
                  -- Clear the Peer_List ??
               end Set_Timeout;

            or
               accept Unset_Timeout do
                  Timeout_Armed := False;
               end Unset_Timeout;

            or
               accept Close do
                  Close_Task := True;
               end Close;


--              or when Timeout_Armed and Size(Peer_List) > 0 =>
--                    delay until Get_Next_Expiration_Time(Peer_List);
--
--                    declare
--                       Expired_Peer : Inet_Addr_Type;
--                    begin
--
--                       Get_And_Delete_Expired(Peer_List, Expired_Peer);
--                       Internal_Queue.Insert
--                         (new Internal_Packet'(Class => Timeout_Event));
--                    end;

            end select;

         end loop Main_Loop;

--         Put_Line("****  Packager: Closed sucessfully  *****");
      exception
         when e: others =>
            Put_Line("input: packager dead!!!");
            Put_Line(Exception_Information(e));

      end Packager;



      -------------------------
      -- Sub-Task Fast_Reader --
      -------------------------
      task Fast_Reader is
         entry Start;
         entry Stop;
         entry Close;
      end Fast_Reader;

      task body Fast_Reader is
         Peer    : Sock_Addr_Type;
         Last    : Stream_Element_Offset;
         Buffer  : Stream_Element_Array (1 .. 1500);
         Reason  : Selector_Status;
         Deadline: Time;
         Read	 : Boolean := False;
         Close_Fast : Boolean := False;
         Rec : Socket_Type;
      begin

         Main_Loop:
         while not Close_Fast loop

            select
               when State = Listening =>
                  accept Stop do
                     --Put_Line("Fast  Reader   Stop!");
                     Read := False;
                  end Stop;

            or when State = Opened or State = Listening =>
               accept Start do
                  Read := True;
               end Start;

            or
               accept Close do
                  Close_Fast := True;
               end Close;

            else
               null;
            end select;

            if Read then
               Deadline := Clock + 0.5;

               --read from socket
               Timeout_Socket.Read_With_Timeout (Socket_Ext   => Socket_Ext,
                                                 Socket_Int   => Socket_Int,
                                                 From     => Peer,
                                                 Receiving_Socket => Rec,
                                                 Item     => Buffer,
                                                 Last     => Last,
                                                 Deadline => Deadline,
                                                 Status   => Reason);


               if Reason = Completed then

--                  Put_Line("Input.Fast_Reader: Sender: " & Image(Peer));
                  if Rec = Socket_Int then
                     --                     Put_Line("Input.Fast_Reader: Send Message");

--                       Put_Line("First: " & Buffer'First'img);
--                       Put_line("Last: " & Last'img);
                     Send_Packet(Byte_Array(Buffer(Buffer'First..Last)));
                  else
                    -- Put_Line("Input.Fast_Reader: Packet Received" );
                     Packager.Packet_Received(Peer, Buffer, Last);
                  end if;
               end if;

            end if;

         end loop Main_Loop;

--         Put_Line("****  Fast Reader: Closed sucessfully  *****");
      exception
         when e: others =>
            Put_Line("Input: Fast_Reader dead!!");
            Put_Line(Exception_Information(e));

      end Fast_Reader;

      ------------------------
      -- Handle_Data_Packet --
      ------------------------

      procedure Handle_Data_Packet (Packet : in out Protocol.Data.Data_Packet) is
         use Command_Queues;

         Source_ID : Peer_ID;
      begin


         declare
         begin

            --*TODO  se un nodo è un lower-peer, non può mandarmi pacchetti dati
            --*      bisogna fare un controllino


            Source_ID := Get_PeerID(PM   => Peer_Manag,
                                    Addr => Packet.Address);

            Destination.Insert
              (new Internal.Internal_Command'
                 (Class      => Internal.Data,
                  Source	  => Packet.Address,
                  Id         => Source_ID,
                  Seq_Number     => 0,   -- not used in Data packet
                  Sub_Seq_Number => 0,   -- Data packet haven't  SSN

                  Data_Seq_Number => Packet.Sequence_Num,
                  Sending_Ch => Packet.Channel,
                  StreamID   => Packet.StreamID,
                  Payload    => Packet.Payload));

         exception
            when e: Constraint_Error =>
               Put_Line("**********   Input:  PeerID not in Peer_Manager    *****************");
               Put_Line(Exception_Information(e));

               Packets.Protocol.Data.Free(Packet);
         end;



      end Handle_Data_Packet;


      ---------------------------
      -- Handle_Control_Packet --
      ---------------------------

      procedure Handle_Control_Packet (Packet : in out Command.Control_Packet;
                                       PB     : in out Parsing_Buffers.Parsing_Buffer) is
         use type Command.Request_Type;

         Source_Addr : Sock_Addr_Type;

         Source_ID : Peer_ID;
         Ctl : Boolean := True;
      begin

--         Put_Line("Input: Control Packet");

         -- Address where send the Ack. If it is a routed packet the Ack address
         -- is not the packet address, but it is indicated inside the packet.
         if Packet.ACK_Target = No_Sock_Addr then
            Source_Addr := Packet.Address;
         else
            Source_Addr := Packet.ACK_Target;
         end if;


         -- The Sender of this packet is not in the Peer_Manager; discard the packet
         if Packet.Command = Forward then
            Ctl := False;
         end if;
         if Packet.Command = Data_Control then
            if Packet.SC = Start then
               Ctl := False;
            end if;
         end if;

         if Ctl then
            declare
            begin

               Source_ID := Get_PeerID(PM   => Peer_Manag,
                                       Addr => Source_Addr);
            exception
               when e: Constraint_Error =>
                  Put_Line("3");
                  Put_Line("Addr: "& image(Source_Addr));
                  Put_Line("**********   Input:  PeerID not in Peer_Manager    *****************");
                  Put_Line("Input: Exception Message: " & Exception_Information(e));

                  Packets.Protocol.Command.Free(Packet);
                  return;
            end;
         end if;


         case Packet.Command is

            when Command.Hello =>

--               Put_Line("Input: HELLO received from: " & Image(Source_Addr));

               Destination.Insert
                 (new Internal.Internal_Command'
                    (Class           => Internal.Hello,
                     Source	     => Source_Addr,
                     Id              => Source_ID,

                     Seq_Number      => Packet.Sequence_Num,
                     Sub_Seq_Number  => Packet.Sub_Seq_Num,

                     Hello_Peer_Cred => Packet.H_Peer_Credential));


            when Command.Set_Default =>
--               Put_Line("Input: SET_DEFAULT received from:" & Image(Packet.Peer));

               Destination.Insert
                 (new Internal.Internal_Command'
                    (Class      => Internal.Set_Default,
                     Source	=> Source_Addr,
                     Id         => Source_ID,

                     Seq_Number     => Packet.Sequence_Num,
                     Sub_Seq_Number => Packet.Sub_Seq_Num,

                     Chann_Def  => Packet.Chann_Def,
                     Default	=> Packet.Default));

            when Command.Acknowledge =>
               --       Put_Line("Input: ACK received from: " & Image(Packet.Peer));
               if Packet.ACK_Reason = OK then
                  Destination.Insert
                    (new Internal.Internal_Command'
                       (Class              => Internal.ACK,
                        Source	           => Source_Addr,
                        Id                 => Source_ID,

                        Seq_Number         => Packet.Sequence_Num,
                        Sub_Seq_Number     => Packet.Sub_Seq_Num,

                        Sequence_Num_ACKed => Packet.ACKed_Number,
                        Sub_Seq_Num_ACKed  => Packet.ACKed_Sub_Number));
               else
                  Destination.Insert
                    (new Internal.Internal_Command'
                       (Class               => Internal.NACK,
                        Source	            => Source_Addr,
                        Id                  => Source_ID,

                        Seq_Number          => Packet.Sequence_Num,
                        Sub_Seq_Number      => Packet.Sub_Seq_Num,

                        Sequence_Num_NACKed => Packet.ACKed_Number,
                        Sub_Seq_Num_NACKed  => Packet.ACKed_Sub_Number,
                        NAcked_Reason       => Packet.ACK_Reason));
               end if;

            when Command.Forward =>

               --* Dal parsing buffer e dai puntatori al First e Last ripristino
               --* il pacchetto originale



               declare

                  subtype Fwd_Data is Byte_Array(1..Parsing_Buffers.Remaining(PB));

                  procedure Extract_Data is
                    new Parsing_Buffers.Extract(Fwd_Data);

                  Data : Fwd_Data;
                  Data_Pt : Byte_Array_Pt;
               begin

                  Extract_Data(PB, Data);
                  Data_Pt := new Byte_Array'(Data);

                  Destination.Insert
                    (new Internal.Internal_Command'
                       (Class          => Internal.Forward,
                        Source	       => Source_Addr,
                        Id	       => PeerID.all,        -- I create the Forward packet

                        Seq_Number     => Packet.Sequence_Num,
                        Sub_Seq_Number => Packet.Sub_Seq_Num,

--                        SourceID       => Packet.SourceID,
                        Forward_Data   => Data_Pt));

               end;
            when Command.Data_Control =>
               Put_Line("Input: DATA_CONTROL received: ");
               case Packet.SC is
                  when Start =>

                     -- A new peer, informations about this peer are not in the Peer_Manager

                     Destination.Insert
                       (new Internal.Internal_Command'
                          (Class           => Internal.Start,
                           Source          => Source_Addr,
                           Id              => Peer_ID(0), -- fake peer_id, because if i receive a Start
                           				  -- i haven't the peer into Peer Manager

                           Seq_Number      => Packet.Sequence_Num,
                           Sub_Seq_Number  => Packet.Sub_Seq_Num,

                           Start_Channel      => PPETP.Channel_ID(Packet.Param_1),
                           Start_New_Peer     => Packet.D_New_Peer,
                           Start_Peer_Cred    => Packet.D_Peer_Credential,
                           Start_Puncturing   => Packet.D_Puncturing,
                           Start_Routing_Prob => Packet.D_Routing_Prob));

                  when Stop =>
                     Destination.Insert
                       (new Internal.Internal_Command'
                          (Class	 => Internal.Stop,
                           Source	 => Source_Addr,
                           Id            => Source_ID,

                           Seq_Number     => Packet.Sequence_Num,
                           Sub_Seq_Number => Packet.Sub_Seq_Num,

                           Stop_Channel  => PPETP.Channel_ID(Packet.Param_1),
                           Stop_Old_Peer => Packet.D_Old_Peer));

                  when Redirect =>
                     Destination.Insert
                       (new Internal.Internal_Command'
                          (Class	      => Internal.Redirect,
                           Source	      => Source_Addr,
                           Id                 => Source_ID,

                           Seq_Number         => Packet.Sequence_Num,
                           Sub_Seq_Number     => Packet.Sub_Seq_Num,

                           Redir_Channel      => PPETP.Channel_ID(Packet.Param_1),
                           Redir_New_Peer     => Packet.D_New_Peer,
                           Redir_Old_Peer     => Packet.D_Old_Peer,
                           Redir_Peer_Cred    => Packet.D_Peer_Credential,
                           Redir_Puncturing   => Packet.D_Puncturing,
                           Redir_Routing_Prob => Packet.D_Routing_Prob));

                  when Punch =>
                     Destination.Insert
                       (new Internal.Internal_Command'
                          (Class	  => Internal.Punch,
                           Source	  => Source_Addr,
                           Id             => Source_ID,

                           Seq_Number     => Packet.Sequence_Num,
                           Sub_Seq_Number => Packet.Sub_Seq_Num,

                           NAT_Method     => Packet.Param_1 and 2#0011_1111#,
                           NAT_Param      => Packet.Param_2,
                           Start_Too      => (Packet.Param_1 and 2#1000_0000#) /= 0,
                           Redirect_Too   => (Packet.Param_1 and 2#0100_0000#) /= 0,
                           Punch_NAT_Attr => Packet.D_NAT_Param,

                           -- Used only if the Start_Too or Resdirect_Too flag is true
                           Punch_Channel      => PPETP.Channel_ID(Packet.Param_3),
                           Punch_New_Peer     => Packet.D_New_Peer,
                           Punch_Old_Peer     => Packet.D_Old_Peer,
                           Punch_Peer_Cred    => Packet.D_Peer_Credential,
                           Punch_Puncturing   => Packet.D_Puncturing,
                           Punch_Routing_Prob => Packet.D_Routing_Prob));

               end case;

         end case;


      end Handle_Control_Packet;


      ----------------------------
      -- Handle_Received_Packet --
      ----------------------------

      procedure Handle_Received_Packet (Packet : Internal_Packet) is
         Net_Pkt : Network_Packet :=
           Binary.Network.New_Packet(Remote_Addr => Packet.Peer,
                                     Data        => Packet.Data.all);
         use Packets.Protocol.Command.Parsing;

         -- used only for routing packets towards others peers
         PB: Parsing_Buffers.Parsing_Buffer := Parsing_Buffers.Make_Parsing_Buffer(Packet.Data.all);
         Actual_Cur, Last_Cur : Byte_Array_Offset;
      begin
         case Packet.Class is
            when Timeout_Event =>
               Put_Line("Input: Timeout Event!");
            when Received_Data =>




               --********************  TODO   **********************************
               --*
               --*     * controlla se il pacchetto è un pacchetto PPETP valido
               --*
               --*	 - se valido controlla il campo Sender Signature alla
               --*	   fine del pacchetto ed vede se elaborare il pacchetto
               --*	   o scartarlo magari segnalando l'evento
               --*       - se non valido deve inoltrare il pacchetto a livello
               --*         superiore per essere elaborato da altri protocolli
               --*         ad esempio ICE
               --*
               --***************************************************************



               if Packets.Protocol.Parsing.Is_PPETP(Packet.Data) then



                  -- if the library is not yet configurated, it can't analyse PPETP
                  -- packets
                  if PPETP_Ready then


                     -- Control if this is a Routed Packet, in this case check
                     -- the source signatoure before parsing it

                     if Command.Is_Command (Net_Pkt) then


                        --* Controllo firma sender per i pacchetti di controllo
                        --* se la firma non è valida scarta il pacchetto



                        if Is_Routed(Net_Pkt) then


                           --* ********************  TODO   **************************
                           --*
                           --*	Control Source signature
                           --*
                           --* deve esserci una funzione che restituisce la firma, se
                           --* il parsing restituisce un pacchetto di forwarding devo
                           --* riattaccare la firma del Source
                           --*
                           --*********************************************************


                           -- Saving the Actual and Last pointer in the Parsing Buffer
                           Actual_Cur := Parsing_Buffers.Actual_Cursor(PB);
                           Last_Cur := Parsing_Buffers.Last(PB);


                           --* se la firma non è valida scarta il pacchetto

                           --* Ripristina il pacchetto in caso sia da forward-are

                        end if;

                     else
                        --* Controllo firma sender per i pacchetti dati
                        --* se la firma non è valida scarta il pacchetto
                        null;
                     end if;


                     -- Put_Line("Input: Handler");
                     -- Put_Line("Input: Peer: " & Image(Packet.Peer));
                     declare

                        Parsed : Protocol_Packet'Class :=
                          Packets.Protocol.Parsing.Parse_Packet
                            (Net_Pkt, PeerID.all);
                     begin




                        if (Parsed in Data.Data_Packet) then
                           --     Put_Line("Input: Handler Data");
                           Handle_Data_Packet (Data.Data_Packet (Parsed));
                           --Free(Parsed);
                        elsif (Parsed in Command.Control_Packet) then
                           Put_Line("Input: Handler Control");
                           Handle_Control_Packet (Command.Control_Packet (Parsed), PB);
                        else
                           raise Program_Error;
                        end if;


                     end;
                  end if;

               else

                  declare
                     Managed: Boolean;
                  begin

                     -- not a PPETP Packet
                     Managed := Other_Input_Applications.Manage_Unknown_Packet(Data => Packet.Data);
                  end;

               end if;

         end case;
      end Handle_Received_Packet;

      Continue : Boolean;
   begin


      Continue := True;





   Main_Loop :
      while Continue loop
         if (State = Listening) then
            declare
               To_Be_Parsed : Internal_Packet_Pt := null;
            begin
               select
                  Internal_Queue.Extract (To_Be_Parsed);
                 -- Put_Line("Input: Extracted from Queue");
                  Handle_Received_Packet (To_Be_Parsed.all);
                  --Free(To_Be_Parsed);
                  Finalize(To_Be_Parsed);
               or
                  delay 0.1;  -- Non troppo bello!!!
               end select;
            end;
         end if;

         select when State = Closed =>
               --accept Open (Port : in out Network.Port_Type) do
               accept Open  do -- Non serve più sapere la porta


--
                  State := Opened;
                  --Socket_Port := Port;

                  State := Listening;
                  Fast_Reader.Start;
                  Packager.Start;

               exception
                  when e: Socket_Error =>
                     raise Program_Error;
               end Open;


         or when State = Opened or State = Listening =>
            accept Close do
               --Close_Socket (Socket); --non e' piu' responsabilità del Task_Input distruggere il socket
               abort Reader;
               State := Closed;
               Fast_Reader.Stop; -- Stop from socket reading

               -- Flush the queue
               declare
                  To_Be_Parsed : Internal_Packet_Pt;
               begin
                  while (not Internal_Queue.Is_Empty) loop
                     Internal_Queue.Extract (To_Be_Parsed);
                     Handle_Received_Packet (To_Be_Parsed.all);
                     --                     Free(To_Be_Parsed);
                     Finalize(To_Be_Parsed);
                  end loop;
               end;
            end Close;


         or
            accept Set_Timeout (Timeout : Duration) do
               Packager.Set_Timeout (Timeout);
            end Set_Timeout;
         or
            accept Change_In_Out_Socket(In_Out_Socket : Socket_Access) do
               Socket_Ext := In_Out_Socket.all;
            end Change_In_Out_Socket;
         or
            accept Stop_Read do
               Fast_Reader.Stop;
            end Stop_Read;
         or
            accept Start_Read do
               Fast_Reader.Start;
            end Start_Read;
         or
            accept Start do
               PPETP_Ready := True;
            end Start;
         or
            accept Stop do
               Fast_Reader.Stop;
               Fast_Reader.Close;

               Packager.Close;
               Continue := False;

            end Stop;

         else
            null;
         end select;
      end loop Main_Loop;

--      Put_Line("****  Input Task: Closed sucessfully  ****");

   exception
      when e: others =>
         Put_Line("input: Dead!!");
         Put_Line(Exception_Information(e));

   end Reader;

   procedure Finalize (X : in out Reader_Task) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Reader, Reader_Task);
   begin
      X.Stop;
      Free (X);
   end Finalize;

end Input;
