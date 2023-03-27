with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Network_UDP;     use Network_UDP;
with Ada.Streams;     use Ada.Streams;
with Ada.Calendar;    use Ada.Calendar;
with Sent_DB;         use Sent_DB;

with PPETP;
with Network; use Network;
with Packets.Protocol.Building;
with Packets.Binary.Network;
with Packets.Protocol.Command;	use Packets.Protocol.Command;
with Ada.Unchecked_Deallocation;


with PPETP_Mail;	use PPETP_Mail;
with PPETP.Attributes.New_Peer;		use PPETP.Attributes.New_Peer;

package body Control_Task is
   use PPETP_Mailboxes;
   use type PPETP.Command_Sequence_Number;

   task body Handler is

      Packet_DB : Sent_DB.Table;

      --* TODO forse si deve randomizzare
      Current_Sequence_Num : PPETP.Command_Sequence_Number := 0;

      My_Attr : ACK_TARGET_Attribute;


      procedure Transmit_Packet (Info     : Packet_Info;
                                 Need_ACK : Boolean := True;
                                 Routed   : Boolean := False) is

      begin


         Output_Tsk.Send_Priority_Packet(
           Packets.Binary.Network.New_Packet(
                  Data        => Byte_Array(Info.Data.all),
                  Remote_Addr => Info.Target));

         if (Need_ACK) then
            Packet_DB.Packet_Sent (Info, Routed);
         end if;

    --     Put_Line("Control Task:  Transmit_Packet: OK");

      end Transmit_Packet;

      procedure Send (Packet    : Control_Packet;
                      Target    : Network.Sock_Addr_Type;
                      Routed    : Boolean;
                      Target_ID : Peer_ID;
                      Mbox      : Mailbox_Access) is

         Bytes : Packets.Binary.Network.Network_Packet;
         Pkt   : Control_Packet := Packet;

      begin
         Pkt.Sequence_Num := Current_Sequence_Num;

         if Routed then
            Bytes := Packets.Protocol.Building.Make_Packet (Source     => Pkt,
                                                            Routed     => True,
                                                            Target_ID  => Target_ID,
                                                            Ack_Target => My_Attr);
         else
            Bytes := Packets.Protocol.Building.Make_Packet (Source     => Pkt,
                                                            Routed     => False,
                                                            Target_ID  => 0,
                                                            Ack_Target => My_Attr);
         end if;



         Put_Line("Control Task:  Send");
         Put_Line("   Type: " & Packet.Command'img);
         Put_Line("   Data Size: " & Bytes.Buffer'length'img);
         Put_Line("   Target: " & Image(Target) );
         Put_Line("   Sequence_Num: " & Current_Sequence_Num'img);
         Put_Line("   Sub_Seq_Num: " & Pkt.Sub_Seq_Num'img);
         Put_line("   Routed: " & Routed'img);
         if Routed then
            Put_line("   Routed Target: " & Target_ID'img);
         end if;


         if (Mbox /= null) then
            Put_Line("   Need ACK:  YES");
         else
            Put_Line("   Need ACK:  NO");
         end if;



         Transmit_Packet (Info => (Data         =>
                                   new Stream_Element_Array'(
                                     Stream_Element_Array(Bytes.Buffer)),
                                   Target       => Target,
                                   Send_Port    => No_Port,    -- <-- porta dalla quale viene spedito non viene usata da
                                   Reply_Mbox   => Mbox,       --     transmit_Packet, la metto cosi di default, si potrebbe
                                   Sequence_Num => Current_Sequence_Num), -- usare al posto del From_Inpu_Port

                          Need_ACK => (Mbox /= null),
                          Routed   => Routed);

         Current_Sequence_Num := (Current_Sequence_Num + 1) mod PPETP.Command_Sequence_Number'Last;
      end Send;

      Again : Boolean;

   begin
      Again := True;


      accept Init do
         null;
      end Init;

      Main_Loop :
      while Again loop



         select

            accept Set_ACK_Target_Attribute(Attr : in ACK_TARGET_Attribute) do
               My_Attr := Attr;
            end Set_ACK_Target_Attribute;
         or
            accept Set_Timeout (Value : Duration) do
               Packet_DB.Set_Timeout_Interval (Value);
            end;
         or
            accept Set_Routed_Timeout (Value : Duration) do
               Packet_DB.Set_Routed_Timeout_Interval (Value);
            end;
         or
            accept Set_Max_Trials (Value : Natural) do
               Packet_DB.Set_Max_Trials (Value);
            end;
         or
            accept Send_Command (What      : in Control_Packet;
                                 To        : in Network.Sock_Addr_Type;
                                 Routed    : in Boolean := False;
                                 Target_Id : in Peer_ID := 0;
                                 Reply_To  : in PPETP_Mailboxes.Mailbox_Access) do

               Send (What, To, Routed, Target_Id, Reply_To);
            end Send_Command;
         or
            accept ACK_Received (Sequence_Num : in PPETP.Command_Sequence_Number;
                                 Reason       : in ACK_Reason_Type) do
               declare
                  Info : Packet_Info := Packet_DB.Find (Sequence_Num);
                  Mail : PPETP_Mail_Ack_Type;
               begin
                  if (Info /= No_Info) then

                     --* TODO  ??? Cosa c'è da fare?
                     Mail.Received := True;
                     Mail.Reason   := Reason;

                     Packet_DB.Packet_Done (Sequence_Num, Mail);
                  end if;
               end;
            end ACK_Received;
         or
            accept Stop do
               Again := False;
            end Stop;
         or
            delay until Packet_DB.Earliest_Timeout;

            declare
               Packet : Packet_Info;
            begin
               Retransmit_Expired :
               loop
                  Packet_DB.Next_Expired (Packet);
                  exit when Packet = No_Info;

                  -- if the packet is a routed one (RH_Length /= 0)
                  -- increment the Sub_Sequence_Number of the packet before
                  -- send it. This is done only for the packet source putting
                  -- the mailbox /= null. For peer that route the packet the
                  -- mailbox is set to null so this code is never called
                  if (Byte(Packet.Data.all(3)) /= 0) then
                     Packet.Data.all(9) := Byte(Packet.Data.all(9) + 1);
                  end if;

                  Transmit_Packet (Packet, False);
               end loop Retransmit_Expired;
            end;

         end select;
      end loop Main_Loop;

--      Put_Line("****  Control Task: Closed sucessfully  ****");

   exception
      when e: others =>
         Put_Line("Control Task dead!!");
         Put_line(Exception_Information(e));
   end Handler;

   procedure Finalize (X : in out Task_Pt) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Handler, Task_Pt);
   begin
      X.Stop;
      Free (X);
   end Finalize;
end Control_Task;

