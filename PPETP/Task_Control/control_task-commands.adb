
with Packets.Protocol.Command;		use Packets.Protocol.Command;

package body Control_Task.Commands is

   ------------------------
   -- Send_Hello_Command --
   ------------------------
   procedure Send_Hello_Command(Control         : Control_Task.Task_Pt;
                                Dst_Peer        : Network.Sock_Addr_Type;

                                Routed          : Boolean := False;
                                Target_ID	: Peer_ID := 0;

                                Hello_Peer_Cred	: Access_Attribute_Class := null;
                                Mbox            : PPETP_Mailboxes.Mailbox_Access) is

      Pkt : Control_Packet := Control_Packet'(ACK_Target   => No_Sock_Addr,
                                              Command      => Hello,
                                              Sequence_Num => <>,
                                              Sub_Seq_Num  => 0,
                                              Address      => <>,

                                              H_Peer_Credential => Hello_Peer_Cred );
   begin


      Control.Send_Command(What      => Pkt,
                           To        => Dst_Peer,
                           Routed    => Routed,
                           Target_ID => Target_ID,
                           Reply_To  => Mbox);

   end Send_Hello_Command;


   ------------------------------
   -- Send_Set_Default_Command --
   ------------------------------
   procedure Send_Set_Default_Command(Control         : Control_Task.Task_Pt;
                                      Dst_Peer        : Network.Sock_Addr_Type;

                                      Routed          : Boolean := False;
                                      Target_ID	      : Peer_ID := 0;

                                      Channel         : PPETP.Channel_ID;
                                      Default         : Byte_Array_Pt;

                                      Mbox            : PPETP_Mailboxes.Mailbox_Access) is

      Pkt : Control_Packet := Control_Packet'(ACK_Target   => No_Sock_Addr,
                                              Command      => Set_Default,
                                              Sequence_Num => <>,
                                              Sub_Seq_Num  => 0,
                                              Address      => <>,

                                              Chann_Def    => Channel,
                                              Default      => Default);

   begin


      Control.Send_Command(What      => Pkt,
                           To        => Dst_Peer,
                           Routed    => Routed,
                           Target_ID => Target_ID,
                           Reply_To  => Mbox);
   end Send_Set_Default_Command;



    ----------------------
   -- Send_Ack_Command --
   ----------------------
   procedure Send_Ack_Command(Control         : Control_Task.Task_Pt;
                              Dst_Peer        : Network.Sock_Addr_Type;

                         --     Routed          : Boolean := False;
                         --     Target_ID	: Peer_ID := 0;

                              Sequence_Num_ACKed     : PPETP.Command_Sequence_Number;
                              Sub_Sequence_Num_ACKed : PPETP.Sub_Sequence_Number;

                              Mbox            : PPETP_Mailboxes.Mailbox_Access) is

      Pkt : Control_Packet := Control_Packet'(ACK_Target   => No_Sock_Addr,
                                              Command      => Acknowledge,
                                              Sequence_Num => <>,
                                              Sub_Seq_Num  => 0,
                                              Address      => <>,

                                              ACKed_Number     => Sequence_Num_ACKed,
                                              ACKed_Sub_Number => Sub_Sequence_Num_ACKed,
                                              ACK_Reason       => OK);

   begin

      Control.Send_Command(What      => Pkt,
                           To        => Dst_Peer,
                           Routed    => False,
                           Target_ID => 0,
                           Reply_To  => Mbox);

   end Send_Ack_Command;

   -----------------------
   -- Send_NAck_Command --
   -----------------------
   procedure Send_NAck_Command(Control         : Control_Task.Task_Pt;
                               Dst_Peer        : Network.Sock_Addr_Type;

                        --       Routed          : Boolean := False;
                        --       Target_ID       : Peer_ID := 0;

                               Sequence_Num_NACKed     : PPETP.Command_Sequence_Number;
                               Sub_Sequence_Num_NACKed : PPETP.Sub_Sequence_Number;
                               NAcked_Reason           : ACK_Reason_Type;

                               Mbox            : PPETP_Mailboxes.Mailbox_Access) is

      Pkt : Control_Packet := Control_Packet'(ACK_Target    => No_Sock_Addr,
                                              Command       => Acknowledge,
                                              Sequence_Num  => <>,
                                              Sub_Seq_Num   => 0,
                                              Address       => <>,

                                              ACKed_Number     => Sequence_Num_NACKed,
                                              ACKed_Sub_Number => Sub_Sequence_Num_NACKed,
                                              ACK_Reason       => NAcked_Reason);

   begin

      pragma Assert(NAcked_Reason /= OK);

      Control.Send_Command(What      => Pkt,
                           To        => Dst_Peer,
                           Routed    => False,
                           Target_ID => 0,
                           Reply_To  => Mbox);

   end Send_NAck_Command;


   ------------------------
   -- Send_Start_Command --
   ------------------------
   procedure Send_Start_Command(Control         : Control_Task.Task_Pt;
                                Dst_Peer        : Network.Sock_Addr_Type;

                                Routed          : Boolean := False;
                                Target_ID	: Peer_ID := 0;

                                Start_Channel      : PPETP.Channel_ID;
                                Start_New_Peer     : Access_Attribute_Class;
                                Start_Peer_Cred    : Access_Attribute_Class := null;
                                Start_Puncturing   : Access_Attribute_Class := null;
                                Start_Routing_Prob : Access_Attribute_Class := null;

                                Mbox            : PPETP_Mailboxes.Mailbox_Access) is

      Pkt : Control_Packet := Control_Packet'(ACK_Target    => No_Sock_Addr,
                                              Command       => Data_Control,
                                              Sequence_Num  => <>,
                                              Sub_Seq_Num   => 0,
                                              Address       => <>,

                                              Param_1 => Byte(Start_Channel),
                                              Param_2 => 0,
                                              Param_3 => 0,

                                              SC                => Start,
                                              D_New_Peer        => Start_New_Peer,
                                              D_Peer_Credential => Start_Peer_Cred,
                                              D_Puncturing      => Start_Puncturing,
                                              D_Routing_Prob    => Start_Routing_Prob,
                                              D_Old_Peer        => null,
                                              D_NAT_Param       => null);


   begin

      Control.Send_Command(What      => Pkt,
                           To        => Dst_Peer,
                           Routed    => Routed,
                           Target_ID => Target_ID,
                           Reply_To  => Mbox);
   end Send_Start_Command;



   -----------------------
   -- Send_Stop_Command --
   -----------------------
   procedure Send_Stop_Command(Control         : Control_Task.Task_Pt;
                               Dst_Peer        : Network.Sock_Addr_Type;

                               Routed          : Boolean := False;
                               Target_ID       : Peer_ID := 0;

                               Stop_Channel    : PPETP.Channel_ID;
                               Stop_Old_Peer   : Access_Attribute_Class;

                               Mbox            : PPETP_Mailboxes.Mailbox_Access) is

      Pkt : Control_Packet := Control_Packet'(ACK_Target    => No_Sock_Addr,
                                              Command       => Data_Control,
                                              Sequence_Num  => <>,
                                              Sub_Seq_Num   => 0,
                                              Address       => <>,

                                              Param_1 => Byte(Stop_Channel),
                                              Param_2 => 0,
                                              Param_3 => 0,

                                              SC                => Stop,
                                              D_New_Peer        => null,
                                              D_Peer_Credential => null,
                                              D_Puncturing      => null,
                                              D_Routing_Prob    => null,
                                              D_Old_Peer        => Stop_Old_Peer,
                                              D_NAT_Param       => null);
   begin

      Control.Send_Command(What      => Pkt,
                           To        => Dst_Peer,
                           Routed    => Routed,
                           Target_ID => Target_ID,
                           Reply_To  => Mbox);

   end Send_Stop_Command;



   ------------------------
   -- Send_Punch_Command --
   ------------------------

   procedure Send_Punch_Command(Control         : Control_Task.Task_Pt;
                                Dst_Peer        : Network.Sock_Addr_Type;

                                Routed          : Boolean := False;
                                Target_ID	: Peer_ID := 0;

                                NAT_Method     : Byte;
                                NAT_Param      : Byte := 0;
                                Start_Too      : Boolean;
                                Redirect_Too   : Boolean;
                                Punch_NAT_Attr : Access_Attribute_Class;

                                -- Used only if the Start_Too flag is true
                                Punch_Channel      : PPETP.Channel_ID;
                                Punch_New_Peer     : Access_Attribute_Class := null;
                                Punch_Peer_Cred    : Access_Attribute_Class := null;
                                Punch_Puncturing   : Access_Attribute_Class := null;
                                Punch_Routing_Prob : Access_Attribute_Class := null;

                                Mbox            : PPETP_Mailboxes.Mailbox_Access) is


      Pkt : Control_Packet := Control_Packet'(ACK_Target    => No_Sock_Addr,
                                              Command       => Data_Control,
                                              Sequence_Num  => <>,
                                              Sub_Seq_Num   => 0,
                                              Address       => <>,

                                              Param_1 => <>,
                                              Param_2 => NAT_Param,
                                              Param_3 => 0,

                                              SC                => Punch,
                                              D_New_Peer        => null,
                                              D_Peer_Credential => null,
                                              D_Puncturing      => null,
                                              D_Routing_Prob    => null,
                                              D_Old_Peer        => null,
                                              D_NAT_Param       => Punch_NAT_Attr);
   begin

      Pkt.Param_1 := NAT_Method;

      if Start_Too then
         Pkt.Param_1 := Byte( 2#1000_0000# + Natural(Pkt.Param_1));
      end if;

      if Redirect_Too then
         Pkt.Param_1 := Byte( 2#0100_0000# + Natural(Pkt.Param_1));
      end if;


      Control.Send_Command(What      => Pkt,
                           To        => Dst_Peer,
                           Routed    => Routed,
                           Target_ID => Target_ID,
                           Reply_To  => Mbox);

   end Send_Punch_Command;





   ---------------------------
   -- Send_Redirect_Command --
   ---------------------------
   procedure Send_Redirect_Command(Control         : Control_Task.Task_Pt;
                                   Dst_Peer        : Network.Sock_Addr_Type;

                                   Routed          : Boolean := False;
                                   Target_ID	   : Peer_ID := 0;

                                   Redir_Channel      : PPETP.Channel_ID;
                                   Redir_New_Peer     : Access_Attribute_Class;
                                   Redir_Old_Peer     : Access_Attribute_Class;
                                   Redir_Peer_Cred    : Access_Attribute_Class := null;
                                   Redir_Puncturing   : Access_Attribute_Class := null;
                                   Redir_Routing_Prob : Access_Attribute_Class := null;

                                   Mbox            : PPETP_Mailboxes.Mailbox_Access) is

      Pkt : Control_Packet := Control_Packet'(ACK_Target    => No_Sock_Addr,
                                              Command       => Data_Control,
                                              Sequence_Num  => <>,
                                              Sub_Seq_Num   => 0,
                                              Address       => <>,

                                              Param_1 => Byte(Redir_Channel),
                                              Param_2 => 0,
                                              Param_3 => 0,

                                              SC                => Redirect,
                                              D_New_Peer        => Redir_New_Peer,
                                              D_Peer_Credential => Redir_Peer_Cred,
                                              D_Puncturing      => Redir_Puncturing,
                                              D_Routing_Prob    => Redir_Routing_Prob,
                                              D_Old_Peer        => Redir_Old_Peer,
                                              D_NAT_Param       => null);
   begin

      Control.Send_Command(What      => Pkt,
                           To        => Dst_Peer,
                           Routed    => Routed,
                           Target_ID => Target_ID,
                           Reply_To  => Mbox);
   end Send_Redirect_Command;


   --------------------------
   -- Send_Forward_Command --
   --------------------------
   procedure Send_Forward_Command(Control         : Control_Task.Task_Pt;
                                  Dst_Peer        : Network.Sock_Addr_Type;

                               --   Routed          : Boolean := False;
                               --   Target_ID       : Peer_ID := 0;

                                  Data  : Byte_Array_Pt) is

      Pkt : Control_Packet := Control_Packet'(ACK_Target    => No_Sock_Addr,
                                              Command       => Forward,
                                              Sequence_Num  => <>,
                                              Sub_Seq_Num   => 0,
                                              Address       => <>,

                                              SourceID      => 0,	-- unused;
                                              Data          => Data);
   begin

      Control.Send_Command(What      => Pkt,
                           To        => Dst_Peer,
                           Routed    => False,
                           Target_ID => 0,
                           Reply_To  => null);

   end Send_Forward_Command;


end Control_Task.Commands;
