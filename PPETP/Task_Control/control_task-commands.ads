with Network;		use Network;
with Control_Task;	use Control_Task;


with PPETP.Attributes;	use PPETP.Attributes;

with PPETP_Mailboxes;
with PPETP;	use PPETP;






package Control_Task.Commands is


   procedure Send_Hello_Command(Control         : Control_Task.Task_Pt;
                                Dst_Peer        : Network.Sock_Addr_Type;

                                Routed          : Boolean := False;
                                Target_ID       : Peer_ID := 0;

                                Hello_Peer_Cred	: Access_Attribute_Class := null;

                                Mbox            : PPETP_Mailboxes.Mailbox_Access);


   procedure Send_Set_Default_Command(Control         : Control_Task.Task_Pt;
                                      Dst_Peer        : Network.Sock_Addr_Type;

                                      Routed          : Boolean := False;
                                      Target_ID	      : Peer_ID := 0;

                                      Channel         : PPETP.Channel_ID;
                                      Default         : Byte_Array_Pt;

                                      Mbox            : PPETP_Mailboxes.Mailbox_Access);


   -- Ack can't be Routed
   procedure Send_Ack_Command(Control         : Control_Task.Task_Pt;
                              Dst_Peer        : Network.Sock_Addr_Type;

                           --   Routed          : Boolean := False;
                           --   Target_ID	: Peer_ID := 0;

                              Sequence_Num_ACKed     : PPETP.Command_Sequence_Number;
                              Sub_Sequence_Num_ACKed : PPETP.Sub_Sequence_Number;

                              Mbox            : PPETP_Mailboxes.Mailbox_Access);

   -- NAck can't be Routed
   procedure Send_NAck_Command(Control         : Control_Task.Task_Pt;
                               Dst_Peer        : Network.Sock_Addr_Type;

                           --    Routed          : Boolean := False;
                           --    Target_ID       : Peer_ID := 0;

                               Sequence_Num_NACKed     : PPETP.Command_Sequence_Number;
                               Sub_Sequence_Num_NACKed : PPETP.Sub_Sequence_Number;
                               NAcked_Reason           : ACK_Reason_Type;

                               Mbox            : PPETP_Mailboxes.Mailbox_Access);

   procedure Send_Start_Command(Control         : Control_Task.Task_Pt;
                                Dst_Peer        : Network.Sock_Addr_Type;

                                Routed          : Boolean := False;
                                Target_ID       : Peer_ID := 0;

                                Start_Channel      : PPETP.Channel_ID;
                                Start_New_Peer     : Access_Attribute_Class;
                                Start_Peer_Cred    : Access_Attribute_Class := null;
                                Start_Puncturing   : Access_Attribute_Class := null;
                                Start_Routing_Prob : Access_Attribute_Class := null;

                                Mbox            : PPETP_Mailboxes.Mailbox_Access);


   procedure Send_Stop_Command(Control         : Control_Task.Task_Pt;
                               Dst_Peer        : Network.Sock_Addr_Type;

                               Routed          : Boolean := False;
                               Target_ID       : Peer_ID := 0;

                               Stop_Channel    : PPETP.Channel_ID;
                               Stop_Old_Peer   : Access_Attribute_Class;

                               Mbox            : PPETP_Mailboxes.Mailbox_Access);

   procedure Send_Punch_Command(Control         : Control_Task.Task_Pt;
                                Dst_Peer        : Network.Sock_Addr_Type;

                                Routed          : Boolean := False;
                                Target_ID	: Peer_ID := 0;

                                NAT_Method      : Byte;
                                NAT_Param       : Byte := 0;
                                Start_Too       : Boolean;
                                Redirect_Too    : Boolean;
                                Punch_NAT_Attr  : Access_Attribute_Class;

                                -- Used only if the Start_Too flag is true
                                Punch_Channel      : PPETP.Channel_ID;
                                Punch_New_Peer     : Access_Attribute_Class := null;
                                Punch_Peer_Cred    : Access_Attribute_Class := null;
                                Punch_Puncturing   : Access_Attribute_Class := null;
                                Punch_Routing_Prob : Access_Attribute_Class := null;

                                Mbox            : PPETP_Mailboxes.Mailbox_Access);




   procedure Send_Redirect_Command(Control         : Control_Task.Task_Pt;
                                   Dst_Peer        : Network.Sock_Addr_Type;

                                   Routed          : Boolean := False;
                                   Target_ID       : Peer_ID := 0;

                                   Redir_Channel      : PPETP.Channel_ID;
                                   Redir_New_Peer     : Access_Attribute_Class;
                                   Redir_Old_Peer     : Access_Attribute_Class;
                                   Redir_Peer_Cred    : Access_Attribute_Class := null;
                                   Redir_Puncturing   : Access_Attribute_Class := null;
                                   Redir_Routing_Prob : Access_Attribute_Class := null;

                                   Mbox            : PPETP_Mailboxes.Mailbox_Access);


   -- Is an internal packet, can't be routed
   procedure Send_Forward_Command(Control         : Control_Task.Task_Pt;
                                  Dst_Peer        : Network.Sock_Addr_Type;

                             --    Routed          : Boolean := False;
                             --    Target_ID	   : Peer_ID := 0;

                                  Data   : Byte_Array_Pt);




end Control_Task.Commands;
