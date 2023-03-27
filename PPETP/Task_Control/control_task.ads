--                              -*- Mode: Ada -*-
--  Filename        : control_task.ads
--  Description     : Definition of the task which sends control packets
--  Author          : Riccardo Bernardini
--  Created On      : Tue Feb  3 15:35:52 2009
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

with Packets.Protocol;     	 use Packets.Protocol;
with Packets.Protocol.Command;	 use Packets.Protocol.Command;
with PPETP_Mailboxes;
with Network;
with PPETP;

with byte_arrays; use byte_arrays;
with Output_Task; use Output_Task;
with Common_Types;	use Common_Types;
with PPETP.Attributes.Ack_Target;		use PPETP.Attributes.Ack_Target;
with PPETP;		use PPETP;


--
--
-- ================================
-- == This package in a nutshell ==
-- ================================
--
-- For every PPETP session there is a _control_task_ which is a
-- task whose duty is to send control packets and handle ACK &
-- retransmissions.   The interface of a control task provides the
-- following entries
--
--   * Send_Command     To send a control packet
--   * Send_ACK         To send an ACK packet
--   * ACK_Received     Called by the "main" part when an ACK packet
--                      is received
--   * Set_Max_Trials
--   * Set_Timeouts
--
-- ==============================
-- == Handling control packets ==
-- ==============================
--
-- The PPETP protocol requires that most of the control packets must
-- be acknowledeged by an ACK packet.  The procedure for handling
-- control packets is as follows
--
--   1. A control packet is transmitted and a timeout is set
--
--   2. If the ACK arrives before the timeout expiration, terminate
--      the packet handling procedure with SUCCESS
--
--   3. If the timeout expires before the arrive of the ACK packet and
--
--       3.1 If the packet has already been sent too many times,
--           terminate the packet handling procedure with FAILURE
--
--       3.2 Otherwise, go back to step 1
--
-- =======================
-- == Asynchronous call ==
-- =======================
--
-- A problem which must be solved is that, since the control task can be
-- used by more than one task at time, Send_Command cannot  block the task
-- while waiting for the ACK packet to arrive.  This means that
-- Send_Command cannot return to the caller the value SUCCESS/FAILURE
-- and another mean to return the result must be implemented.
--
-- The chosen solution is to return the result by means of a
-- Boolean_Mailbox.   Briefly, a Boolean_Mailbox is a protected object
-- with a state (ready or not) which can hold a boolean value.  In
-- order to use Send_Command the calling task will do
--
--   1. Create a new Boolean_Mailbox which is initialized with the
--      state "not ready"
--
--   2. Give the Boolean_Mailbox to the control task via the
--      Send_Command function
--
--   3. Do a "wait" over the Boolean_Mailbox.  When the control task
--      will terminate the packet handling procedure will store the
--      result into the Boolean_Mailbox.  This will cause the
--      termination of the "wait" procedure.
--
-- -- Example --
-- -------------
--
-- declare
--   Mbox : Boolean_Mailboxes.Mailbox_Access :=
--             new Boolean_Mailboxes.Mailbox;
--
--   OK   : Boolean;
-- begin
--   H.Send_Command(What     => Packet,
--                  To       => Peer_Address,
--                  Reply_To => Mbox);
--   Mbox.Wait(OK);
--   if (OK) then
--      Put_Line("Success!");
--   else
--      Put_Line("Failure");
--   end if;
-- end;
--
--
--

package Control_Task is
   task type Handler(Output_Tsk   : Writer_Task) is

      entry Init;

      entry Set_ACK_Target_Attribute(Attr : in ACK_TARGET_Attribute);

      entry Send_Command (What      : in Control_Packet;
                          To        : in Network.Sock_Addr_Type;
                          Routed    : in Boolean := False;
                          Target_Id : in Peer_ID := 0;
                          Reply_To  : in PPETP_Mailboxes.Mailbox_Access);

      entry ACK_Received (Sequence_Num : in PPETP.Command_Sequence_Number;
                          Reason       : in ACK_Reason_Type);

      entry Set_Max_Trials (Value : Natural);

      entry Set_Timeout (Value : Duration);

      entry Set_Routed_Timeout (Value : Duration);



      entry Stop;
   end Handler;

   type Task_Pt is access Handler;

   procedure Finalize (X : in out Task_Pt);
end Control_Task;
