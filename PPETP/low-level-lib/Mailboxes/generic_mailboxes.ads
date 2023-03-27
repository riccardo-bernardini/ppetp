--                              -*- Mode: Ada -*-
--  Filename        : generic_mailbox.ads
--  Description     : Generic mailbox for asynch returns
--  Author          : Riccardo Bernardini
--  Created On      : Fri Aug 29 14:39:10 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!
--  Type            : <GENERIC>

--
-- == Abstract ==
-- ==============
--
-- A generic mailbox is used to do some kind of "asynchronous return."
-- A mailbox has an internal Boolean state (ready or not) and it can
-- store a value (whose type is the formal parameter of this
-- generic package).  A mailbox provides two operations
--
--   Wait(X : out Return_Value)
--     Wait for the mailbox become ready and write to X the
--     value stored in the mailbox.  Before returning, set the
--     state to "not ready"
--
--  Done(X : in Return_Value)
--     Wait for the mailbox to be not ready and store the value
--     of X inside the mailbox.  Set the mailbox to ready.
--
-- == Motivation==
-- ===============
--
-- For example, suppose that we want to carry out an handshaking
-- procedure by sending some packet to a remote node and waiting
-- for an ACK.  Suppose that if the ACK is not received, the packet
-- should be sent again, untile the ACK arrives.  Suppose, finally,
-- that all the stuff "send packet, wait for ACK, send againg, ..." is
-- carried out by a separate task which handles the handshaking request
-- coming from many different tasks.
--
-- The "handshaking task" would have an entry similar to
--
--       Do_Handshaking(Peer : Remote_Address)
--
-- After calling Do_Handshaking we would like to wait for the handshaking
-- result, represented, say, by a Boolean value (True = OK).  However, since
-- the task is handling many requests at time it cannot wait for the
-- ACK to arrive before returning.
--
-- The solution is the Mailbox.  We change the entry profile to
--
--       Do_Handshaking(Peer : Remote_Address;
--                      Box  : Mailbox_Access);
--
-- Now the handshaking task after sending the handshaking packet to
-- the remote peer, can exit from Do_Handshaking and go to serve other
-- requestes.  On the other side of the call, the task who requested
-- the service does
--
--          Box.Wait(OK)
--
-- As soon as the ACK packet arrives it can call
--
--            Box.Done(True)
--
-- or if the timeout expires
--
--            Box.Done(False)
--
-- After the completion of the Done call, the Wait call can be done and
-- the calling task will find the result in variable OK.
--

generic
   type Return_Type is private;
package Generic_Mailboxes is
   protected type Mailbox is
      entry Wait(Result :    out Return_Type);
      entry Done(Result : in     Return_Type);
   private
      Ready : Boolean := False;
      Return_Value : Return_Type;
   end Mailbox;

   type Mailbox_Access is access Mailbox;
end Generic_Mailboxes;
