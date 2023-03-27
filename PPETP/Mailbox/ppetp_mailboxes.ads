with PPETP_Mail;	use PPETP_Mail;
with Generic_Mailboxes;


package PPETP_Mailboxes is
   new Generic_Mailboxes (Return_Type => PPETP_Mail_Ack_Type);
