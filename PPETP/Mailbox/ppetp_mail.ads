with Packets.Protocol.Command;	use Packets.Protocol.Command;

package PPETP_Mail is

   type PPETP_Mail_Ack_Type is
      record
         Received : Boolean;
         Reason   : ACK_Reason_Type;
      end record;
end PPETP_Mail;

