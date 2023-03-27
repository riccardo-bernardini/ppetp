with Profiles.Handshakers.Basic;
with Boolean_Mailboxes;
with Packets.Protocol.Command;               use Packets;
with Packets.Protocol.Building;
with Packets.Binary.Network;

package body Profiles.Handshakers is
   function New_Handshaker (Profile : Profile_Type)
                            return Handshaker_Class_Pt is
      Result : Handshaker_Class_Pt;
   begin
      case (Profile) is
         when Basic_Profile =>
            Result := Handshaker_Class_Pt (Basic.New_Handshaker);
         when Vandermonde_Profile =>
            raise Program_Error;
      end case;

      Result.My_Profile := Profile;
      return Result;
   end New_Handshaker;

   function Send_Hello (Actor   : Root_Handshaker;
                        Sess    : PPETP.Sessions.Session_Record;
                        Peer    : Network.Sock_Addr_Type;
                        Profile : Profiles.Profile_Type)
                        return Boolean is
      use Boolean_Mailboxes;
      use Packets.Protocol.Command;

      Mbox    : Mailbox_Access := new Mailbox;
      Success : Boolean;
      Packet  : Control_Packet :=
                  Control_Packet'(Command    => Hello,
                                  Reply_Port => Sess.Control_Port,
                                  Profile    => Profile,
                                  Timestamp  => <>,
                                  Peer       => <>);
   begin
      Sess.Control.Send_Command (What     => Packet,
                                 To       => Peer,
                                 Reply_To => Mbox);
      Mbox.Wait (Success);
      return Success;
   end Send_Hello;

   procedure Generic_Handshaking (Actor   : in     Handshaker_Type;
                                  Sess    : in     PPETP.Sessions.Session_Record;
                                  Peer    : in     Network.Sock_Addr_Type;
                                  Param   : in     Parameter_Type;
                                  Profile : in     Profiles.Profile_Type;
                                  Success :    out Boolean) is
   begin
      Success := Actor.Send_Hello (Sess, Peer, Profile);
      if (not Success) then
         return;
      end if;

      raise Program_Error;
   end Generic_Handshaking;

end Profiles.Handshakers;
