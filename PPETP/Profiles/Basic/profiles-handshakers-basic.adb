package body Profiles.Handshakers.Basic is
   function New_Handshaker return Handshaker_Pt is
   begin
      return new Handshaker;
   end New_Handshaker;

   procedure Do_Handshaking (Actor   : Handshaker;
                             Sess    : PPETP.Sessions.Session_Record;
                             Peer    : Network.Sock_Addr_Type;
                             Profile : Profiles.Profile_Type;
                             Success : out Boolean) is
   begin
      Success := Actor.Send_Hello(Sess, Peer, Profile);
   end Do_Handshaking;
end Profiles.Handshakers.Basic;
