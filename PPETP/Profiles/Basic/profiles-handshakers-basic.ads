package  profiles.handshakers.basic is
   -- ============== --
   -- = Handshaker = --
   -- ============== --

   type Handshaker is new Root_Handshaker with null record;
   type Handshaker_Pt is access Handshaker;

   function New_Handshaker return Handshaker_Pt;

   overriding
   procedure Do_Handshaking (Actor   : Handshaker;
                             Sess    : PPETP.Sessions.Session_Record;
                             Peer    : Network.Sock_Addr_Type;
                             Profile : Profiles.Profile_Type;
                             Success : out Boolean);
end profiles.handshakers.basic;
