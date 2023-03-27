--*
--*	I Think this file is obsolete
--*


with Profiles.Parameters;
with PPETP.Sessions;
with Network;

package Profiles.Handshakers is
   -- ================ --
   -- == Handshaker == --
   -- ================ --

   type Root_Handshaker is abstract new Root_Profile_Handler with private;
   type Handshaker_Class_Pt is access all Root_Handshaker'Class;

   --
   -- Create a new handshaker for the given profile
   --
   function New_Handshaker (Profile : Profile_Type)
                            return Handshaker_Class_Pt;

   --
   -- It is expected that sending the value of the control port will be
   -- the first step of every handshaking sequence.  Function
   -- Send_Ctl_Port sends the Reply_Port command, wait for an ACK
   -- and return True if the handshaking procedure worked.
   --
   function Send_Hello (Actor   : Root_Handshaker;
                        Sess    : PPETP.Sessions.Session_Record;
                        Peer    : Network.Sock_Addr_Type;
                        Profile : Profiles.Profile_Type)
                        return Boolean;

   --
   -- Do_Handshaking is the method used to ask an Handshaker to do the
   -- handshaking procedure with the remote peer.  Since each profile
   -- can have its own variations, each profile must define its own
   -- version of this procedure.  However, since most of profiles will
   -- first send the control port, then the default parameters,
   -- makers of new profiles could consider using the Generic_Handshaking
   -- procedure.
   --
   procedure Do_Handshaking (Actor   : Root_Handshaker;
                             Sess    : PPETP.Sessions.Session_Record;
                             Peer    : Network.Sock_Addr_Type;
                             Profile : Profiles.Profile_Type;
                             Success : out Boolean)
      is abstract;

   generic
      type Parameter_Type is new Parameters.Root_Parameters with private;
      type Handshaker_Type is new Root_Handshaker with private;
   procedure Generic_Handshaking (Actor   : in     Handshaker_Type;
                                  Sess    : in     PPETP.Sessions.Session_Record;
                                  Peer    : in     Network.Sock_Addr_Type;
                                  Param   : in     Parameter_Type;
                                  Profile : in     Profiles.Profile_Type;
                                  Success :    out Boolean);

private
   type Root_Handshaker is abstract new Root_Profile_Handler with  null record;
end Profiles.Handshakers;
