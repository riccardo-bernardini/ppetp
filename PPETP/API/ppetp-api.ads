--                              -*- Mode: Ada -*-
--  Filename        : ppetp-api.ads
--  Description     : External API for PPETP
--  Author          :
--  Created On      : Sat Jul 19 12:18:59 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!



with Ada.Streams;
with Ada.Calendar;
use  Ada;

with Network;	use Network;   -- Same interface as GNAT.Sockets
with Profiles;	use Profiles;
with Profiles.Parameters;	use Profiles.Parameters;
with Peer_Informations;		use Peer_Informations;

with PPETP;		use PPETP;
with PPETP_Mailboxes;	use PPETP_Mailboxes;

with PPETP.Attributes;				use PPETP.Attributes;
with PPETP.Attributes.Attributes_Records;	use PPETP.Attributes.Attributes_Records;

with Packets.Internal;		use Packets.Internal;


package PPETP.API is
   Too_Many_Sessions   : exception;
   Session_Yet_Started : exception;
   Session_Not_Ready   : exception;
   Invalid_Session     : exception;
   Invalid_Source      : exception;
   Invalid_Channel     : exception;
   Invalid_Target      : exception;





   --------------------------
   -- Open/Close functions --
   --------------------------


   -- Open/close sessions --
   function New_Session (Ctl          : Port_Type := Any_Port;
                         My_Public_IP : Inet_Addr_Type := No_Inet_Addr; -- If i don't send routed packet, i don't use this field
                         Profile      : Profile_Type := Basic_Profile;
                         Prof_Parameters : Parameters_Class_Pt;
                         PeerID          : Peer_ID := 0; -- if peerID=0 generate a random PeerID
                         N_Trials     : Natural := 1)
                       return Session_ID;

   -- This function create the session, configures the channels and connect
   -- to the peers
   function New_Session(XML_Configuration: String;
                        Ctl              : Port_Type := Any_Port;
                        My_Public_IP     : Inet_Addr_Type := No_Inet_Addr; -- If i don't send routed packet, i don't use this field
                        PeerID           : Peer_ID := 0;
                        N_Trials         : Natural := 1) return Session_ID;

   procedure Close (Session : in Session_ID);


   function Get_PeerID (Session : in Session_ID)
                        return Peer_ID;

   procedure Set_PeerID (Session: in Session_ID;
                         PeerID:  in Peer_ID);

   procedure Set_Address (Session: in Session_ID;
                          Addr:    in Inet_Addr_Type);

   function Get_Address (Session : in Session_ID)
                         return Inet_Addr_Type;

   procedure Set_Port (Session: in Session_ID;
                       Port:    in Port_Type);

   function Get_Port (Session : in Session_ID)
                      return Port_Type;


   function Get_Internal_Port(Session: in Session_ID) return Port_Type;


   procedure Set_Profile(Session: in Session_ID;
                         Profile: Profile_Type := Basic_Profile;
                         Parameters: Parameters_Class_Pt := null);


   -- Open/Close output channels --

   -- Open a new output channel and assign it a profile and
   -- (possibly) a se of profile parameters.


   -- Set the Sequence Number for the StreamID
   procedure Set_Sequence_Num(Session  : in Session_ID;
                              StreamID : in Stream_ID;
                              Seq_Num  : in Data_Sequence_Number);


   procedure Set_Default_StreamID(Session  : in Session_ID;
                                  StreamID : in Stream_ID);




   procedure New_Channel (Session    : Session_ID;
                          Profile    : Profiles.Profile_Type;
                          Parameters : Profiles.Parameters.Parameters_Class_Pt := null;
                          Channel    : PPETP_Channel_ID);

   procedure Get_Channel (Session    : in     Session_ID;
                          Channel    : in     PPETP_Channel_ID;
                          Profile    :    out Profiles.Profile_Type;
                          Parameters :    out Profiles.Parameters.Parameters_Class_Pt);






   --     -- Open/Close input sources --
   procedure Connect_To_Peer (Session   : Session_ID;
                              Peer_Addr : Inet_Addr_Type;
                              Port      : Port_Type;
                              Channel   : PPETP_Channel_ID;
                              PeerID    : Peer_ID);



   -- Add a peer in the list of the known peer (Peer_Manager)
   procedure Add_Peer (Session  : Session_ID;
                       PeerID   : Peer_ID;
                       Address  : Sock_Addr_Type;
                       PeerKind : Peer_Kind;
                       Peer_Cred    : Access_Attribute_Class := null;
                       Puncturing   : Access_Attribute_Class := null;
                       Routing_Prob : Access_Attribute_Class := null;
                       Result   : in out Boolean);


   -- Delete a peer from the list of the known peer (Peer_Manager)
   procedure Delete_Peer (Session  : Session_ID;
                          PeerID   : Peer_ID;
                          Result   : in out Boolean);

   -------------------
   -- I/O Functions --
   -------------------

   procedure Send (Session   : Session_ID;
                   StreamID  : PPETP.Stream_ID;
                   Data      : Streams.Stream_Element_Array);


   procedure Recv (Session   : in     Session_ID;
                   StreamID  :    out PPETP.Stream_ID;
                   Data      :    out Streams.Stream_Element_Array;
                   Last      :    out Streams.Stream_Element_Offset);
   -- Wait for a packet to arrive. Return its content in Data and


--     function Packet_Size (Session : Session_Id)
--                           return Natural;
   -- Return the size of the first packet in the queue



   --------------------
   -- Routed Packets --
   --------------------

   procedure Send_Routed_Packet (Session    : in Session_ID;
                                 Target_ID  : in Peer_ID;
                                 Command    : in Command_Class;
                                 Channel    : in Channel_ID;
                                 Attributes : in Attributes_Record;
                                 Mbox :       in PPETP_Mailboxes.Mailbox_Access);




   -------------------------
   -- Debugging Functions --
   -------------------------
   procedure Print_State(Session   : Session_ID);


   -----------------------------------------------------------------------------
   --
   --		NEW API
   --
   -----------------------------------------------------------------------------


   procedure Start_Session (Session : Session_ID);

   -----------------
   -- New_Session --
   -----------------
   function New_Session (Host_Port     : in Port_Type := No_Port;
                         Host_IP       : in Inet_Addr_Type := Any_Inet_Addr;
                         Internal_Port : in Port_Type := No_Port;
                         PeerID :        in Peer_ID := No_Peer_ID) return Session_ID;




end PPETP.API;
