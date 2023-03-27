
--* TODO
--*
--*	Sono ancora temporanee, tanto per fare un pò di prove
--*
--*

with Ada.Streams;		use Ada.Streams;

with PPETP;			use PPETP;
with Network;			use Network;
with Profiles;			use Profiles;
with Profiles.Parameters;	use Profiles.Parameters;
with Peer_Informations;		use Peer_Informations;
with byte_arrays;		use byte_arrays;

with PPETP.Attributes;			use PPETP.Attributes;
with PPETP.Attributes.Puncturing;	use PPETP.Attributes.Puncturing;
with Packets.Internal;			use Packets.Internal;
with Packets.Protocol.Command;

package PPETP.External_API is


   subtype ACK_Motivation is Packets.Protocol.Command.ACK_Reason_Type;


   -----------------------------------------------------------------------------
   -- Peer Definition end procedures

   --* Forse non dovrebbe stare qui...


   subtype Puncturing_Info is Puncturing_Data;

   type Peer_Type is  private;

   procedure Set_Peer_Info(Peer :   in out Peer_Type;
                           PeerID : in     Peer_ID;
                           Addr :   in     Sock_Addr_Type;
                           Kind :   in     Peer_Kind);

   procedure Set_Peer_Credential(Peer : in out Peer_Type;
                                 Data : in     Byte_Array);

   procedure Set_Peer_Puncturing(Peer      : in out Peer_Type;
                                 Punct_Info: in     Puncturing_Info);

   procedure Set_Peer_Routing(Peer : in out Peer_Type;
                              Num :  in     Byte;
                              Den :  in     Byte);

   function Get_Peer_PeerID(Peer : in Peer_Type) return Peer_ID;
   function Get_Peer_Addr(Peer : in Peer_Type) return Sock_Addr_Type;
   function Get_Peer_Kind(Peer : in Peer_Type) return Peer_Kind;
   function Get_Peer_Credential(Peer : in Peer_Type) return Access_Attribute_Class;
   function Get_Peer_Puncturing(Peer : in Peer_Type) return Access_Attribute_Class;
   function Get_Peer_Routing(Peer : in Peer_Type) return Access_Attribute_Class;





   -----------------------------------------------------------------------------
   -- Create / Destroy Sessions

   function New_Session return Session_ID;

   procedure Close_Session (Session : in Session_ID);


   --* TODO solo provvisoria ????
   procedure Start (Session : in Session_ID);


   -----------------------------------------------------------------------------
   -- Set Info / Connections

   procedure Set_Info (Session :   in Session_ID;
                       Host_Port : in Port_Type := No_Port;
                       Host_IP :   in Inet_Addr_Type := Any_Inet_Addr;
                       PeerID :    in Peer_ID := No_Peer_ID);

   procedure Set_Profile (Session :   in Session_ID;
                          Profile:    in Profile_Type := Basic_Profile;
                          Parameters: in Parameters_Class_Pt := null);


   procedure New_Channel (Session:    in Session_ID;
                          Channel:    in PPETP_Channel_ID;
                          Profile:    in Profile_Type := Basic_Profile;
                          Parameters: in Parameters_Class_Pt := null);



   --* Forse serviranno ulteriori parametri
   procedure Connect_To_Peer(Session   : Session_ID;
                             Peer_Addr : Inet_Addr_Type;
                             Port      : Port_Type;
                             Channel   : PPETP_Channel_ID;
                             PeerID    : Peer_ID);

--     -- Disconnect from upper-peer
--     procedure Disconnect_From_Peer(Session   : Session_ID;
--                                    PeerID    : Peer_ID := No_Peer_ID);
--     -- Disconnect lower-peer
--     procedure Disconnect_Peer(Session   : Session_ID;
--                               PeerID    : Peer_ID := No_Peer_ID);


   procedure XML_Configure (Session  : in Session_ID;
                            XML_Conf : in String);

--     procedure Connect(Session : in Session_ID;
--                       URL :     in String);


   procedure Set_Sequence_Num(Session  : in Session_ID;
                              StreamID : in Stream_ID;
                              Seq_Num  : in Data_Sequence_Number);


   -----------------------------------------------------------------------------
   -- Management


   -- Add and Remove peers from the Peer_Manager

   procedure Add_Peer (Session: in     Session_ID;
                       Peer:    in     Peer_Type;
                       Result:     out Boolean);

   procedure Remove_Peer (Session: in     Session_ID;
                          PeerID : in     Peer_ID;
                          Result:  in out Boolean);

   -----------------------------------------------------------------------------
   -- Get Info

   function Get_PeerID (Session : in Session_ID) return Peer_ID;

   function Get_Address (Session : in Session_ID) return Inet_Addr_Type;

   function Get_Port (Session : in Session_ID) return Port_Type;



   -----------------------------------------------------------------------------
   -- I/O

   procedure Send (Session   : Session_ID;
                   StreamID  : PPETP.Stream_ID;
                   Data      : Stream_Element_Array);

   procedure Recv (Session   : in     Session_ID;
                   StreamID  :    out PPETP.Stream_ID;
                   Data      :    out Stream_Element_Array;
                   Last      :    out Stream_Element_Offset);


   -- Routed Packets
   procedure Send_Routed_Hello(Session:    in Session_ID;
                               Target :    in Peer_ID;

                               Credential: in Byte_Array_Pt := null;

                               Ack_Received: out Boolean;
                               Ack_Response: out ACK_Motivation
                              );


   procedure Send_Routed_Start(Session: in Session_ID;
                               Target : in Peer_ID;

                               Channel: in PPETP.Channel_ID;

                               New_Peer_Address:    in Sock_Addr_Type;
                               New_Peer_PeerID:     in Peer_ID;

                               Credential:   in Byte_Array_Pt := null;

                               Punct_Info:   in Puncturing_Info;

                               Routing_Info: in Puncturing_Info;

                               Ack_Received: out Boolean;
                               Ack_Response: out ACK_Motivation
                              );

   procedure Send_Routed_Stop(Session: in Session_ID;
                              Target : in Peer_ID;

                              Channel: in PPETP.Channel_ID;

                              Old_Peer_Address: in Sock_Addr_Type;
                              Old_Peer_PeerID:  in Peer_ID;

                              Ack_Received: out Boolean;
                              Ack_Response: out ACK_Motivation
                             );

   --************     TODO      ****************************
   procedure Send_Routed_Punch(Session: in Session_ID;
                               Target : in Peer_ID;

                               NAT_Method:   in Byte;
                               NAT_Param:    in Byte := 0;
                               Start_Too:    in Boolean;
                               Redirect_Too: in Boolean;

                               Channel: in PPETP.Channel_ID;

                               New_Peer_Address:    in Sock_Addr_Type := No_Sock_Addr;
                               New_Peer_PeerID:     in Peer_ID := No_Peer_ID;

                               Credential:   in Byte_Array_Pt := null;

                               Punct_Info:   in Puncturing_Info;

                               Routing_Info: in Puncturing_Info;

                               Ack_Received: out Boolean;
                               Ack_Response: out ACK_Motivation
                              );

   procedure Send_Routed_Redirect(Session: in Session_ID;
                                  Target : in Peer_ID;

                                  Channel: in PPETP.Channel_ID;

                                  New_Peer_Address:    in Sock_Addr_Type;
                                  New_Peer_PeerID:     in Peer_ID;

                                  Old_Peer_Address: in Sock_Addr_Type;
                                  Old_Peer_PeerID:  in Peer_ID;

                                  Credential:   in Byte_Array_Pt := null;

                                  Punct_Info:   in Puncturing_Info;

                                  Routing_Info: in Puncturing_Info;

                                  Ack_Received: out Boolean;
                                  Ack_Response: out ACK_Motivation
                                 );



   --
   -----------------------------------------------------------------------------
   --

private

   type Peer_Type is record
	PeerID :   Peer_ID := 0;
	Address:   Sock_Addr_Type := No_Sock_Addr;	-- Public Address of the peer
	PeerKind:  Peer_Kind;
	Credential: Access_Attribute_Class := null;
	Puncturing: Access_Attribute_Class := null;
	Routing:    Access_Attribute_Class := null;
   end record;


end PPETP.External_API;
