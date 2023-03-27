--                              -*- Mode: Ada -*-
--  Filename        : ppetp-sessions-initializer.ads
--  Description     : Initialize a PPETP session
--  Author          : Roberto Cesco Fabbro
--  Created On      :
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

with Configuration.Xml; use Configuration.Xml;
with Configuration; use Configuration;

with Network; use Network;

with Profiles; use Profiles;
with PPETP.Sessions; use PPETP.Sessions;
with Boolean_Mailboxes; use Boolean_Mailboxes;

with PPETP;



package PPETP.Sessions.Initializer is

   -- Used to associate the internal Channel_ID with the Global PPETP_Channel_ID
   type Channel_List_Type is array(PPETP.PPETP_Channel_ID range <>) of PPETP.Channel_ID;
   type Channel_List_Type_Pt is access Channel_List_Type;

   type Initializer_Record is limited
      record
         Session      : PPETP.Sessions.Session;
         Config	      : Config_Data(1..1);
         Channel_List : Channel_List_Type_Pt;
      end record;

   type Initializer is access Initializer_Record;


   function Init(Conf: Configuration.Config_Data) return Initializer;

   -- used for test
   procedure Send_Punch(Session_Init: Initializer;
                        Address     : Sock_Addr_Type;
                        To          : Sock_Addr_Type);
   -- used for test
   function Ping_Peer(Session_Init: Initializer;
                      To          : Sock_Addr_Type) return boolean;


private

   procedure Add_Channels_To_Session(Session : in out PPETP.Sessions.Session;
                                     Config  : in out Config_Data;
                                     Ch_List :    out Channel_List_Type_Pt);

   procedure Send_Hello_To_Sources(Session : in out PPETP.Sessions.Session;
                                   Config  : in out Config_Data);

   -- used for test
   function Send_Hello(Session  : PPETP.Sessions.Session;
                       Dst_Peer : Network.Sock_Addr_Type) return Boolean;

   procedure Auto_Punch(Session : in out PPETP.Sessions.Session;
                        Address : in     Sock_Addr_Type;
                        Reply_To: in out Boolean_Mailboxes.Mailbox_Access);



end PPETP.Sessions.Initializer;


