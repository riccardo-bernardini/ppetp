with PPETP;	use PPETP;
with Target_Tables;
with Application_Packet_Queues;
with Packets.Binary.Application;   use  Packets.Binary.Application;

with Command_Queues;
with Network; use Network;
with Network_Packet_Queues; use Network_Packet_Queues;
with Common_Types;	use Common_Types;

with Main_Task;
with Control_Task;
with Transmitters;
with Output_Task;
with Input;	use Input;


with Profiles;			use Profiles;
with Profiles.Parameters;	use Profiles.Parameters;
with Ada.Streams;	use Ada.Streams;

with Peer_Manager;
with PPETP.Attributes.Ack_Target;	use PPETP.Attributes.Ack_Target;

package PPETP.Sessions is

   type Sequence_Number_Array is array (Stream_ID'range) of Data_Sequence_Number;

   type State_Record is
      record
         Started   : Boolean := False;
         Ready     : Boolean := False;
         Profile   : Boolean := False;
         Low_Level : Boolean := False;
      end record;

   type Session_Record is limited
      record
         Transmitter      : Transmitters.Handler_Pt;
         Commands         : Command_Queues.Queue_Pt;
         Output_Queue     : Network_Packet_Queues.Queue_Pt;
         Main             : Main_Task.Task_Pt;
         Control          : Control_Task.Task_Pt;
         Output	          : Output_Task.Writer_Task;
         Ready_Queue      : Application_Packet_Queues.Queue_Pt;
         Control_Port     : Port_Type;
         Input	          : Reader_Task;
         Profile          : Profiles.Profile_Type;
         Prof_Parameters  : Parameters_Class_Pt;
         In_Out_Socket    : Socket_Access;
         Sequence_Num     : Sequence_Number_Array;
         My_Address       : Inet_Addr_Type;
         My_PeerID        : Peer_ID_Access;
         Peer_Manag	  : Peer_Manager.Peer_Manager_pt;
         Inter_Proc_Socket : Socket_Access;
         State            : State_Record;
         Default_StreamID : Stream_ID;
      end record;

   type Session is access Session_Record;

   procedure Set_Ready_Flag(State : in out Sessions.State_Record) ;


   function New_Session (Ctl_Port        : in Port_Type := Any_Port;
                         My_Public_IP    : in Inet_Addr_Type := No_Inet_Addr;
                         Profile         : in Profile_Type;
                         Prof_Parameters : in Parameters_Class_Pt;
                         PeerID          : in Peer_ID;
                         N_Trials        : in Natural := 1) return Session;

   procedure Destroy (What : in out Session);



   -- I/O Functions
   procedure Send (Parent   : in Session;
                   StreamID : in PPETP.Stream_ID;
                   Data     : in Stream_Element_Array);

   procedure Recv (Parent   : in     Session;
                   StreamID :    out PPETP.Stream_ID;
                   Data     :    out Stream_Element_Array;
                   Last     :    out Stream_Element_Offset);

   -----------------------------------------------------------------------------
   --
   --   NEW Functions
   --
   -----------------------------------------------------------------------------

   function New_Session (Host_Port:      in Port_Type;
                         Host_IP :       in Inet_Addr_Type;
                         Internal_Port : in Port_Type;
                         PeerID :        in Peer_ID) return Session;

   procedure Set_Low_Level_Values(Parent   :      in Session;
                                  Max_Trials:     in Natural;      -- Control_Task
                                  Timeout:        in Duration;     -- Control_Task
                                  Routed_Timeout: in Duration;     -- Control_Task
                                  Max_Sup:        in Natural := 100;    -- Data duplicate control
                                  Re_ACK:         in Natural := 20;     -- Control duplicate control
                                  Max_Oldest:     in Natural := 50_000; -- Control duplicate control
                                  Max_ACK:        in Natural := 8);     -- Control duplicate control

   procedure Start_Session(Parent : Session);

   procedure Set_New_Port(Parent: in Session;
                          Port:   in Port_Type);

   procedure Set_New_Address(Parent: in Session;
                             Addr:   in Inet_Addr_Type);

   procedure Set_PeerID(Parent: in Session;
                        PeerID: in Peer_ID);


end PPETP.Sessions;
