with Command_Queues;
with Transmitters;
with Control_Task;
with Network;
with Profiles;
with Profiles.Parameters;	use Profiles.Parameters;
with Application_Packet_Queues;
with Packets.Internal;		use Packets.Internal;

with Auth.Credentials;	use Auth.Credentials;

with Peer_Manager;
with PPETP;	use PPETP;
with PPETP.Attributes.Attributes_Records;	use PPETP.Attributes.Attributes_Records;
with PPETP_Mailboxes;

package Main_Task is
   -------------
   -- Handler --
   -------------

   task type Handler  is

      entry Set_Low_Level_Values(Max_Sup:        in Natural := 100;    -- Data duplicate control
                                 Re_ACK:         in Natural := 20;     -- Control duplicate control
                                 Max_Oldest:     in Natural := 50_000; -- Control duplicate control
                                 Max_ACK:        in Natural := 8);     -- Control duplicate control

      entry Init (Queue       : Command_Queues.Queue_Pt;
                  Output      : Transmitters.Handler_Pt;
                  Ctl         : Control_Task.Task_Pt;
                  Prof_Type   : Profiles.Profile_Type;
                  Prof_Param  : Parameters_Class_Pt;
                  Ready_Queue : Application_Packet_Queues.Queue_Pt;
                  Peer_Man    : Peer_Manager.Peer_Manager_pt);

      entry Stop;


      entry Send_Routed_Command(My_Addr: Network.Sock_Addr_Type;
                                Target_ID  : in Peer_ID;
                                Command    : in Command_Class;
                                Channel    : in Channel_ID;
                                Attributes : in Attributes_Record;
                                Mbox :       in PPETP_Mailboxes.Mailbox_Access);

   end Handler;

   type Task_Pt is access Handler;

   procedure Finalize (X : in out Task_Pt);



end Main_Task;
