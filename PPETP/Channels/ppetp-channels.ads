with Target_Tables;

with Network;

with PPETP.Targets;         use PPETP.Targets;
with PPETP.Attributes;	    use PPETP.Attributes;
with Profiles.Parameters;   use Profiles.Parameters, Profiles;
with Profiles.Entanglers;   use Profiles.Entanglers;
with Packets.Binary.Application;   use Packets.Binary.Application;
with Network_Packet_Queues; use Network_Packet_Queues;

package PPETP.Channels is
   type Channel_Handler is tagged limited private;

   type Channel is access Channel_Handler;

   Target_Yet_Present: exception;

   -- function Id (CH : Channel) return Channel_ID;
   -- Return the ID of CH

   function New_Channel
     (Channel_Num	   : PPETP_Channel_ID;
      Profile      : Profile_Type;
      Parameters   : Parameters_Class_Pt;
      Output_Queue : Network_Packet_Queues.Queue_Pt)
      return       Channel;
   -- Create a new channel and assign it a reduction vector,
   -- a reduction factor and an ID.  Return the Channel access

   function New_Target
     (CH    : Channel_Handler;
      Peer  : Network.Sock_Addr_Type;
      Punct : Access_Attribute_Class)
      return Target_ID;
   -- Assign to channel CH a new target and assign address Peer
   -- to the new Target.  Return the ID of the newly created
   -- channel.  The new target start in the Not_Ready status.

   function Delete_Target(CH   : Channel_Handler;
                          Addr : Network.Sock_Addr_Type) return Boolean;

   -- Return true if a target with target with the same address exists
   function Target_Exist(CH   : Channel_Handler;
                         Addr : Network.Sock_Addr_Type) return Boolean;

   procedure Set_Target_Ready (CH        : Channel_Handler;
                               Target    : Target_Id);
   -- Change the status of the target.

   procedure Send_Packet
     (CH     : Channel_Handler;
      Packet : Application_Packet);
   -- Send a reduced packet over a channel

   -- procedure Send_Packet (CH     : Channel;
   --                       Packet : Entangled_Data);
   -- -- Send a packet (to be reduced)

   function Get_Profile (CH : Channel_Handler) return Profile_Type;

   function Get_Parameters(CH : Channel_Handler) return Parameters_Class_Pt;

   -- Print a Channel (and all the targets)
   procedure Print (CH : in Channel_Handler;
                    Tab_Num : in Natural);

private
   type Table_Access is access Target_Tables.Table;

   type Channel_Handler is tagged limited record
      Channel	: PPETP_Channel_ID;
      Profile   : Profile_Type;
      Entangler : Entangler_Class_Pt;
      Targets   : Table_Access;
      Output_Queue: Network_Packet_Queues.Queue_Pt;
   end record;
end PPETP.Channels;
