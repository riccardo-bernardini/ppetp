with Network;
with Channel_Tables;
with Profiles.Parameters;   use Profiles, Profiles.Parameters;
with PPETP;                 use PPETP;
with PPETP.Attributes;	    use PPETP.Attributes;
with PPETP.Channels;
with Packets.Binary.Application;   use Packets.Binary.Application;
with Network_Packet_Queues; use Network_Packet_Queues;

--
-- ===================
-- == What is this? ==
-- ===================
--
-- This package provides resources to handle PPETP Transmitters.
-- A Transmitter is a collection of Channels.  Although its
-- presence is not necessary from a logical point of view (one
-- could store an array of channels in the Session structure
-- anyway), it is more convenient to have a separate type which
-- handles all the creation/distrution/etc.
--

package Transmitters is

   type Channel_List_Type is array(PPETP.PPETP_Channel_ID'First .. PPETP.PPETP_Channel_ID'Last) of PPETP.Channel_ID;



   protected type Output_Handler is
      -- Open a new channel with associated profile and
      -- processing parameters.  Return the channel ID
      procedure New_Channel (Profile     : in Profile_Type;
                             Details     : in Parameters_Class_Pt;
                             Output_Queue: in Network_Packet_Queues.Queue_Pt;
                             Channel     : in PPETP.PPETP_Channel_ID);

      -- Associate a new target with the given channel
      procedure New_Target (Channel : in     PPETP.PPETP_Channel_ID;
                            Address : in     Network.Sock_Addr_Type;
                            Punct   : in     Access_Attribute_Class;
                            Target  :    out Target_ID);

      -- Declare the target ready (i.e., the handshaking is
      -- concluded)
      procedure Target_Ready (Channel : PPETP.PPETP_Channel_ID;
                              Target  : Target_ID);

      -- Delete a target; return False if the peer is not in the channel
      function Delete_Target(Channel : PPETP.PPETP_Channel_ID;
                             Peer : Network.Sock_Addr_Type) return Boolean;

      -- Send Packet "urbi et orbi"
      procedure Transmit (Packet : Application_Packet);

      -- Set the size of the Channel_Table
      procedure Set_Size(Size: Natural);

      -- Get a Channel
      procedure Get_Channel(Channel : in     PPETP.PPETP_Channel_ID;
                            Result  :    out PPETP.Channels.Channel);


      procedure Print(Tab_Num : in Natural);

   private
      Channel_List  : Channel_List_Type;
      Channel_Table : Channel_Tables.Table;
   end Output_Handler;

   type Handler_Pt is access Output_Handler;
end Transmitters;
