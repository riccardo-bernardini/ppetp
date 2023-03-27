--                              -*- Mode: Ada -*-
--  Filename        : generic_synthesis_queue.ads
--  Description     : Generic implementation of a synthesis queue.
--  Author          : Riccardo Bernardini
--  Created On      : Thu Feb 14 14:42:33 2008
--  Last Modified By: .
--  Last Modified On: .
--  Update Count    : 0
--  Status          : Unknown, Use with caution!

--
-- A SYNTHESIS QUEUE is a protected object which accepts COMPONENTs
-- and returns MULTIMEDIA_PACKETS.  Since most of the work (receiving
-- the components, storing them in temporary memory, recognize when
-- enough components are received and so on...) is independent on the
-- actual multimedia format, it is more convenient to implement
-- a <tt>generic</tt> sinthesis queue to be instantiated with the
-- procedures which depend on the actual multimedia format.
--
-- Currently, the only format dependent information which is
-- needed is a "synthesis procedure" which accepts an array
-- of *components* and returns a *multimedia* packet.
--
with Generic_Packet_Map;
with Generic_Queue;
with Ignore_Packet_List;

generic
   -- Type associated with the crumb packets
   type Crumb_Type    is private;
   type Crumb_Array   is
     array (Positive range <>) of Crumb_Type;

   -- Timestamp associated to crumbs and complete packets
   type Timestamp is (<>);

   -- Special "void" crumb packet
   No_Crumb : Crumb_Type;

   -- Type associated with the "complete" packet
   type Complete_Type is private;
package Generic_Synthesis_Queue is
   --
   -- Access type for the procedure used to reconstruct a
   -- complete packet from (possibily a subset of) its components.
   -- This procedure can be called even if Input contains only a subset
   -- of the components.  In such a case the procedure can try
   -- to reconstruct the multimedia packet at its best or
   -- ignore the request.  If the reconstruction is
   -- successful, Success is set to True.
   --
   type Synthesis_Function is
     access procedure (Input   : in     Crumb_Array;
                       Output  :    out Multimedia_Packet;
                       Success :    out Boolean);

   subtype Crumb_Index is Positive;

   type Crumb_Index_Array is
     array (Positive range <>) of Crumb_Index;

   package Packet_Map is
      new Generic_Packet_Map (Packet_Type   => Crumb_Type,
                              Packet_Vector => Crumb_Array,
                              Index_Type    => Timestamp,
                              Portion_Type  => Crumb_Index,
                              Portion_Array => Crumb_Index_Array);

   package Internal_Queues is
      new Generic_Queues (Element => Complete_Type);

   package Timestamp_Lists is
      new Integer_Lists(Timestamp);

   protected type Synthesis_Queue is
      --
      -- Initialize the Queue. N_Components is the (maximum?) number
      -- of components per Multimedia_Packet.
      --
      procedure Initialize (N_Components : Positive;
                            Synthetizer  : Synthesis_Function);

      --
      -- Receive a new "full" component.  If possible, reconstruct
      -- a new multimedia packet.
      --
      procedure Receive (Packet : in Crumb_Type;
                         Time   : in Timestamp;
                         Index  : in Crumb_Index);

      --
      -- Extract the next multimedia buffer from the queue of ready
      -- packets.  Entry (obviously) conditioned by the presence of
      -- at least one ready multimedia packet.
      --
      entry     Extract (Packet :    out Multimedia_Packet);

      --
      -- Called when the packet with timestamp Time has been declared
      -- "lost" and it is not needed anymore.
      --
      procedure Lost    (Time : in Timestamp);

      procedure Lost_Component (Time  : in Timestamp;
                                Index : in Crumb_Index);

      --
      -- Return an array with the indexes of the components which
      -- are necessary to reconstruct the Multimedia_Packet
      -- with timestamp Time.  If the packet was already recovered,
      -- return an empty array.
      --
      function  Missing_Components (Time : Timestamp)
                                   return Crumb_Index_Array;

      --
      -- Try to reconstruct now (maybe approximately) the
      -- Multimedia_Packet with timestamp Time, even if some components
      -- are missing.
      --
      procedure Reconstruct_Now (Time : Network_Timestamp);
   private
      Needed         : Positive;
      Rebuild_Packet : Synthesis_Function;

      Table          : Packet_Map.Packet_Table;
      Ready_Queue    : Internal_Queues.Queue;
      Ignored_List   : Timestamp_Lists.Integer_List;
   end Synthesis_Queue;
private

end Generic_Synthesis_Queue;
