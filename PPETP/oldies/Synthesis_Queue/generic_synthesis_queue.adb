use Network_Packet_Map;
use Timestamp_Lists;

package body Generic_Synthesis_Queue is
   protected body Synthesis_Queue is
      procedure Initialize (N_Components : Positive;
                            Synthetizer  : Synthesis_Function) is
      begin
         Init(Buffer, N_Components);
         Needed := N_Components;
         Rebuild_Packet := Synthetizer;
      end Initialize;

      --
      -- Receive a new "full" component.  If possible, reconstruct
      -- a new multimedia packet.
      --
      procedure Receive (Packet : in Crumb_Type;
                         Time   : in Timestamp;
                         Index  : in Crumb_Index) is
         How_Many : Positive;
      begin
         if (Contains(Ignored_List, Timestamp)) then
            return;
         end if;

         Insert(Table    => Buffer,
                Index    => Timestamp,
                Portion  => Crumb_Index,
                Inserted => How_Many);

         if (How_Many = Needed) then
            Reconstruct_Now(Timestamp);
         end if;
      end Receive;

      --
      -- Extract the next multimedia buffer from the queue of ready
      -- packets.  Entry (obviously) conditioned by the presence of a
      -- ready multimedia packet.
      --
      entry Extract (Packet : out Complete_Type) when
        not Is_Empty(Ready_Queue) is
         Result : Complete_Type;
      begin
         Extract(Ready_Queue, Result);


         Packet := Result;
      end Extract;

      --
      -- Called when the packet with timestamp Time has been declared
      -- "lost" and it is not needed anymore.
      --
      procedure Lost    (Time   : in     Timestamp) is
      begin
         if (Contains(Table, Time)) then
            Remove (Table, Time);
         end if;

         Insert (Ignored_List, Time);
      end Lost;

      procedure Lost_Component (Time     : in Timestamp;
                                Comp_Idx : in Crumb_Index) is
      begin
         null;
      end Lost_Component;

      --
      -- Return an array with the indexes of the components which
      -- are necessary to reconstruct the Complete_Type
      -- with timestamp Time.  If the packet was already recovered,
      -- return an empty array.
      --
      function  Missing_Components (Time : Timestamp)
                                   return Crumb_Index_Array is
      begin
         return Missing(Table, Time);
      end Missing_Components;

      --
      -- Try to reconstruct now (maybe approximately) the
      -- Complete_Type with timestamp Time, even if some components
      -- are missing.
      --
      procedure Reconstruct_Now (Time : Timestamp) is
         Ok : Boolean;
         Result : Complete_Type;
      begin
         if (Contains(Table, Time)) then
            Synthesize (Input   => Get(Table, Time),
                        Output  => Result,
                        Success => Ok);

            if (Ok) then
               Insert(Ready_Queue, Result);
            end if;
         end if;

         Remove(Table, Time);
         Insert(Ignored_List, Time);
      end Reconstruct_Now;
   end Synthesis_Queue;
end Generic_Synthesis_Queue;
