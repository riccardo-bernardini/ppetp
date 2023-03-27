package body Generic_Processor is
   procedure Initialize (Processor       : in out Processor_Type;
                         Crumb_Dst       : in     Crumbs_Queue;
                         Full_Dst        : in     Components_Queue;
                         Encoder_Obj     : in     Encoder_Function;
                         Decoder_Obj     : in     Decoder_Function;
                         Reconstruct_Obj : in     Reconstructor_Type;
                         Reducer_Obj     : in     Reducer_Type) is
   begin
      Crumbs_Destination := Crumb_Dst;
      Full_Destination   := Full_Dst;

      Encoder := Encoder_Obj;
      Decoder := Decoder_Obj;

      Reconstructor := Reconstruct_Obj;
      Reducer       := Reducer_Obj;
   end Initialize;

   -- =========== --
   --  Propagate  --
   -- =========== --

   -- Send to the outer world a just recovered complete component.
   procedure Propagate (Processor          : in out Processor_Type;
                        Complete_Component : Complete_Type) is
      Reduced_Component  : Reduced_Type;
      Crumb              : Network_Crumb;
   begin
      Processor.Full_Destination.Receive (Complete_Component);

      Reduce(Reducer => Processor.Reducer,
             Full    => Complete_Component,
             Reduced => Reduced_Component);

      Encode(Encoder => Processor.Encoder,
             Data    => Reduced_Component,
             Encoded => Crumb);

      Processor.Crumbs_Destination.Receive (Crumb);
   end Do_Recovering;

   procedure Receive (Processor : in out Processor_Type;
                      Input     : in     Network_Crumb) is
      Reduced_Component  : Reduced_Type;
      Complete : Boolean;
   begin
      Decode(Decoder => Processor.Decoder,
             Crumb   => Input,
             Decoded => Reduced_Component);

      Receive(Oxidizer => Processor.Reconstructor,
              Packet   => Reduced_Component,
              Complete => Complete);

      if (Complete) then
         declare
            Complete_Component : Complete_Type;
         begin
            Recover (Oxidizer         => Processor.Reconstructor,
                     Timestamp        => Timestamp,
                     Recovered_Packet => Complete_Component,
                     Success          => Ok);

            Propagate(Processor, Complete_Component);
         end;
      end if;
   end Receive;

   procedure Packet_Needed (Processor : in out Processor_Type;
                            Timestamp : in     Multimedia_Timestamp) is
      Missing : Component_Index_Array :=
        Processor.Full_Destination.Missing_Components(Timestamp);

      Component_Idx : Component_Timestamp;
   begin
      for I in Missing'Range loop
         Component_Idx := Comp_Time(Timestamp, Missing(I)),
         Do_Recovering(Processor => Processor,
                       Timestamp => Component_Idx;
                       Success   => Ok);

         if (not Ok) then
            Lost(Processor.Oxidizer, Component_Idx);
            Processor.Full_Destination.Lost_Component(Timestamp, I);
            Processor.Crumbs_Destination.Lost_Component(Timestamp, I);
         end if;
      end loop;

      Processor.Full_Destination.Reconstruct_Now(Timestamp);
      Processor.Crumbs_Destination.Reconstruct_Now(Timestamp);
   end Packet_Needed;

   procedure Packet_Lost   (Processor : in out Processor_Type;
                            Timestamp : in     Timestamp_Type) is
   begin
      Lost(Processor.Oxidizer, Timestamp);

      Processor.Full_Destination.Lost   (Timestamp);
      Processor.Crumbs_Destination.Lost (Timestamp);
   end Packet_Lost;
end Generic_Processor;
