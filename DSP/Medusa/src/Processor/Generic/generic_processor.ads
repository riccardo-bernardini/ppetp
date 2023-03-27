generic
   -- Type of the reduced component
   type Reduced_Type  is private;

   -- Type of the full component
   type Complete_Type is private;


   --==========================--
   -- Reducer type and methods --
   --==========================--

   type Reducer_Type is private;

   with procedure Reduce (Reducer : in out Reducer_Type;
                          Full    : in     Complete_Type;
                          Reduced :    out Reduced_Type) is <>;

   --================================--
   -- Reconstructor type and methods --
   --================================--

   type Reconstructor_Type is private;

   -- This methods hands a new reduced packet to the reconstructor.
   -- If a sufficient number of packets have been received, the
   -- reconstructor sets Complete to True.
   with procedure Receive (Oxidizer  : in out Reconstructor;
                           Packet    : in     Reduced_Type;
                           Complete  :    out Boolean)
   is <>;

   -- Ask the reconstructor to recover the Component with timestamp
   -- Timestamp.  Note that it is not an error to call this function
   -- in the case Receive returned Complete=False (that is, not enough
   -- packets were received).  In that case the Reconstructor could
   -- try to recover the component in an approximate way.  Flag Success
   -- is set to true if the reconstruction was possible.  After
   -- a successful recovering the reduced packet with timestamp
   -- Timestamp are removed.
   with procedure Recover (Oxidizer         : in out Reconstructor;
                           Timestamp        : in     Timestamp_Type;
                           Recovered_Packet :    out Complete_Type;
                           Success          :    out Boolean)
   is <>;

   -- Say to the Reconstructor that packet at time Timestamp was
   -- declared Lost.  The Oxidizer could decide to return in Reduced
   -- a received reduced component.  In this case it will set Success
   -- to True. After this call the Oxidizer is free to delete all
   -- the corresponding  reduced packets already received.
   with procedure Lost (Oxidizer  : in out Reconstructor;
                        Timestamp : in     Timestamp_Type);
   --                        Reduced   :    out Reduced_Type;
   --                        Success   :    out Boolean)
   is <>;

   --================================--
   -- Encode/Decode type and methods --
   --================================--

   type Encoder_Type  is private;
   type Decoder_Type  is private;

   with procedure Encode (Encoder : in out Encoder_Type;
                          Data    : in     Reduced_Type;
                          Crumb   :    out Network_Crumb) is <>;


   with procedure Decode (Decoder : in out Decoder_Type;
                          Crumb   : in     Network_Crumb;
                          Data    :    out Reduced_Type) is <>;

package Generic_Processor is
   type Processor_Type is private;

   -- type Encoder_Function is
   --   access procedure (Data    : in     Reduced_Type;
   --                     Crumb   :    out Network_Crumb);
   --
   -- type Decoder_Function is
   --   access procedure (Crumb   : in     Network_Crumb;
   --                     Decoded :    out Reduced_Type);

   -- Initialize the processor by handing to it all the
   -- object it needs to do its job.
   procedure Initialize (Processor       : in out Processor_Type;
                         Crumb_Dst       : in     Crumbs_Queue;
                         Full_Dst        : in     Components_Queue;
                         Encoder_Obj     : in     Encoder_Function;
                         Decoder_Obj     : in     Decoder_Function;
                         Reconstruct_Obj : in     Reconstructor_Type;
                         Reducer_Obj     : in     Reducer_Type);

   -- Receive and process a new network crumb
   procedure Receive (Processor : in out Processor_Type;
                      Input     : in     Network_Crumb);

   -- Used to force the processor to recover all the components
   -- with a given timestamp
   procedure Packet_Needed (Processor : in out Processor_Type;
                            Timestamp : in     Timestamp_Type);

   procedure Packet_Lost   (Processor : in out Processor_Type;
                            Timestamp : in     Timestamp_Type);

private
   type Processor_Type is record
      Crumbs_Destination : Crumbs_Queue;
      Full_Destination   : Components_Queue;
      Encoder            : Encoder_Type;
      Decoder            : Decoder_Type;
      Reconstructor      : Reconstructor_Type;
      Reducer            : Reducer_Type;
   end record;
end Generic_Processor;
