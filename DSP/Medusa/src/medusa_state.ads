with Input_Data;
with Generic_Multimedia_Synthesis;
with Network_Synthesis;

package Medusa_State is
   package Multimedia_Synthesis is
      new Generic_Multimedia_Synthesis(Synthesize => Trivial_Mmedia_Syn);

   Input_Queue : aliased Input_Data.Command_Queue_Type;
   To_Player   : aliased Queue;
   To_P2P      : aliased Queue;
   To_Root     : aliased Queue;

   Data_To_Peers  : aliased Network_Synthesis.Synthesis_Queue;
   Data_To_Player : aliased Multimedia_Synthesis.Synthesis_Queue;
end Medusa_State;
