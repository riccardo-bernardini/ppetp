with PPETP;			use PPETP;
with Packets.Protocol.Data;	use Packets.Protocol.Data;
with Profiles.Entangled;	use Profiles.Entangled;

package Profiles_Utility is

   -- forse non servira' piu'
   type Integer_Sequence_Number is new Long_Integer range -1 .. Long_Integer'Last;


   type Entangled_Array is array(1 .. 128) of Entangled_Payload_Pt;


   Empty_Queue: exception;

   type Queue_Element (Entangled : Boolean := True) is
      record
         Sequence_Num : PPETP.Data_Sequence_Number;
         StreamID     : PPETP.Stream_ID;
         Data         : Raw_Data;
      end record;

   type SeqNum_StreamID_Key is
      record
         SeqNum   : PPETP.Data_Sequence_Number;
         StreamID : PPETP.Stream_ID;
      end record;

   function "=" (Left, Right : SeqNum_StreamID_Key) return boolean;
   function "<" (Left, Right : SeqNum_StreamID_Key) return boolean;

end Profiles_Utility;
