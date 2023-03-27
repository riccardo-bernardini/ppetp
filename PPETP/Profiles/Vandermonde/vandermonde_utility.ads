with PPETP;				use PPETP;
with Profiles_Utility;			use Profiles_Utility;
with Profiles.Entangled.Vandermonde;	use Profiles.Entangled.Vandermonde;

with Profiles.Entangled;	use Profiles.Entangled;

package Vandermonde_Utility is

   type Entangled_Record is
      record
         Occupation   : Integer := 0;
         Reduction_Factor : Natural :=0;
         Vectors      : Entangled_Array := (1 .. 128 => null);
         Sequence_Num : PPETP.Data_Sequence_Number := 0;
         StreamID     : PPETP.Stream_ID := 0;
      end record;

   type Peer_Parameters_Key is
      record
         Id: Peer_ID;
         Ch: Channel_ID;
      end record;

   function "=" (Left, Right : Peer_Parameters_Key) return boolean;
   function "<" (Left, Right : Peer_Parameters_Key) return boolean;


   type IntSeqNum_StreamID_Key is
      record
         IntSeqNum : Integer_Sequence_Number;
         StreamID  : PPETP.Stream_ID;
      end record;

   No_IntSeqNum_StreamID_Key : constant IntSeqNum_StreamID_Key;

   function "=" (Left, Right : IntSeqNum_StreamID_Key) return boolean;
   function "<" (Left, Right : IntSeqNum_StreamID_Key) return boolean;


   -- This functiona inserts a New_Element into the Entangled_Record
   -- of object Disentangler
   procedure Insert_New_Element(Subject      : in out Entangled_Record;
                                New_Element  : in     Entangled_Payload_Pt;
                                Sequence_Num : in     PPETP.Data_Sequence_Number;
                                StreamID     : in     PPETP.Stream_ID);

private
   No_IntSeqNum_StreamID_Key : constant IntSeqNum_StreamID_Key := (IntSeqNum  => -1,
                                                                StreamID => 0);
end Vandermonde_Utility;
