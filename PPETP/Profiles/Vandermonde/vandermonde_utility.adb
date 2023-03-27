with PPETP;		use PPETP;

package body Vandermonde_Utility is

   function "=" (Left, Right : Peer_Parameters_Key) return boolean is
   begin
      if (Left.Id = Right.Id) and (Left.Ch = Right.Ch) then
         return True;
      else
         return False;
      end if;

   end "=";

   function "<" (Left, Right : Peer_Parameters_Key) return boolean is
   begin
      if (Left.Id < Right.Id) then
         return True;
      elsif (Left.Id = Right.Id) then
         return Left.Ch < Right.Ch;
      else
         return False;
      end if;
   end "<";


   function "=" (Left, Right : IntSeqNum_StreamID_Key) return boolean is
   begin
      return (Left.IntSeqNum = Right.IntSeqNum) and (Left.StreamID = Right.StreamID);
   end "=";

   function "<" (Left, Right : IntSeqNum_StreamID_Key) return boolean is
   begin
      if Left.IntSeqNum = Right.IntSeqNum then
         return Left.StreamID < Right.StreamID;
      else
         return Left.IntSeqNum < Right.IntSeqNum;
      end if;
   end "<";



   procedure Insert_New_Element(Subject      : in out Entangled_Record;
                                New_Element  : in     Entangled_Payload_Pt;
                                Sequence_Num : in     PPETP.Data_Sequence_Number;
                                StreamID     : in     PPETP.Stream_ID) is
   begin

      if Subject.Occupation = 0 then
         Subject.Reduction_Factor :=
         Vandermonde_Ent(New_Element.all).Param.Reduction_Factor;
      end if;

      Subject.Sequence_Num := Sequence_Num;
      Subject.StreamID     := StreamID;
      Subject.Vectors (Subject.Occupation + 1) := New_Element;
      Subject.Occupation := Subject.Occupation + 1;
   end Insert_New_Element;

end Vandermonde_Utility;
