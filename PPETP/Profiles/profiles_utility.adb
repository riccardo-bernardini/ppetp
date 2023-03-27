
package body Profiles_Utility is

   function "=" (Left, Right : SeqNum_StreamID_Key) return boolean is
   begin
      return (Left.SeqNum = Right.SeqNum) and (Left.StreamID = Right.StreamID);
   end "=";


   function "<" (Left, Right : SeqNum_StreamID_Key) return boolean is
   begin
      if Left.SeqNum = Right.SeqNum then
         return Left.StreamID < Right.StreamID;
      else
         return Left.SeqNum < Right.SeqNum;
      end if;
   end "<";


end Profiles_Utility;
