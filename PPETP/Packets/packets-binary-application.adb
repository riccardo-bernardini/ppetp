with  PPETP;
use   PPETP;
with  Ada.Unchecked_Deallocation;

package body Packets.binary.Application is

   --------------------
   -- New_Bin_Packet --
   --------------------

   function New_Packet (Sequence_Num : PPETP.Data_Sequence_Number;
                        StreamID     : PPETP.Stream_ID;
                        Data         : Byte_Array)
                        return Application_Packet
   is
   begin
      return Result : Application_Packet do
         Result.Set(Data);
         Result.StreamID := StreamID;
         Result.Sequence_Num := Sequence_Num;
      end return;
   end New_Packet;

   ------------------
   -- Sequence_Num --
   ------------------

   function Sequence_Num (Packet : Application_Packet)
      return PPETP.Data_Sequence_Number
   is
   begin
      return Packet.Sequence_Num;
   end Sequence_Num;

   --------------
   -- StreamID --
   --------------
   function StreamID (Packet : Application_Packet)
                      return PPETP.Stream_ID is
   begin
      return Packet.StreamID;
   end StreamID;

   --------------
   -- Bin_Data --
   --------------

   function "="
     (Left, Right : Application_Packet)
      return Boolean
   is
   begin
      return (Left.Sequence_Num = Right.Sequence_Num) and then
             (Left.StreamID     = Right.StreamID ) and then
        (Left.Data_Buffer.all = Right.Data_Buffer.all);
   end "=";

end Packets.Binary.Application;
