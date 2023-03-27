with Profiles;              use Profiles;
with PPETP;                 use PPETP;
with Byte_Arrays;           use Byte_Arrays;


package Packets.Protocol.Data is
   -- Internal correspondent of a Data packet --
   ---------------------------------------------
   type Source_ID is mod 2**32;

   type Raw_Data is
      record
         Flags  : Profile_Flags;
         Inline : Boolean;
         Data   : Byte_Array_Pt;
         --#SRC#  SRC_ID : Source_ID;
      end record;

   type Data_Packet is
     new Protocol_Packet with
      record
         Sequence_Num : PPETP.Data_Sequence_Number;
         StreamID: PPETP.Stream_ID;
         Channel : PPETP_Channel_ID;
         Payload : Raw_Data;
      end record;



   overriding
   procedure Print (A : Data_Packet);
   procedure Free(Object: in out Data_Packet);

end Packets.Protocol.Data;
